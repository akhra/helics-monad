{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.Helics.Monad
( module Types
, MonadHelics, Helics
, withHelics, recordMetric, withTransaction
, MonadHelicsTxn, HelicsTxn
, setAttribute, setError, genericSegment, datastoreSegment, externalSegment
, MonadHelicsSeg, HelicsSeg
) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.State.Ref
import           Control.Monad.Trans.Unlift
import           Data.Bool
import qualified Data.ByteString               as S
import           Data.Foldable
import           Data.IORef
import qualified Data.Map.Strict               as M
import qualified Network.Helics                as H
import qualified Network.Helics.Internal.Types as H
import           Network.Helics.Monad.InitOnce
import           Network.Helics.Monad.Types    as Types


type MonadHelics'   m = (MonadBaseUnlift IO m, MonadIO m, MonadReader HelicsState m)
type MonadHelics    m = (MonadHelics' m, MonadHelicsBase m, MonadHelicsMetrics m)
type MonadHelicsTxn m = (MonadHelics m, MonadState HelicsTxnState m)
type MonadHelicsSeg m = (MonadHelics' m, MonadHelicsMetrics m, MonadReader HelicsSegState m)

type family IsHelicsSegment (a :: k) :: Bool where
  IsHelicsSegment (MonadHelicsSeg m) = 'True
  IsHelicsSegment _ = 'False


newtype HelicsState = H' { isHelics :: Bool }
type Helics = ReaderT HelicsState

data HelicsTxnState = T'
  { nextSeg :: H.SegmentId
  , txnId   :: H.TransactionId
  }
type HelicsTxn = StateRefT IORef HelicsTxnState

newtype HelicsSegState = S' ()
type HelicsSeg = ReaderT HelicsSegState

class MonadHelicsBase m where
  withTransaction :: Transaction -> HelicsTxn m a -> m a
instance (MonadHelics m, IsHelicsSegment m ~ 'False) => MonadHelicsBase m where
  withTransaction = withTransaction'

withTransaction' :: (MonadHelics m) => Transaction -> HelicsTxn m a -> m a
withTransaction' Transaction{..} txn = fst <$> dispatch
  where
    dispatch = bool wrapped bare =<< asks isHelics
    bare = runStateRefT txn $ t' H.DummyTransactionId
    wrapped = do
      run <- askRunBase
      liftIO . H.withTransaction name txnType $ go run
    go run tid = liftIO . run $ runStateRefT (liftIO (prep tid) >> txn) $ t' tid
    txnType = bool H.Web H.Other isWebTransaction $ category
    t' = T' H.rootSegment
    prep tid = do
      perhaps H.setMaxTraceSegments maxTraceSegments
      perhaps H.setRequestUrl requestUrl
      perhaps (uncurry H.addAttribute) (M.toList attributes)
      where
        perhaps :: (Traversable t, Monad m) => (a -> H.TransactionId -> m ()) -> t a -> m ()
        perhaps f = traverse_ (`f` tid)

class MonadHelicsMetrics m where
  recordMetric :: MetricName -> MetricValue -> m ()
instance (MonadHelics m) => MonadHelicsMetrics m where
  recordMetric = recordMetric'

recordMetric' :: MonadHelics m => MetricName -> MetricValue -> m ()
recordMetric' k = whenHelics . liftIO . H.recordMetric k

whenHelics :: MonadHelics m => m () -> m ()
whenHelics act = flip when act =<< asks isHelics

withHelics :: (MonadBaseUnlift IO m, MonadIO m) =>
  NewRelicLicenseKey -> NewRelicAppName -> Helics m a -> m a
withHelics key app helics
  | ok = do
    liftIO initOnce
    run <- askRunBase
    liftIO . H.withHelics config $ go run
  | otherwise = runReaderT helics (H' False)
  where
    go run = liftIO . run $ runReaderT helics (H' True)
    config = H.def { H.licenseKey = key, H.appName = app }
    ok     = notElem S.empty [key, app]


class MonadHelicsTxn' m where
  genericSegment   :: MonadHelicsTxn m => SegmentName -> HelicsSeg m a -> m a
  datastoreSegment :: MonadHelicsTxn m => H.DatastoreSegment -> HelicsSeg m a -> m a
  externalSegment  :: MonadHelicsTxn m => SegmentHost -> SegmentName -> HelicsSeg m a -> m a
instance MonadHelicsTxn' (HelicsTxn m) where
  genericSegment    = runSegmentWith . genericSegment'
  datastoreSegment  = runSegmentWith . datastoreSegment'
  externalSegment h = runSegmentWith . externalSegment' h

runSegmentWith :: MonadHelicsTxn m =>
  (H.TransactionId -> H.SegmentId -> IO a -> IO a) -> HelicsSeg m a -> m a
runSegmentWith wrapper seg = bool wrapped bare =<< asks isHelics
  where
    bare = runReaderT seg $ S' ()
    wrapped = do
      T' segId txnId <- autoScope
      run <- askRunBase
      liftIO . wrapper txnId segId $ go run
    go run = liftIO . run $ bare
    autoScope = do
      t' <- get
      put t' { nextSeg = H.autoScope }
      pure t'

genericSegment' :: SegmentName -> H.TransactionId -> H.SegmentId -> IO a -> IO a
genericSegment' name txnId segId seg = H.genericSegment segId name seg txnId

datastoreSegment' :: H.DatastoreSegment -> H.TransactionId -> H.SegmentId -> IO a -> IO a
datastoreSegment' dstore txnId segId seg = H.datastoreSegment segId dstore seg txnId

externalSegment' :: SegmentHost -> SegmentName -> H.TransactionId -> H.SegmentId -> IO a -> IO a
externalSegment' host name txnId segId seg = H.externalSegment segId host name seg txnId

setAttribute :: (MonadHelicsTxn m) => AttributeName -> AttributeValue -> m ()
setAttribute name value = whenHelics go
  where go = (liftIO . H.addAttribute name value) =<< gets txnId

setError :: (MonadHelicsTxn m) => Maybe H.TransactionError -> m ()
setError err = whenHelics go
  where go = (liftIO . H.setError err) =<< gets txnId
