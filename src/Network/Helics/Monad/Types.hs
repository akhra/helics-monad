{-# LANGUAGE TemplateHaskell #-}
module Network.Helics.Monad.Types
( module Network.Helics.Monad.Types
, DatastoreSegment(..), Operation(..), TransactionError(..)
) where

import           Control.Lens.TH               (makeFields)
import           Data.ByteString               (ByteString)
import           Data.Map.Strict               as M (Map, empty)
import           Network.Helics.Internal.Types (DatastoreSegment (..),
                                                Operation (..),
                                                TransactionError (..))

type NewRelicLicenseKey = ByteString
type NewRelicAppName    = ByteString

type MetricName  = ByteString
type MetricValue = Double

type AttributeName  = ByteString
type AttributeValue = ByteString

type TransactionName     = ByteString
type TransactionCategory = ByteString

type SegmentName = ByteString
type SegmentHost = ByteString

data Transaction = Transaction
  { requestUrl       :: Maybe ByteString
  , maxTraceSegments :: Maybe Int
  , attributes       :: Map AttributeName AttributeValue
  , isWebTransaction :: Bool
  , name             :: ByteString
  , category         :: ByteString
  }
makeFields ''Transaction

webTransaction, nonwebTransaction
  :: TransactionName -> TransactionCategory -> Transaction
[webTransaction, nonwebTransaction]
  = Transaction Nothing Nothing M.empty <$> [True, False]
