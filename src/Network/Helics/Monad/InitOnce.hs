{-# OPTIONS_GHC -fno-cse #-}
module Network.Helics.Monad.InitOnce where

import           Control.Monad  (void)
import           Network.Helics (sampler)

initOnce :: IO ()
{-# NOINLINE initOnce #-}
initOnce = void $ sampler 60
