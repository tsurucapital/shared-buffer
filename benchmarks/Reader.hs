{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Concurrent
import Control.Exception as Ex
import Control.Monad
import Data.IORef
import System.Posix.CircularBuffer
import System.Environment

-- read Int's from the supplied buffer as fast as possible, and print the
-- number read after 10 seconds
main :: IO ()
main = do
    args <- getArgs
    counter <- newIORef (0::Int)
    case args of
      [nm,szStr] -> do
          let sz = read szStr
          Ex.bracket ((openBuffer nm nm sz 0o600 :: IO (ReadBuffer Int))) (removeBuffer) $ \rb -> do
              tid <- forkIO $ forever $ do
                  xs <- getAvailable rb
                  if (null xs) then yield else writeIORef counter $ last xs
                  -- x <- getBuffer WaitYield rb
                  -- writeIORef counter x
              threadDelay $ 10*1000000
              killThread tid
              print =<< readIORef counter
      _ -> error "usage: reader NAME SZ"
