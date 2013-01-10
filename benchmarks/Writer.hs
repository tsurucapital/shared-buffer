{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Exception as Ex
import Control.Monad
import System.Posix
import System.Posix.CircularBuffer
import System.Environment
import Network.Socket

import Debug.Trace

-- write Int's to the supplied buffer as fast as possible.
main :: IO ()
main = withSocketsDo $ do
    void $ installHandler sigPIPE Ignore Nothing
    args <- getArgs
    case args of
      [nm,szStr] -> do
          let sz = read szStr
          when (sz <= 0 ) (error $ "invalid size: " ++ show sz)
          let closer buf = do
                  closeBuffer buf

          bracket ((createBuffer nm nm sz 0o600 :: IO (WriteBuffer Int)) ) closer $ \wb -> do
            let loop n = do
                    res <- Ex.try $ putBufferList wb [n..n+15]
                    -- res <- Ex.try $ putBufferList wb [n]

                    case res of
                        Right () -> loop $! n+16
                        Left e@Ex.SomeException{} -> traceIO (show e) >> Ex.throwIO e
            loop 0
      _ -> error "usage: writer NAME SZ"
