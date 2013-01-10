module Main where

import System.Posix.CircularBuffer

main :: IO ()
main = do
    wb <- createBuffer "/abuf" "/abuf" 4 384 :: IO (WriteBuffer Int)
    mapM_ (putBuffer wb) [1..16]
    removeBuffer wb
