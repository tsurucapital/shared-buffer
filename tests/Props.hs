module Props where

import System.Posix.CircularBuffer
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Foreign.Storable

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Test.QuickCheck.Monadic as QC

tests :: [Test]
tests =
  [ testProperty "id" $ propAtInt prop_identity
  , testProperty "wrap" $ propAtInt prop_wrap
  , testProperty "wait" $ propAtInt prop_wait
  , testProperty "putBufferList" $ propAtInt1 prop_bulk_write
  , testProperty "getAvailable" $ propAtInt prop_bulk_read
  ]

withReaderWriter :: Storable a => Int -> (ReadBuffer a -> WriteBuffer a -> IO b) -> IO b
withReaderWriter sz fn = do
    wb <- createBuffer "/abuf" "/abuf" sz 0o600
    rb <- openBuffer   "/abuf" "/abuf" sz 0o400
    unlinkBuffer wb
    fn rb wb `finally` ( closeBuffer rb >> removeBuffer wb )

propAtInt :: ([Int] -> IO Bool) -> [Int] -> Property
propAtInt action xs = QC.monadicIO $ do
    result <- QC.run $ action xs
    if result then return () else fail "property failed"

propAtInt1 :: (a -> [Int] -> IO Bool) -> a -> [Int] -> Property
propAtInt1 action arg1 xs = QC.monadicIO $ do
    result <- QC.run $ action arg1 xs
    if result then return () else fail "property failed"


prop_identity :: (Eq a, Show a, Storable a) => [a] -> IO Bool
prop_identity xs = do
    let sz = length xs
    if sz == 0 then return True else withReaderWriter sz $ \rb wb -> do
        mapM_ (putBuffer wb) xs
        result <- replicateM sz (getBuffer rb)
        return $ result == xs

-- test wrapping around the end of the buffer
prop_wrap :: (Eq a, Show a, Storable a) => [a] -> IO Bool
prop_wrap xs = do
    let sz = 3
    withReaderWriter sz $ \rb wb -> do
        let loop [] = return True
            loop xss = do
                let (this,next) = splitAt sz xss
                mapM_ (putBuffer wb) this
                result <- replicateM (length this) (getBuffer rb)
                if result == this
                    then loop next
                    else fail $"wrapping failed: got " ++ show result
        loop xs

-- test waiting on a full buffer
prop_wait :: (Eq a, Show a, Storable a) => [a] -> IO Bool
prop_wait xs = do
    let sz = len `div` 4
        len = length xs
    -- putStrLn $ "input: " ++ show xs
    -- putStrLn $ "bufsz: " ++ show sz
    if len <= 4 then return True else withReaderWriter sz $ \rb wb -> do
        void . forkIO $ mapM_ (putBuffer wb) xs
        result <- replicateM len (getBuffer rb)
        if result == xs
            then return True
            else putStrLn ("*******got: " ++ show result) >> return False

prop_bulk_write :: (Eq a, Show a, Storable a) => Int -> [a] -> IO Bool
prop_bulk_write n' xs = do
    let sz = length xs
        n  = max 1 n'
    if sz == 0 then return True else withReaderWriter sz $ \rb wb -> do
        let (s0,s1) = splitAt n xs
        putBufferList wb s0
        putBufferList wb s1
        result <- replicateM sz (getBuffer rb)
        return $ result == xs

prop_bulk_read :: (Eq a, Show a, Storable a) => [a] -> IO Bool
prop_bulk_read xs = do
    let sz = length xs
    if sz == 0 then return True else withReaderWriter sz $ \rb wb -> do
        mapM_ (putBuffer wb) xs
        s0 <- getAvailable rb
        s1 <- replicateM (sz-length s0) (getBuffer rb)
        let result = s0++s1
        return $ result == xs
