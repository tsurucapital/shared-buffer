{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
{- | Create a circular buffer over shared memory that can be accessed from
   separate processes.

   This module assumes that exactly one WriteBuffer and one ReadBuffer will be
   used to access the same shared memory object.  The ReadBuffer and
   WriteBuffer may exist in separate processes.

   to use this library, in one process

   > bracket (createBuffer "aBuffer" "aSemaphore" 256 0o600) (removeBuffer)
   >         (\buffer -> doSomethingWith buffer)

   and in the other

   > bracket (openBuffer "aBuffer" "aSemaphore" 256 0o600) (closeBuffer)
   >         (\buffer -> doSomethingWith buffer)

   The buffer may be opened from either the reader or writer end, but you
   should ensure that the buffer is created before it is opened.

   As the underlying objects (shm and named posix semaphores) exist in the file
   system, failing to call removeBuffer will leave stale objects in the
   filesystem.

   Opening multiple ReadBuffers or WriteBuffers with the same names (whether in
   one process or several) results in undefined behavior.
 -}
module System.Posix.CircularBuffer (
  WriteBuffer
, ReadBuffer
, Shared (..)
, WaitStrategy (..)
-- * normal interface
, putBuffer
, getBuffer
, tryGetBuffer
-- ** batch operations
, putBufferList
, getAvailable
, sizeOfInt
) where

import System.Posix.SharedBuffer

import Control.Concurrent.MVar
import Control.Exception (try, finally, allowInterrupt)
import Control.Monad
import Data.Bits
import Data.Maybe (isJust)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable
import System.Posix.Semaphore.Unsafe
import System.Posix (FileMode)

import Debug.Trace (traceEventIO)
import Control.Concurrent (threadDelay,yield)

-- we could use Ptr's instead of ForeignPtr's, but then we'd need to free them
-- in the case of an exception, which would require exporting more from this
-- module.
data WriteBuffer a = WB CircularBuffer (MVar Int)
data ReadBuffer a  = RB CircularBuffer (ForeignPtr Int)

-- | Functions for creating/opening/closing/removing shared buffers.
class Shared b where
    createBuffer :: String -> String -> Int -> FileMode -> IO b
    openBuffer   :: String -> String -> Int -> FileMode -> IO b
    closeBuffer  :: b -> IO ()
    removeBuffer :: b -> IO ()
    unlinkBuffer :: b -> IO ()

instance Storable a => Shared (WriteBuffer a) where
    createBuffer = openSharedBuffer makeWB
                                    OpenSemFlags{semCreate = True, semExclusive = True}
                                    ShmOpenFlags{shmCreate = True
                                                ,shmReadWrite = True
                                                ,shmExclusive = True
                                                ,shmTrunc = False
                                                }
                                    writeProtection
                                    (sizeOf (undefined :: a))
    openBuffer = openSharedBuffer makeWB
                                  OpenSemFlags{semCreate = False, semExclusive = False}
                                  ShmOpenFlags{shmCreate = False
                                              ,shmReadWrite = True
                                              ,shmExclusive = False
                                              ,shmTrunc = False
                                              }
                                  writeProtection
                                  (sizeOf (undefined :: a))
    closeBuffer (WB cb _)  = closeBuffer cb
    removeBuffer (WB cb _) = removeBuffer cb
    unlinkBuffer (WB cb _) = unlinkBuffer cb

instance Storable a => Shared (ReadBuffer a) where
    createBuffer = openSharedBuffer makeRB
                                    OpenSemFlags{semCreate = True, semExclusive = True}
                                    ShmOpenFlags{shmCreate = True
                                                ,shmReadWrite = False
                                                ,shmExclusive = True
                                                ,shmTrunc = False
                                                }
                                    [ProtRead]
                                    (sizeOf (undefined :: a))
    openBuffer   = openSharedBuffer makeRB
                                    OpenSemFlags{semCreate = False, semExclusive = False}
                                    ShmOpenFlags{shmCreate = False
                                                ,shmReadWrite = False
                                                ,shmExclusive = False
                                                ,shmTrunc = False
                                                }
                                    [ProtRead]
                                    (sizeOf (undefined :: a))
    closeBuffer (RB cb _)  = closeBuffer cb
    removeBuffer (RB cb _) = removeBuffer cb
    unlinkBuffer (RB cb _) = unlinkBuffer cb

makeRB :: CircularBuffer -> MVar Int -> ForeignPtr Int -> ReadBuffer a
makeRB buf _ fp = RB buf fp

makeWB :: CircularBuffer -> MVar Int -> ForeignPtr Int -> WriteBuffer a
makeWB buf mv _ = WB buf mv

-- | open an existing shared memory buffer and semaphore.
openSharedBuffer :: (CircularBuffer -> MVar Int -> ForeignPtr Int -> b)
                 -> OpenSemFlags
                 -> ShmOpenFlags
                 -> [Protection]
                 -> Int
                 -> String
                 -> String
                 -> Int
                 -> FileMode
                 -> IO b
openSharedBuffer maker semFlags shmFlags prot bitwidth shmName cbSemName reqCbSize mode = do
    let bufsz = fromIntegral $ bitwidth*cbSize
        cbSize = 2^(ceiling (logBase 2 (fromIntegral $ 1+reqCbSize) :: Double) :: Int)
        -- the buffer is effectively 1 element smaller than specified
        -- (due to a race condition in the reader, see 'readSeqBlocking')
        -- so make it 1 larger so that the full requested size is always
        -- available.
    cbBuf <- openSBuffer shmName bufsz shmFlags prot mode
    cbSem <- semOpen cbSemName semFlags mode 0
    seqref <- newMVar 0
    seqptr <- mallocForeignPtr
    withForeignPtr seqptr $ flip poke 0
    return $ maker (CircularBuffer{cbBuf,cbSize,cbSem,cbSemName}) seqref seqptr

-- | Write a value to the writer end.
--
-- This function is thread-safe.
putBuffer :: Storable a => WriteBuffer a -> a -> IO ()
putBuffer (WB cb seqvar) val = modifyMVar_ seqvar $ \seqnum -> do
    writeSeqBlocking cb seqnum val
    return $! seqnum+1
{-# INLINEABLE putBuffer #-}

-- | Write a list of values to the writer end.
--
-- This function is thread-safe.
putBufferList :: Storable a => WriteBuffer a -> [a] -> IO ()
putBufferList (WB cb seqvar) vals = modifyMVar_ seqvar $ \seqnum -> do
    cnt <- writeSeqList cb seqnum vals
    return $! seqnum+cnt
{-# INLINE putBufferList #-}

data WaitStrategy =
    KBlocking
  | Spin {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | SpinContinuous {-# UNPACK #-} !Int

-- | read the next value from the reader end.
--
-- This function is *NOT* thread-safe.
getBuffer :: Storable a => WaitStrategy -> ReadBuffer a -> IO a
getBuffer ws (RB cb seqvar) = withForeignPtr seqvar $ \seqPtr -> do
    seqnum <- peek seqPtr
    val <- readSeqBlocking ws cb seqnum
    poke seqPtr $ seqnum+1
    return val
{-# INLINEABLE getBuffer #-}

-- | Try to read the next value from the reader end.
--
-- This function is *NOT* thread-safe.
tryGetBuffer :: Storable a => ReadBuffer a -> IO (Maybe a)
tryGetBuffer (RB cb seqvar) = withForeignPtr seqvar $ \seqPtr -> do
    seqnum <- peek seqPtr
    val <- tryReadSeq cb seqnum
    when (isJust val) $ poke seqPtr $ seqnum+1
    return val
{-# INLINEABLE tryGetBuffer #-}

-- | read all currently available values from the reader end.
--
-- This function is *NOT* thread-safe.
getAvailable :: Storable a => ReadBuffer a -> IO [a]
getAvailable (RB cb seqvar) = withForeignPtr seqvar $ \seqPtr -> do
    seqnum <- peek seqPtr
    val <- readSeqReady cb seqnum
    poke seqPtr $ seqnum+length val
    return val
{-# INLINEABLE getAvailable #-}

------------------------------------------------------------------
-- circular buffer interface

-- intended use:
-- a single producer and single consumer in separate processes
--
-- invariants:
--   cbSem contains the number of items available to be read in buffer.  That
--   is, the number of items that are immediately available to the reader.
data CircularBuffer = CircularBuffer
    { cbSize :: {-# UNPACK #-} !Int
    , cbBuf  :: {-# UNPACK #-} !SharedBuffer
    , cbSem  :: {-# UNPACK #-} !Semaphore
    , cbSemName :: String
    }

instance Shared CircularBuffer where
    createBuffer = error "can't create a CircularBuffer directly"
    openBuffer = error "can't open a CircularBuffer directly"
    closeBuffer = closeSharedBuffer . cbBuf
    removeBuffer cb =
        removeSharedBuffer (cbBuf cb) `finally`
            void (try (semUnlink (cbSemName cb)) :: IO (Either IOError ()))
    unlinkBuffer cb =
        void (try $ unlinkSharedBuffer (cbBuf cb) `finally` semUnlink (cbSemName cb) :: IO (Either IOError ()))

-- Wait until data is available, then read the value at a particular sequence number.
--
-- logically, this should proceed as:
-- - first wait until a value is available (cbSem > 0)
-- - read the value
-- - lock the semaphore (decrement it).
--
-- but we actually wait and lock the semaphore in one operation, then read the
-- value.  The writer side must take care to not overwrite the end of the
-- buffer, as it will see the semaphore decrement before the value is actually
-- read.
readSeqBlocking :: Storable a => WaitStrategy -> CircularBuffer -> Int -> IO a
readSeqBlocking KBlocking cb = \rseq -> do
    waitAndLock (cbSem cb)
    readSeq cb rseq
readSeqBlocking (SpinContinuous n) cb = \rseq -> do
    waitSpin n (cbSem cb)
    readSeq cb rseq
readSeqBlocking (Spin spinStay spinStart spinStop) cb = \rseq -> do
    waitBackoff spinStart spinStay spinStop (cbSem cb)
    readSeq cb rseq
{-# INLINEABLE readSeqBlocking #-}

tryReadSeq :: Storable a => CircularBuffer -> Int -> IO (Maybe a)
tryReadSeq cb rseq = do
    gotLock <- unsafeSemTryWait $ cbSem cb
    if gotLock then Just `fmap` readSeq cb rseq else return Nothing
{-# INLINEABLE tryReadSeq #-}

-- read currently available data starting from the given sequence number.
readSeqReady :: Storable a => CircularBuffer -> Int -> IO [a]
readSeqReady cb@CircularBuffer{..} rseq = do
    curReady <- unsafeSemGetValue cbSem
    vals <- readSeqs cb rseq curReady
    replicateM_ curReady (unsafeSemLock cbSem)
    -- unsafeSemLock is ok if there are no other readers on this
    -- semaphore.
    return vals
{-# INLINEABLE readSeqReady #-}

-- Write data to the buffer at the current position.  May block if the reader
-- is far behind.
--
-- first check for space: if the buffer is full (cbSem == bufsize-1), wait until
-- cbSem decreases.  The buffer size is reduced by 1 because the reader
-- decrements the semaphore before reading the value.
--
-- This is only safe if all writes are serialized at a higher level.
--
-- When space is available, write the value
--
-- finally increment cbSem to indicate another value is ready
writeSeqBlocking :: Storable a => CircularBuffer -> Int -> a -> IO ()
writeSeqBlocking cb@CircularBuffer{..} writePos val = do
    -- first check if the buffer is full, and if so, wait until space is
    -- available.  Currently just spinning on this, because we expect it to be
    -- a rare occurrence.
    let waitUntilAvailable = do
            curLag <- unsafeSemGetValue cbSem
            when (curLag >= cbSize-1) $ do
                traceEventIO "writeSeqBlocking: waitUntilAvailable spinning"
                waitUntilAvailable
    waitUntilAvailable
    writeSeq cb writePos val
    unsafeSemPost cbSem
{-# INLINEABLE writeSeqBlocking #-}

-- | write a list of items, starting from the given sequence number.
-- Attempt to write everything in one batch.
-- returns the number of items written (which should be the length of the list)
writeSeqList :: Storable a => CircularBuffer -> Int -> [a] -> IO Int
writeSeqList cb@CircularBuffer{..} writePos = go 0
  where
    go !len [] = return len
    go !prevWritten xs = do
        currentLag <- unsafeSemGetValue cbSem
        let numReady = cbSize-currentLag
            (toWrite,toWait) = splitAt numReady xs
            offset = writePos+prevWritten
        numWritten <- foldM (\(!ix) x -> writeSeq cb (offset+ix) x >> return (ix+1)) 0 toWrite
        replicateM_ numWritten (unsafeSemPost cbSem)
        go (prevWritten+numWritten) toWait
{-# INLINE writeSeqList #-}

------------------------------------------------------------------
-- low-level interface
--
-- these functions deal *only* with reading from/writing to the buffer.  They
-- do not synchronize results.

-- precondition: must have a valid sequence number.
-- this is not checked.
readSeq :: Storable a => CircularBuffer -> Int -> IO a
readSeq CircularBuffer{..} rseq = do
    let cbPtr  = castPtr $ sbPtr $ cbBuf
        offset = rseq .&. (cbSize-1)
    peek (cbPtr `advancePtr` offset)
-- ghc inlines these already

-- precondition: must have a valid sequence number.
-- this is not checked.
readSeqs :: Storable a => CircularBuffer -> Int -> Int -> IO [a]
readSeqs CircularBuffer{..} rseq count
  | count <= 0 = return []
  | otherwise  = go (count-1) []
  where
    cbPtr = castPtr $ sbPtr cbBuf
    go 0 acc = do
        let offset = rseq .&. (cbSize-1)
        x <- peek (cbPtr `advancePtr` offset)
        return (x:acc)
    go n acc = do
        let offset = (n+rseq) .&. (cbSize-1)
        x <- peek (cbPtr `advancePtr` offset)
        go (n-1) (x:acc)
{-# INLINE readSeqs #-}

-- write to a position in the buffer.  Doesn't validate anything.
writeSeq :: Storable a => CircularBuffer -> Int -> a -> IO ()
writeSeq CircularBuffer{..} wseq val = do
    let cbPtr  = castPtr $ sbPtr $ cbBuf
        offset = wseq .&. (cbSize-1)
    poke (cbPtr `advancePtr` offset) val

-- Does the same as `semWait`, but cheaper if the semaphore
-- is immediately available.
waitAndLock :: Semaphore -> IO ()
waitAndLock sem = do
    gotLock <- unsafeSemTryWait sem
    when (not gotLock) $ do
        gotLock' <- semTimedWait 1 0 sem
        allowInterrupt
        when (not gotLock') $ waitAndLock sem

waitSpin :: Int -> Semaphore -> IO ()
waitSpin n sem = go (20 :: Int)
  where
    go 0 = yield >> go 20
    go k = do
      gotLock <- unsafeSemTryWait sem
      when (not gotLock) $ do
        replicateM_ n $ return ()
        go (k-1)

waitBackoff :: Int -> Int -> Int -> Semaphore -> IO ()
waitBackoff k0 stay k1 sem = go 0 0
  where
    go _ 0 = do
        gotLock <- unsafeSemTryWait sem
        when (not gotLock) $ go stay k0
    go 0 n = go stay (n*2)
    go l n
      | n < k1 = do
        mapM_ (\_ -> return ()) [0..n]
        gotLock <- unsafeSemTryWait sem
        when (not gotLock) $ go (l-1) n
      | otherwise = do
          threadDelay 500
          gotLock <- unsafeSemTryWait sem
          when (not gotLock) $ go 1 n

------------------------------------------------------------------
-- size of an int, as a compile-time constant.
sizeOfInt :: Int
sizeOfInt = $(let sz = sizeOf (0::Int) in [| sz |])

