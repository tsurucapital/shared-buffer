{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}
------------------------------------------------------------------
-- semaphore stuff.  System.Posix.Semaphore uses all safe calls.  blech.

module System.Posix.Semaphore.Unsafe (
  unsafeSemTryWait
, unsafeSemPost
, unsafeSemGetValue
, unsafeSemLock
, semTimedWait
, module System.Posix.Semaphore
) where

import Control.Monad
import Control.Applicative
import Data.Int
import Foreign.C
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Semaphore
import System.Posix.Time
import Unsafe.Coerce

#include <semaphore.h>
#include <errno.h>

-- | Attempt to lock the semaphore without blocking, and call error if the
-- semaphore is unavailable.
--
-- This function should only be called when you want to lock the semaphore, and
-- can guarantee that doing so will not block.
unsafeSemLock :: Semaphore -> IO ()
unsafeSemLock sem = do
    didLock <- unsafeSemTryWait sem
    when (not didLock) $ error "unsafeSemLock: couldn't lock"

-- | Attempt to lock the semaphore without blocking.  Immediately return
--   False if it is not available.
unsafeSemTryWait :: Semaphore -> IO Bool
unsafeSemTryWait (unsafeCoerce -> fptr) = withForeignPtr fptr semTrywait'
    where semTrywait' sem = do res <- sem_trywait sem
                               (if res == 0 then return True
                                else do errno <- getErrno
                                        (if errno == eINTR
                                         then semTrywait' sem
                                         else if errno == eAGAIN
                                              then return False
                                              else throwErrno "unsafeSemTryWait"))

-- | Unlock the semaphore.
unsafeSemPost :: Semaphore -> IO ()
unsafeSemPost (unsafeCoerce -> fptr) = withForeignPtr fptr semPost'
    where semPost' sem = throwErrnoIfMinus1Retry_ "unsafeSemPost" $
                         sem_post sem

-- | Return the semaphore's current value.
unsafeSemGetValue :: Semaphore -> IO Int
unsafeSemGetValue (unsafeCoerce -> fptr) = withForeignPtr fptr semGetValue'
    where semGetValue' sem = alloca (semGetValue_ sem)

semGetValue_ :: Ptr () -> Ptr CInt -> IO Int
semGetValue_ sem ptr = do throwErrnoIfMinus1Retry_ "unsafeSemGetValue" $
                            sem_getvalue sem ptr
                          cint <- peek ptr
                          return $ fromEnum cint

----------------------------------------------------
-- timed wait

data SemT = SemT { tv_sec :: CTime, tv_nsec :: #{type long} } deriving Show

instance Storable SemT where
    sizeOf _ = #{size struct timespec}
    alignment _ = alignment (undefined :: CInt)
    peek p = SemT <$> #{peek struct timespec, tv_sec} p <*> #{peek struct timespec, tv_nsec} p
    poke p semT = do
        #{poke struct timespec, tv_sec}  p (tv_sec semT)
        #{poke struct timespec, tv_nsec} p (tv_nsec semT)

-- | Wait on a semaphore with a timeout value.  Returns True if a lock was
-- acquired, False if it timed out before acquiring a lock.
--
-- if interrupted by a signal, semTimedWait will retry.  An exception will be
-- raised if sem_timedwait() fails for any other reason.
semTimedWait :: Int -> Int -> Semaphore -> IO Bool
semTimedWait secs nsecs (unsafeCoerce -> sem) = do
  curTime <- epochTime
  let timeout = SemT (curTime+fromIntegral secs) (fromIntegral nsecs)
  runWithTime timeout
  where
    runWithTime t = do
        outval <- withForeignPtr sem $ \semP -> alloca $ \tPtr -> do
            poke tPtr t
            sem_timedwait semP tPtr
        case outval of
            0 -> return True
            _ -> do
                   err <- getErrno
                   case () of
                     () | err == eTIMEDOUT -> return False
                        | err == eINTR -> runWithTime t
                        | otherwise -> throwErrno "semTimedWait"

----------------------------------------------------
foreign import ccall unsafe "sem_trywait"
        sem_trywait :: Ptr () -> IO CInt
foreign import ccall unsafe "sem_post"
        sem_post :: Ptr () -> IO CInt
foreign import ccall unsafe "sem_getvalue"
        sem_getvalue :: Ptr () -> Ptr CInt -> IO Int
foreign import ccall "sem_timedwait"
        sem_timedwait :: Ptr () -> Ptr SemT -> IO CInt
