{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_GHC -Wall #-}
module System.Posix.SharedBuffer (
-- * Main shared memory interface
  openSBuffer
, closeSharedBuffer
, removeSharedBuffer
, unlinkSharedBuffer
-- * private stuff (exported for wizards)
, SharedBuffer (..)
-- ** generalized buffer access
, Flags (..)
, Protection (..)
, getProtection
, writeProtection
-- ** re-exports
, ShmOpenFlags(..)
, openReadWriteFlags
, openReadFlags
) where

import Control.Exception (try, finally)
import Control.Monad
import Data.Bits
import Data.List (foldl')
import Foreign.Ptr
import Foreign.C.Error
import Foreign.C.Types
import System.Posix.SharedMem
import System.Posix (FileMode, Fd (..), closeFd)

------------------------------------------------------------------
-- foreign imports

foreign import ccall "mmap" c_mmap :: Ptr () -> CInt -> CInt -> CInt -> Fd -> CInt -> IO (Ptr ())
foreign import ccall "munmap" c_munmap :: Ptr () -> CInt -> IO CInt

foreign import ccall "ftruncate" c_ftruncate :: Fd -> CInt -> IO CInt

------------------------------------------------------------------
-- main user-visible stuff

-- | A shared memory object
data SharedBuffer = SharedBuffer
    { sbPtr :: {-# UNPACK #-} !(Ptr ())
    , sbLen :: {-# UNPACK #-} !CInt
    , sbName :: String
    }
    deriving (Show)

-- | Close a reference to a shared memory object.  Just calls 'munmap'.
closeSharedBuffer :: SharedBuffer -> IO ()
closeSharedBuffer SharedBuffer{sbPtr,sbLen} = do
    throwErrnoIfMinus1_ "munmap" $ c_munmap sbPtr (fromIntegral sbLen)

-- | Close a reference to a shared memory object and removes it.
-- Calls 'munmap' followed by 'shm_unlink'
removeSharedBuffer :: SharedBuffer -> IO ()
removeSharedBuffer sb@SharedBuffer{sbName} =
    closeSharedBuffer sb `finally`
        (void (try (shmUnlink sbName) :: IO (Either IOError ())))

-- | Unlink a shared buffer (shm_unlink) without closing the reference to it.
-- Any processes that have already opened the buffer (including this one)
-- should be able to continue accessing it.
--
-- After 'unlinkSharedBuffer', references should be closed with
-- closeSharedBuffer.
unlinkSharedBuffer :: SharedBuffer -> IO ()
unlinkSharedBuffer SharedBuffer{sbName} = do
    void (try (shmUnlink sbName) :: IO (Either IOError ()))

------------------------------------------------------------------
-- protection levels

-- | mmap protection level
data Protection =
    ProtNone
  | ProtRead
  | ProtWrite
  | ProtExec
  deriving (Eq, Show, Ord, Bounded)

instance Enum Protection where
    toEnum 0 = ProtNone
    toEnum 1 = ProtRead
    toEnum 2 = ProtWrite
    toEnum 4 = ProtExec
    toEnum _ = error "toEnum: invalid Protection level (bit-twiddle it?)"
    fromEnum ProtNone = 0
    fromEnum ProtRead = 1
    fromEnum ProtWrite = 2
    fromEnum ProtExec  = 4

getProtection :: [Protection] -> CInt
getProtection = fromIntegral . foldl' ((. fromEnum) . (.|.)) 0

writeProtection :: [Protection]
writeProtection = [ProtWrite, ProtRead]

------------------------------------------------------------------
-- flags

data Flags =
    MapShared  -- 0x01
  | MapPrivate -- 0x02
  deriving (Eq, Ord, Show)

instance Enum Flags where
    toEnum 1 = MapShared
    toEnum 2 = MapPrivate
    toEnum x = error $ "invalid mmap flag: " ++ show x
    fromEnum MapShared = 1
    fromEnum MapPrivate = 2

openReadWriteFlags :: ShmOpenFlags
openReadWriteFlags = ShmOpenFlags
  { shmCreate = True
  , shmReadWrite = True
  , shmExclusive = True
  , shmTrunc = False
  }

openReadFlags :: ShmOpenFlags
openReadFlags = ShmOpenFlags
  { shmCreate = False
  , shmReadWrite = False
  , shmExclusive = False
  , shmTrunc = False
  }

------------------------------------------------------------------
-- helper functions

-- | Open a shared memory object, then mmap it, with the specified flags.
openSBuffer :: String
           -> CInt
           -> ShmOpenFlags
           -> [Protection]
           -> FileMode
           -> IO SharedBuffer
openSBuffer sbName sbLen openFlags sbProtection sbpmode = do
    resetErrno
    fd <- shmOpen sbName openFlags sbpmode
    when (shmReadWrite openFlags) $
        throwErrnoIfMinus1_ "ftruncate" $ c_ftruncate fd sbLen
    sbPtr <- c_mmap nullPtr sbLen (getProtection sbProtection)
                            (fromIntegral $ fromEnum MapShared) fd 0
    errno <- getErrno
    closeFd fd
    if errno == eOK
        then return (SharedBuffer { sbPtr, sbLen, sbName })
        else throwErrno "openSBuffer: mmap"
