{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
module System.Posix.MQueue (
  WriteBuffer
, ReadBuffer
, Shared (..)
-- * normal interface
, putBuffer
, getBuffer
, putBufferList
, getAvailable
) where

import System.Posix.CircularBuffer (Shared (..))

import Control.Applicative
import Control.Monad
import Data.Bits
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Data.Int
import Data.Word

#include <mqueue.h>
#include <fcntl.h>

type MQD = #{type mqd_t}

data WriteBuffer a = WB String {-# UNPACK #-} !MQD
data ReadBuffer a  = RB String {-# UNPACK #-} !MQD

foreign import ccall "mq_open"
  c_mq_open_c :: CString -> CInt -> CInt -> Ptr () -> IO MQD

foreign import ccall "mq_open"
  c_mq_open :: CString -> CInt -> IO MQD

foreign import ccall "mq_close"
  c_mq_close :: MQD -> IO CInt

foreign import ccall "mq_unlink"
  c_mq_unlink :: CString -> IO CInt

foreign import ccall "mq_send"
  c_mq_send :: MQD -> Ptr () -> #{type size_t} -> CInt -> IO CInt

foreign import ccall "mq_receive"
  c_mq_receive :: MQD -> Ptr () -> #{type size_t} -> Ptr CInt -> IO CInt


cREAT, eXCL, rONLY, wRONLY :: CInt
cREAT = #{const O_CREAT}
eXCL = #{const O_EXCL}
rONLY  = #{const O_RDONLY}
wRONLY = #{const O_WRONLY}

mkAttr :: forall a b. Storable a => a -> Int -> (Ptr () -> IO b) -> IO b
mkAttr a sz f = allocaBytes #{size struct mq_attr} $ \attrP -> do
    #{poke struct mq_attr, mq_flags} attrP (0 :: #{type long})
    #{poke struct mq_attr, mq_maxmsg} attrP sz
    #{poke struct mq_attr, mq_msgsize} attrP (sizeOf a)
    #{poke struct mq_attr, mq_curmsgs} attrP (0 :: #{type long})
    f attrP


instance Storable a => Shared (WriteBuffer a) where
  createBuffer name _ sz fmode = do
      x <- withCString name $ \cName -> throwErrnoIfMinus1 "createBuffer: WriteBuffer" $ mkAttr (undefined :: a) sz $ c_mq_open_c cName (wRONLY .|. cREAT .|. eXCL) (fromIntegral fmode)
      return $ WB name x
  openBuffer name _ _ _fmode = do
      WB name <$> withCString name (\cName -> c_mq_open cName wRONLY)
  closeBuffer (WB _ mqd) = void $ throwErrnoIfMinus1 "closeBuffer: WriteBuffer" $ c_mq_close mqd
  removeBuffer wb = closeBuffer wb >> unlinkBuffer wb
  unlinkBuffer (WB name _) = void $ withCString name $ throwErrnoIfMinus1 "unlinkBuffer: WriteBuffer" . c_mq_unlink 

instance Storable a => Shared (ReadBuffer a) where
  createBuffer name _ sz fmode = do
      x <- withCString name $ \cName -> throwErrnoIfMinus1 "createBuffer: ReadBuffer" $ mkAttr (undefined :: a) sz $ c_mq_open_c cName (rONLY .|. cREAT .|. eXCL) (fromIntegral fmode)
      return $ RB name x
  openBuffer name _ _ _fmode = do
      RB name <$> withCString name (\cName -> c_mq_open cName rONLY)
  closeBuffer (RB _ mqd) = void $ throwErrnoIfMinus1 "closeBuffer: ReadBuffer" $ c_mq_close mqd
  removeBuffer wb = closeBuffer wb >> unlinkBuffer wb
  unlinkBuffer (RB name _) = void $ withCString name $ throwErrnoIfMinus1 "unlinkBuffer: ReadBuffer" . c_mq_unlink 


-- | Write a value to the writer end.
--
-- This function is thread-safe.
putBuffer :: Storable a => WriteBuffer a -> a -> IO ()
putBuffer (WB _ mqd) val = alloca $ \msgP -> do
    poke msgP val
    void $ throwErrnoIfMinus1 "putBuffer" $ c_mq_send mqd (castPtr msgP) (fromIntegral $ sizeOf val) 1

{-# INLINEABLE putBuffer #-}

-- | read the next value from the reader end.
--
-- This function is *NOT* thread-safe.
getBuffer :: forall a. Storable a => ReadBuffer a -> IO a
getBuffer (RB _ mqd) = alloca $ \msgP -> do
    let sz = sizeOf (undefined :: a)
    numBytes <- throwErrnoIfMinus1 "getBuffer" $ c_mq_receive mqd (castPtr msgP) (fromIntegral sz) nullPtr
    when (numBytes /= fromIntegral sz) $ error "getBuffer: too few bytes"
    peek msgP

{-# INLINEABLE getBuffer #-}


-- | Write a list of values to the writer end.
--
-- This function is thread-safe.
putBufferList :: Storable a => WriteBuffer a -> [a] -> IO ()
putBufferList wb = mapM_ (putBuffer wb)
{-# INLINE putBufferList #-}

-- | read all currently available values from the reader end.
--
-- This function is *NOT* thread-safe.
getAvailable :: Storable a => ReadBuffer a -> IO [a]
getAvailable (RB _ _) = error "getAvailable: not implemented"
