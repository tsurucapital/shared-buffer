{-# LANGUAGE ForeignFunctionInterface #-}
module System.Posix.AtomicOps (
  -- * types
  FPRef
  -- * basic interface
, newFPRef
, readFPRef
, writeFPRef

  -- * FPRef Int operations
, fetchAddFPRef
, addFetchFPRef
) where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

-- | A reference to a value held in a Foreign Ptr
newtype FPRef a = FPRef { unFPRef :: ForeignPtr a }

foreign import ccall unsafe "fetch_add_int64" c_fetch_add :: Ptr CInt -> CInt -> IO CInt
foreign import ccall unsafe "add_fetch_int64" c_add_fetch :: Ptr CInt -> CInt -> IO CInt

newFPRef :: Storable a => a -> IO (FPRef a)
newFPRef a = do
    fp <- mallocForeignPtr
    withForeignPtr fp (flip poke a)
    return $ FPRef fp

readFPRef :: Storable a => FPRef a -> IO a
readFPRef (FPRef fp) = withForeignPtr fp peek

writeFPRef :: Storable a => FPRef a -> a -> IO ()
writeFPRef (FPRef fp) a = withForeignPtr fp (flip poke a)

-- | fetch the previous value in an 'FPRef', and increment the FPRef by the given
-- amount.  This operation is atomic.
fetchAddFPRef :: FPRef Int -> Int -> IO Int
fetchAddFPRef (FPRef fp) incr = withForeignPtr fp $ \p -> fromIntegral `fmap` c_fetch_add (castPtr p) (fromIntegral incr)

-- | increment the FPRef by the given amount and return the new value.  This operation is atomic.
addFetchFPRef :: FPRef Int -> Int -> IO Int
addFetchFPRef (FPRef fp) incr = withForeignPtr fp $ \p -> fromIntegral `fmap` c_add_fetch (castPtr p) (fromIntegral incr)
