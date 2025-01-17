module Data.ByteString.SuperBuffer.Pure
    ( SuperBuffer, withBuffer, appendBuffer, appendBufferT )
where

import Control.Concurrent.MVar
import Control.Exception
import Data.Bits
import Data.IORef
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Primitive.PrimRef
import Data.Primitive.Addr
import GHC.Exts (Ptr(..))

-- | The buffer data structure.
data SuperBuffer
    = SuperBuffer
    { sb_buffer :: {-# UNPACK #-}!(PrimRef Addr)
    , sb_currentSize :: {-# UNPACK #-} !(PrimRef Int)
    , sb_maxSize :: {-# UNPACK #-}!(PrimRef Int)
    , sb_lock :: {-# UNPACK #-}!(MVar ())
    }

-- | Allocate a new buffer with a given initial size. The perfect starting point
-- depends on the expected total size and the average size for a single chunk
-- written with 'appendBuffer'. You can always start with 1024 and optimize from
-- there with benchmarks. Please note that the SuperBuffer will no longer be
-- valid after this function terminates, so do NOT pass it to some other
-- thread without waiting for it to finish in the action.
withBuffer :: Int -> (SuperBuffer -> IO ()) -> IO BS.ByteString
withBuffer size action =
    do Ptr ptr <- mallocBytes size
       bufRef  <- newPrimRef (Addr ptr)
       go bufRef `onException` freeOnException bufRef
    where
        freeOnException ref =
            do Addr ptr <- readPrimRef ref
               free (Ptr ptr)
        go ref =
            do sizeRef    <- newPrimRef 0
               maxSizeRef <- newPrimRef size
               lock       <- newEmptyMVar
               let sb = SuperBuffer ref sizeRef maxSizeRef lock
               action sb
               readBuffer sb
{-# INLINE withBuffer #-}

-- | Write a bytestring to the buffer and grow the buffer if needed. Note that only
-- one thread at any given time may call this function. Use 'appendBufferT' when
-- accessing 'SuperBuffer' from multiple threads.
appendBuffer :: SuperBuffer -> BS.ByteString -> IO ()
appendBuffer sb bs
    | BS.null bs = pure ()
    | otherwise =
          BS.unsafeUseAsCStringLen bs $ \(cstr, len) ->
          do currentSize <- readPrimRef (sb_currentSize sb)
             maxSize <- readPrimRef (sb_maxSize sb)
             let nextSize = currentSize + len
             Addr writePtr <-
                 if nextSize > maxSize
                 then do let maxSize' = nextSize + unsafeShiftR nextSize 1
                         writePrimRef (sb_maxSize sb) maxSize'
                         Addr buff <- readPrimRef (sb_buffer sb)
                         Ptr buff' <- reallocBytes (Ptr buff) maxSize'
                         writePrimRef (sb_buffer sb) (Addr buff')
                         pure (Addr buff')
                 else readPrimRef (sb_buffer sb)
             let copyTarget = Ptr writePtr `plusPtr` currentSize
             copyBytes copyTarget cstr len
             writePrimRef (sb_currentSize sb) (currentSize + len)
{-# INLINE appendBuffer #-}

-- | Write a bytestring to the buffer and grow the buffer if needed. This function
-- can be used accross different threads, but is slower than 'appendBuffer'.
appendBufferT :: SuperBuffer -> BS.ByteString -> IO ()
appendBufferT sb bs =
    bracket_ (putMVar (sb_lock sb) ()) (takeMVar (sb_lock sb)) $
    appendBuffer sb bs
{-# INLINE appendBufferT #-}

-- | Read the final buffer contents. This must only be called once
readBuffer :: SuperBuffer -> IO BS.ByteString
readBuffer sb =
    do (Addr buff, currentSize, maxSize) <-
           (,,)
           <$> readPrimRef (sb_buffer sb)
           <*> readPrimRef (sb_currentSize sb)
           <*> readPrimRef (sb_maxSize sb)
       finalPtr <-
           if currentSize < maxSize
           then reallocBytes (Ptr buff) currentSize
           else pure (Ptr buff)
       BS.unsafePackCStringFinalizer finalPtr currentSize (free finalPtr)
{-# INLINE readBuffer #-}
