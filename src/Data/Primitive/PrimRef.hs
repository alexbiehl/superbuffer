{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE Unsafe          #-}
module Data.Primitive.PrimRef where

import           Control.Monad.Primitive
import           Data.Primitive

newtype PrimRef a = PrimRef (MutableByteArray (PrimState IO))

instance Eq (PrimRef a) where
  PrimRef m == PrimRef n = sameMutableByteArray m n
  {-# INLINE (==) #-}

-- | Create a primitive reference.
newPrimRef :: Prim a => a -> IO (PrimRef a)
newPrimRef a = do
  m <- newByteArray (sizeOf a)
  writeByteArray m 0 a
  return (PrimRef m)
{-# INLINE newPrimRef #-}

-- | Read a primitive value from the reference
readPrimRef :: Prim a => PrimRef a -> IO a
readPrimRef (PrimRef m) = readByteArray m 0
{-# INLINE readPrimRef #-}

-- | Write a primitive value to the reference
writePrimRef :: Prim a => PrimRef a -> a -> IO ()
writePrimRef (PrimRef m) a = writeByteArray m 0 a
{-# INLINE writePrimRef #-}
