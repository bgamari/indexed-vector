{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Vector.Indexed.Mutable
    ( MVector(..)
      -- * Creation
    , new
    , replicate
      -- * Accessors
    , length
      -- * Modification
    , read
    , write
    , modify
    ) where

import Data.Ix as Ix
import Control.Monad.Primitive
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Prelude hiding (read, replicate)

data MVector v s i a = MVector { lower :: !i
                               , upper :: !i
                               , vector  :: !(v s a)
                               }

-- | /O(1)/. The bounds of a 'Vector'.
bounds :: MVector v s i a -> (i, i)
bounds (MVector l u _) = (l, u)
{-# INLINE bounds #-}

new :: (Ix i, PrimMonad m, VGM.MVector v a)
    => (i, i) -> m (MVector v (PrimState m) i a)
new (l,u) = do
    v <- VGM.new $ Ix.rangeSize (l,u)
    return $ MVector l u v

replicate :: (Ix i, PrimMonad m, VGM.MVector v a)
          => (i, i) -> a -> m (MVector v (PrimState m) i a)
replicate (l,u) x = do
    v <- VGM.replicate (Ix.rangeSize (l,u)) x
    return $ MVector l u v
{-# INLINE replicate #-}

read :: (Ix i, PrimMonad m, VGM.MVector v a)
     => MVector v (PrimState m) i a -> i -> m a
read v'@(MVector l u v) i = VGM.read v (Ix.index (bounds v') i)
{-# INLINE read #-}

write :: (Ix i, PrimMonad m, VGM.MVector v a)
      => MVector v (PrimState m) i a -> i -> a -> m ()
write v'@(MVector l u v) i x = VGM.write v (Ix.index (bounds v') i) x
{-# INLINE write #-}

modify :: (Ix i, PrimMonad m, VGM.MVector v a)
       => MVector v (PrimState m) i a -> (a -> a) -> i -> m ()
modify v' f i = do
    let i' = Ix.index (bounds v') i
    x <- VGM.read (vector v') i'
    VGM.write (vector v') i' (f x)
