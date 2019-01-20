{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}

module Data.Vector.Indexed
    ( Vector
    , vector
      -- * Construction
    , fromList
    , fromVector
    , singleton
    , replicate
    , indexes
    , generate, generateM
      -- * Queries
    , (!), (!?)
    , bounds
    , assocs
    , elems
      -- * Element-wise operations
    , indexed
      -- ** Mapping
    , map
    , imap
      -- ** Monadic mapping
    , mapM, mapM_
    , imapM, imapM_
      -- * Folds
    , foldl'
    , sum
      -- * Scans
    , prescanl'
      -- * Updates and accumulation
    , (//)
    , accum
    , accum'
      -- * Zipping
      -- | These all require that the bounds of all arguments are identical.
      -- ** Without indexes
    , zipWith
    , zipManyWith
      -- ** With indexes
    , izipWith
      -- * Conversion
    , convert
      -- * Linear algebra
    , quadrance
    , norm
    , dot
      -- * Mutable vectors
    , freeze
    , unsafeFreeze
    , thaw
    , unsafeThaw
    , create
    ) where

import Control.Exception
import Control.DeepSeq
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as Foldable
import Data.Ix as Ix
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Indexed.Mutable as VIM
import qualified Data.Vector.Fusion.Bundle as B
import Prelude hiding (map, mapM, mapM_, replicate, sum, zipWith)

-- | @Vector v i a@ is a vector of values of type @a@ whose dimensions are
-- identified by values of type @i@. The values are stored in a vector of type
-- @v a@.
data Vector v i a = Vector { lower  :: !i
                           , upper  :: !i
                           , vector :: !(v a)
                           }
                  deriving (Ord, Eq, Functor, Foldable, Traversable)

instance NFData (v a) => NFData (Vector v i a) where
    rnf (Vector _ _ v) = rnf v

instance (Show i, Show (v a)) => Show (Vector v i a) where
    showsPrec _ (Vector l u v) =
        showString "fromVector (" . shows l . showChar ',' . shows u . showString ") " . shows v

-- | \(O(1)\). The bounds of a 'Vector'.
bounds :: Vector v i a -> (i, i)
bounds (Vector l u _) = (l, u)
{-# INLINE bounds #-}

indexStream :: Ix i => Vector v i a -> B.Bundle v i
indexStream = B.fromList . range . bounds
{-# INLINE indexStream #-}

-- | \(O(n)\). Index into a 'Vector'. Fails if the index doesn't fall within the
-- 'bounds' of the vector.
(!) :: (VG.Vector v a, Ix i) => Vector v i a -> i -> a
(!) v i = vector v VG.! Ix.index (bounds v) i
{-# INLINE (!) #-}

-- | \(O(n)\). Index into a 'Vector'.
(!?) :: (VG.Vector v a, Ix i) => Vector v i a -> i -> Maybe a
v !? i = vector v VG.!? Ix.index (bounds v) i
{-# INLINE (!?) #-}

-- | \(O(n)\). Map over each element of a 'Vector'.
map :: (VG.Vector v a, VG.Vector v b) => (a -> b) -> Vector v i a -> Vector v i b
map f v = Vector (lower v) (upper v) (VG.map f $ vector v)
{-# INLINE map #-}

-- | \(O(n)\). Map over each element of a 'Vector' and its index.
imap :: (VG.Vector v a, VG.Vector v b, Ix i) => (i -> a -> b) -> Vector v i a -> Vector v i b
imap f v =
    v { vector = VG.unstream $ B.zipWith f (indexStream v) (VG.stream $ vector v) }
{-# INLINE imap #-}

-- | \(O(n)\). Monadically map over each element of a 'Vector'.
mapM :: (VG.Vector v a, VG.Vector v b, Monad m)
     => (a -> m b) -> Vector v i a -> m (Vector v i b)
mapM f v = Vector (lower v) (upper v) <$> VG.mapM f (vector v)
{-# INLINE mapM #-}

-- | \(O(n)\). Monadically map over each element of a 'Vector', discarding the result.
mapM_ :: (VG.Vector v a, Monad m)
      => (a -> m ()) -> Vector v i a -> m ()
mapM_ f = VG.mapM_ f . vector
{-# INLINE mapM_ #-}

-- | \(O(n)\). Monadically map over each element of a 'Vector' and its index.
imapM :: (VG.Vector v a, VG.Vector v b, PrimMonad m, Ix i)
      => (i -> a -> m b) -> Vector v i a -> m (Vector v i b)
imapM f v = do
    mv' <- VGM.munstream $ B.mapM id $ B.zipWith f (indexStream v) (VG.stream $ vector v)
    v' <- VG.freeze mv'
    return $ v { vector = v' }
{-# INLINE imapM #-}

-- | \(O(n)\). Map over each element of a 'Vector' and its index.
imapM_ :: (Ix i, VG.Vector v a, VG.Vector v b, Monad m)
      => (i -> a -> m b) -> Vector v i a -> m ()
imapM_ f v =
    B.mapM_ id $ B.zipWith f (indexStream v) (VG.stream $ vector v)
{-# INLINE imapM_ #-}

-- | \(O(n)\). Strictly fold over the given 'Vector'.
foldl' :: (VG.Vector v b) => (a -> b -> a) -> a -> Vector v i b -> a
foldl' f z = VG.foldl' f z . vector
{-# INLINE [1] foldl' #-}

-- This is a fairly important case (e.g. for the dot product) which ends up
-- breaking due to the bounds checking performed by zipWith.
{-# RULES "foldl' f z (zipWith g v1 v2)"
      forall f z g v1 v2. foldl' f z (zipWith g v1 v2)
        = withSameBounds [bounds v1, bounds v2] (const $ VG.foldl' f z $ VG.zipWith g (vector v1) (vector v2))
    #-}

prescanl' :: (VG.Vector v a, VG.Vector v b)
          => (a -> b -> a) -> a -> Vector v i b -> Vector v i a
prescanl' f z v = Vector (lower v) (upper v) $ VG.prescanl' f z (vector v)
{-# INLINE prescanl' #-}

-- | \(O(n)\). Strictly Compute the sum of the elements of a 'Vector'.
sum :: (Num b, VG.Vector v b) => Vector v i b -> b
sum = foldl' (+) 0
{-# INLINE sum #-}

-- | \(O(1)\). Construct a singleton 'Vector' covering the given index with the given value.
singleton :: (VG.Vector v a) => i -> a -> Vector v i a
singleton i a = Vector i i (VG.singleton a)

-- | \(O(n)\). Construct a 'Vector' over the given range, mapping each index to the given
-- value.
replicate :: (Ix i, VG.Vector v a) => (i, i) -> a -> Vector v i a
replicate (l, u) x = Vector l u (VG.replicate (rangeSize (l,u)) x)
{-# INLINEABLE replicate #-}

-- | \(O(1)\). .Constructor an indexed 'Vector' over the given index range from a vector of
-- values. Expects that the length of the input vector be @'rangeSize' (l,u)@.
-- Errors otherwise.
fromVector :: (Ix i, VG.Vector v a) => (i, i) -> v a -> Vector v i a
fromVector (l,u) v
  | VG.length v == len = Vector l u v
  | otherwise          = error $ "Data.Vector.Indexed.fromVector: Expected length "
                                 ++ show len++", found length "++show (VG.length v)
  where len = rangeSize (l,u)

-- | \(O(m+n)\). For each pair @(i,a)@ from the list, replace the vector element
-- at position @i@ by @a@.
(//) :: (Ix i, VG.Vector v a)
     => Vector v i a -> [(i, a)] -> Vector v i a
v // xs = accum (const id) v xs

-- | \(O(n)\). Accumulate elements from a list into a 'Vector'.
accum :: (Ix i, VG.Vector v a) => (a -> b -> a) -> Vector v i a -> [(i, b)] -> Vector v i a
accum f (Vector l u v) = Vector l u . VG.accum f v . fmap (first $ Ix.index b)
  where b = (l, u)
{-# INLINEABLE accum #-}

-- | \(O(n)\). Accumulate elements from a list into a 'Vector' initialized to the given value.
accum' :: (Ix i, VG.Vector v a)
       => (i, i) -> (a -> b -> a) -> a -> [(i, b)] -> Vector v i a
accum' (l,u) f x0 xs = Vector l u $ VG.create $ do
    v <- VGM.replicate n x0
    Foldable.mapM_ (upd v) xs
    return v
  where
    upd v (i,b) = do
        let !i' = Ix.index (l,u) i
        a <- VGM.read v i'
        VGM.unsafeWrite v i' $! f a b

    !n = rangeSize (l,u)
{-# INLINE accum' #-}

-- | \(O(n)\). Generate a 'Vector' from a set of bounds and a list of elements.
-- Expects the list to be of length @'rangeSize' bounds@. May fail with error
-- otherwise.
fromList :: (Ix i, VG.Vector v a) => (i, i) -> [a] -> Vector v i a
fromList bs xs =
    assert (length xs == len) $ fromVector bs $ VG.fromListN len xs
  where len = rangeSize bs
{-# INLINE fromList #-}

-- | \(O(n)\). Generate a 'Vector' from a set of bounds and a function mapping
-- each index to its associated value.
generate :: (Ix i, VG.Vector v a) => (i, i) -> (i -> a) -> Vector v i a
generate bs f = fromList bs $ fmap f (Ix.range bs)
{-# INLINE generate #-}

-- | \(O(n)\). Generate a 'Vector' from a set of bounds and a function mapping
-- each index to its associated value.
generateM :: (Ix i, VG.Vector v a, PrimMonad m)
          => (i, i) -> (i -> m a) -> m (Vector v i a)
generateM bs f = do
    mv <- VGM.munstream $ B.mapM f $ B.fromList $ range bs
    v <- VG.freeze mv
    return $ uncurry Vector bs v
{-# INLINE generateM #-}

-- | \(O(n)\). Produce a vector of indices.
indexes :: (Ix i, VG.Vector v i)
        => (i, i) -> Vector v i i
indexes (l, u) = Vector l u $ VG.unstream $ B.fromList $ range (l, u)
{-# INLINE indexes #-}

-- | \(O(n)\). Zip each value of a 'Vector' with its index.
indexed :: (Ix i, VG.Vector v a, VG.Vector v (i, a))
        => Vector v i a -> v (i, a)
indexed v = VG.unstream $ B.zip (indexStream v) (VG.stream $ vector v)
{-# INLINE indexed #-}

-- | \(O(n)\). List the index/value associations of a 'Vector'.
assocs :: (Ix i, VG.Vector v a) => Vector v i a -> [(i, a)]
assocs v = zip (range $ bounds v) (VG.toList $ vector v)
{-# INLINE assocs #-}

-- | \(O(n)\). List the values of a 'Vector'.
elems :: VG.Vector v a => Vector v i a -> [a]
elems = VG.toList . vector
{-# INLINE elems #-}

-- | \(O(n)\). Inner product.
dot :: (Eq i, Num a, VG.Vector v a) => Vector v i a -> Vector v i a -> a
dot v1 v2 = withSameBounds [bounds v1, bounds v2] $ const $
    VG.sum $ VG.zipWith (*) (vector v1) (vector v2)

-- | \(O(n)\). Compute \( \sum_i i^2 \).
quadrance :: (Num a, VG.Vector v a) => Vector v i a -> a
-- Ideally we would write this in terms of map and sum but I have had trouble
-- getting this to fuse:
--   quadrance = sum . map squared
quadrance = VG.sum . VG.map squared . vector
  where squared x = x*x
{-# INLINE quadrance #-}

-- | \(O(n)\). Compute the \(L^2\) norm, \( \sqrt{\sum_i i^2} \).
norm :: (RealFloat a, VG.Vector v a) => Vector v i a -> a
norm = sqrt . quadrance
{-# INLINE norm #-}

sameBounds :: Eq i => [(i,i)] -> Maybe (i,i)
sameBounds (x:xs)
  | all (== x) xs = Just x
sameBounds _ = Nothing
{-# INLINE sameBounds #-}

data IncompatibleBounds = IncompatibleBounds
                        deriving (Show)
instance Exception IncompatibleBounds

withSameBounds :: Eq i => [(i,i)] -> ((i,i) -> a) -> a
withSameBounds bnds cont =
    case sameBounds bnds of
      Just (l,u) -> cont (l,u)
      Nothing -> throw IncompatibleBounds
{-# NOINLINE withSameBounds #-}

-- | Zip together many 'Vector's with a function.
zipManyWith :: (Eq i, VG.Vector v a)
            => (a -> a -> a)
            -> NE.NonEmpty (Vector v i a)
            -> Vector v i a
zipManyWith f allVs@(v0 NE.:| vs) =
    withSameBounds (fmap bounds $ NE.toList allVs) $
    \(l,u) -> Vector l u $ VG.create $ do
        acc <- VG.thaw $ vector v0
        let g i y = do
                x0 <- VGM.unsafeRead acc i
                VGM.unsafeWrite acc i (f x0 y)
        Foldable.mapM_ (\(Vector _ _ v) -> VG.imapM_ g v) vs
        return acc
{-# INLINE zipManyWith #-}

-- | \(O(n)\) Zip together two 'Vector's with a function.
zipWith :: (Eq i, VG.Vector v a, VG.Vector v b, VG.Vector v c)
        => (a -> b -> c)
        -> Vector v i a -> Vector v i b
        -> Vector v i c
zipWith f v1 v2 =
    withSameBounds [bounds v1, bounds v2] $
    \(l,u) -> Vector l u $ VG.zipWith f (vector v1) (vector v2)
{-# INLINE [1] zipWith #-}

-- | \(O(n)\) Zip together two 'Vector's with a function and indexes.
izipWith :: (Ix i, VG.Vector v a, VG.Vector v b, VG.Vector v c)
         => (i -> a -> b -> c)
         -> Vector v i a -> Vector v i b
         -> Vector v i c
izipWith f v1 v2 =
    withSameBounds [bounds v1, bounds v2]
    $ \(l,u) -> Vector l u $ VG.unstream
    $ B.zipWith3 f (indexStream v1) (VG.stream $ vector v1) (VG.stream $ vector v2)
{-# INLINE izipWith #-}

-- | Convert between vector types.
convert :: (VG.Vector v a, VG.Vector v' a)
        => Vector v i a -> Vector v' i a
convert (Vector l u v) = Vector l u (VG.convert v)
{-# INLINE convert #-}

-- | \(O(n)\) Yield a mutable copy of the immutable vector.
thaw :: (PrimMonad m, VG.Vector v a)
     => Vector v i a
     -> m (VIM.MVector (VG.Mutable v) (PrimState m) i a)
thaw (Vector l u v) = do
    v' <- VG.thaw v
    return $ VIM.MVector l u v'

-- | \(O(1)\) Unsafely convert an immutable vector to a mutable one without copying.
-- The immutable vector may not be used after this operation.
unsafeThaw :: (PrimMonad m, VG.Vector v a)
           => Vector v i a
           -> m (VIM.MVector (VG.Mutable v) (PrimState m) i a)
unsafeThaw (Vector l u v) = do
    v' <- VG.unsafeThaw v
    return $ VIM.MVector l u v'

-- | \(O(1)\) Unsafe convert a mutable vector to an immutable one without copying.
-- The mutable vector may not be used after this operation.
freeze :: (PrimMonad m, VG.Vector v a)
       => VIM.MVector (VG.Mutable v) (PrimState m) i a
       -> m (Vector v i a)
freeze (VIM.MVector l u v) = do
    v' <- VG.freeze v
    return $ Vector l u v'

-- | \(O(n)\) Yield an immutable copy of the mutable vector.
unsafeFreeze :: (PrimMonad m, VG.Vector v a)
             => VIM.MVector (VG.Mutable v) (PrimState m) i a
             -> m (Vector v i a)
unsafeFreeze (VIM.MVector l u v) = do
    v' <- VG.unsafeFreeze v
    return $ Vector l u v'

-- | Execute the monadic action and freeze the resulting vector.
create :: (VG.Vector v a)
       => (forall s. ST s (VIM.MVector (VG.Mutable v) s i a))
       -> Vector v i a
create f = runST (f >>= unsafeFreeze)
