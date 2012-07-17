{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LATC.NestedList
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file latc/LICENSE)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- Nested lists as a linear algebra backend
-----------------------------------------------------------------------------

module Numeric.LATC.NestedList (
                                -- * Newtype wrappers
                                Vector
                               , Matrix
                                -- * Structural functions
                                -- ** Structural functions on a @Vector@
                               , fromList
                               , toList
                               , length
                               , map
                               , vbinary
                               , vindex
                               , vconcat
                                -- ** Structural functions on a @Matrix@
                               , fromLists
                               , toLists
                               , size
                               , mmap
                               , transpose
                               , mbinary
                               , mindex
                               , mconcatrows
                               , mconcatcols
                                -- ** Structural functions between @Vector@s and @Matrix@es
                               , toRows
                               , fromRows
                               , toCols
                               , fromCols
                               , mCol
                               , mRow
                                -- * Math functions
                               , matvec
                               , vecmat
                               , matmat
                               , inner
                               , outer
                               ) where

import Prelude hiding (length, map)
import qualified Prelude as P
import qualified Data.List as DL
import Data.Hashable ( Hashable(..) )

-- | A vector abstraction for lists
newtype Vector a = Vector {unvector :: [a]} deriving (Eq,Ord)

-- | A matrix abstraction for nested lists
newtype Matrix a = Matrix {unmatrix :: [[a]]} deriving (Eq,Ord)

instance Show a => Show (Matrix a) where
    show (Matrix m) = "Matrix " ++ show m

instance Show a => Show (Vector a) where
    show (Vector v) = "Vector " ++ show v

instance Hashable a => Hashable (Matrix a) where
  hash = hash

instance Hashable a => Hashable (Vector a) where
  hash = hash

instance Num a => Num (Vector a) where
  (+) = vbinary (+)
  (*) = vbinary (*)
  (-) = vbinary (-)

  abs = map abs
  signum = map signum

instance Num a => Num (Matrix a) where
  (+) = mbinary (+)
  (*) = mbinary (*)
  (-) = mbinary (-)

  abs = mmap abs
  signum = mmap signum

-- | Convert a list to a @Vector@
fromList :: [b] -> Vector b
fromList = Vector

-- | Convert a @Vector@ to a list
toList :: Vector b -> [b]
toList (Vector v) = v

-- | How many elements are in the vector?
length :: Vector b -> Int
length (Vector v) = P.length v

-- | Map a function @f@ over the elements of a vector
map :: (a -> b) -> Vector a -> Vector b
map f (Vector v) = Vector $ P.map f v

-- | Apply a binary function @f@ to two @Vector@s
vbinary :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
vbinary f (Vector v1) (Vector v2) = Vector $ P.zipWith f v1 v2

-- | Return the element at the given position in the vector.
-- Zero-indexed.
vindex :: Vector a -> Int -> a
vindex (Vector v) i = v !! i

-- | Concatenate two vectors end-to-end.
vconcat :: Vector a -> Vector a -> Vector a
vconcat (Vector v1) (Vector v2) = Vector $ v1 ++ v2

-- | Convert a list of lists to a @Matrix@ (row-major)
fromLists :: [[b]] -> Matrix b
fromLists = Matrix

-- | Convert a @Matrix@ to a list of lists
toLists :: Matrix b -> [[b]]
toLists (Matrix m) = m

-- | How many rows and columns does this matrix have?
size :: Matrix b -> (Int, Int)
size (Matrix a) = (P.length a, case take 1 a of
                                 [] -> 0
                                 x -> P.length x)

-- | Map a function @f@ over the elements of a matrix
mmap :: (a -> b) -> Matrix a -> Matrix b
mmap f (Matrix m) = Matrix $ P.map (P.map f) m

-- | Transpose a matrix
transpose :: Matrix a -> Matrix a
transpose = Matrix . DL.transpose . unmatrix

-- | Apply a binary function @f@ to two @Matrix@es
mbinary :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
mbinary f (Matrix m1) (Matrix m2) = Matrix $ P.zipWith veczip m1 m2
    where veczip = P.zipWith f

-- | Return the element at the given @(row, column)@ position
mindex :: Matrix a -> (Int, Int) -> a
mindex (Matrix m) (r, c) = (m !! r) !! c

-- | Concatenate two matrices such that their rows are now
-- concatenated
mconcatrows :: Matrix a -> Matrix a -> Matrix a
mconcatrows (Matrix m1) (Matrix m2) = Matrix $ P.zipWith (++) m1 m2

-- | Concatenate two matrices such that their columns are now
-- concatenated
mconcatcols :: Matrix a -> Matrix a -> Matrix a
mconcatcols (Matrix m1) (Matrix m2) = Matrix $ DL.transpose $ P.zipWith (++) (DL.transpose m1) (DL.transpose m2)

-- | Split a matrix into a list of vectors of its columns
toCols :: Matrix a -> [Vector a]
toCols (Matrix m) = P.map Vector $ DL.transpose m

-- | Form a matrix out of a list of vectors of its columns
fromCols :: [Vector a] -> Matrix a
fromCols vs = Matrix $ DL.transpose $ P.map toList vs

-- | Split a matrix into a list of vectors of its rows
toRows :: Matrix a -> [Vector a]
toRows (Matrix m) = P.map Vector m

-- | Form a matrix out of a list of vectors of its rows
fromRows :: [Vector a] -> Matrix a
fromRows vs = Matrix $ P.map toList vs

-- | Get the specified row vector from a matrix
mRow :: Matrix a -> Int -> Vector a
mRow (Matrix m) i = Vector $ m !! i

-- | Get the specified column vector from a matrix
mCol :: Matrix a -> Int -> Vector a
mCol (Matrix m) i = Vector $ (DL.transpose m) !! i

-- | Multiply a matrix by a column vector
matvec :: Num b => Matrix b -> Vector b -> Vector b
matvec (Matrix m) (Vector v) = Vector $ P.map (sum . zipWith (*) v) rows
    where rows = DL.transpose m

-- | Multiply a row vector by a matrix
vecmat :: Num b => Vector b -> Matrix b -> Vector b
vecmat (Vector v) (Matrix m) = Vector $ P.map (sum . zipWith (*) v) m

-- | Multiply two matrices
matmat :: Num b => Matrix b -> Matrix b -> Matrix b
matmat m1 (Matrix m2) = Matrix $ P.map (unvector . matvec m1 . Vector) m2

-- | Take the inner (dot) product of two vectors
inner :: Num b => Vector b -> Vector b -> b
inner (Vector v1) (Vector v2) = if P.length v1 == P.length v2
                                then sum $ zipWith (*) v1 v2
                                else error "Can't take the inner product of different-sized vectors!"

-- | Take the outer product of two vectors
outer :: Num b => Vector b -> Vector b -> Matrix b
outer (Vector v1) (Vector v2) = Matrix $ P.map (\x -> [x*v | v <- v1]) v2
