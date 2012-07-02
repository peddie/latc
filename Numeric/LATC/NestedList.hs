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
                                -- ** Structural functions on a @Matrix@
                               , fromLists
                               , toLists
                               , size
                               , mmap
                               , transpose
                                -- ** Structural functions between @Vector@s and @Matrix@es
                               , toRows
                               , fromRows
                               , toCols
                               , fromCols
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

-- | A vector abstraction for lists
newtype Vector a = Vector {unvector :: [a]} deriving (Eq)

-- | A matrix abstraction for nested lists
newtype Matrix a = Matrix {unmatrix :: [[a]]} deriving (Eq)

instance Show a => Show (Matrix a) where
    show (Matrix m) = "Matrix " ++ show m

instance Show a => Show (Vector a) where
    show (Vector v) = "Vector " ++ show v

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

-- | Convert a list of lists to a @Matrix@ (column-major)
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

-- | Split a matrix into a list of vectors of its columns
toCols :: Matrix a -> [Vector a]
toCols (Matrix m) = P.map Vector m

-- | Form a matrix out of a list of vectors of its columns
fromCols :: [Vector a] -> Matrix a
fromCols vs = Matrix $ P.map toList vs

-- | Split a matrix into a list of vectors of its rows
toRows :: Matrix a -> [Vector a]
toRows (Matrix m) = P.map Vector $ DL.transpose m

-- | Form a matrix out of a list of vectors of its rows
fromRows :: [Vector a] -> Matrix a
fromRows vs = Matrix $ DL.transpose $ P.map toList vs

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
