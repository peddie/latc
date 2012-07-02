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
-- Nested Data.Vector.Vectors as a matrix backend
-----------------------------------------------------------------------------

module Numeric.LATC.NestedVector (
                                -- * Newtype wrappers
                               Matrix
                                 -- * Structural functions on a @Matrix@
                               , fromLists
                               , toLists
                               , size
                               , mmap
                               , transpose
                                 -- * Structural functions between @Vector@s and @Matrix@es
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

import qualified Data.Vector as DV

-- | A matrix abstraction for nested lists
newtype Matrix a = Matrix {unmatrix :: DV.Vector (DV.Vector a)} deriving (Eq)

instance Show a => Show (Matrix a) where
    show (Matrix m) = "Matrix " ++ show m

-- | Convert nested lists into column-major nested vector matrices
fromLists :: [[a]] -> Matrix a
fromLists a = Matrix $ DV.fromList $ map DV.fromList a

-- | Convert column-major nested vector matrices into nested lists
toLists :: Matrix a -> [[a]]
toLists (Matrix a) = DV.toList $ DV.map DV.toList a

-- | Report how many rows and columns are in the matrix
size :: Matrix a -> (Int, Int)
size (Matrix a) = (DV.length a, DV.length $ DV.head a)

-- | Apply a function @f@ to all elements of the matrix
mmap :: (a -> b) -> Matrix a -> Matrix b
mmap f (Matrix a) = Matrix $ DV.map (DV.map f) a

-- | Transpose the matrix
transpose :: Matrix a -> Matrix a
transpose (Matrix a) = Matrix $ transpose' a

transpose' :: DV.Vector (DV.Vector a) -> DV.Vector (DV.Vector a)
transpose' v | DV.null v = v
             | DV.null (DV.head v) = transpose' $ DV.tail v
             | otherwise = DV.map DV.head v `DV.cons` transpose' (DV.map DV.tail v)

-- | Split a matrix into a list of vectors of its columns
toCols :: Matrix a -> [DV.Vector a]
toCols = DV.toList . unmatrix

-- | Form a matrix out of a list of vectors of its columns
fromCols :: [DV.Vector a] -> Matrix a
fromCols = Matrix . DV.fromList

-- | Split a matrix into a list of vectors of its rows
toRows :: Matrix a -> [DV.Vector a]
toRows (Matrix m) = DV.toList $ transpose' m

-- | Form a matrix out of a list of vectors of its rows
fromRows :: [DV.Vector a] -> Matrix a
fromRows = Matrix . transpose' . DV.fromList

-- | Multiply a matrix by a column vector
matvec :: Num b => Matrix b -> DV.Vector b -> DV.Vector b
matvec (Matrix m) v = DV.map (DV.sum . DV.zipWith (*) v) rows
    where rows = transpose' m

-- | Multiply a row vector by a matrix
vecmat :: Num b => DV.Vector b -> Matrix b -> DV.Vector b
vecmat v (Matrix m) = DV.map (DV.sum . DV.zipWith (*) v) m

-- | Multiply two matrices
matmat :: Num b => Matrix b -> Matrix b -> Matrix b
matmat m1 (Matrix m2) = Matrix $ DV.map (matvec m1) m2

-- | Take the inner (dot product) of two vectors
inner :: Num b => DV.Vector b -> DV.Vector b -> b
inner v1 v2 = DV.sum $ DV.zipWith (*) v1 v2

-- | Take the outer product of two vectors
outer :: Num b => DV.Vector b -> DV.Vector b -> Matrix b
outer v1 v2 = Matrix $ DV.map (\x -> DV.map(*x) v1) v2
