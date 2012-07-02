{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LATC.SparseIntMap
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file latc/LICENSE)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- IntMaps as a (sparse) linear algebra backend
-----------------------------------------------------------------------------

module Numeric.LATC.SparseIntMap (
                                -- * Newtype wrappers
                                Vector
                               , Matrix
                                -- * Structural functions
                                -- ** Structural functions on a @Vector@
--                                , fromList
--                                , toList
--                                , fromSparseList
--                                , toSparseList
--                                , length
--                                , map
--                                 -- ** Structural functions on a @Matrix@
--                                , fromLists
--                                , toLists
--                                , fromSparseLists
--                                , toSparseLists
--                                , size
--                                , mmap
--                                , transpose
--                                 -- ** Structural functions between @Vector@s and @Matrix@es
--                                , toRows
--                                , fromRows
--                                , toCols
--                                , fromCols
--                                 -- * Math functions
--                                , matvec
--                                , vecmat
--                                , matmat
--                                , inner
--                                , outer
                               ) where

import Prelude hiding (length, map)
import qualified Prelude as P
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IM

data Vector a = Vector { unvector :: IntMap a
                       , vlen :: Int
                       } deriving (Eq)

data Matrix a = Matrix { unmatrix :: IntMap (IntMap a) 
                       , msize :: (Int, Int)
                       } deriving (Eq)

instance Show a => Show (Vector a) where
    show (Vector l v) = "Vector<" ++ show l ++ "> " ++ show v

instance Show a => Show (Matrix a) where
    show (Matrix m) = "Matrix " ++ show m

unsparse :: a -> Int -> IM.IntMap a -> [a]
unsparse def size im = P.map (\k -> IM.findWithDefault def k im) [0..size]

-- | Convert a list to a @Vector@
fromList :: [b] -> Vector b
fromList l = Vector (IM.fromList $ (zip [0..] l)) (P.length l)

-- | Convert a @Vector@ to a list
toList :: Vector b -> [b]
toList (Vector l v) = if l == IM.size v
                      then IM.toList v
                      else zip (unsparse 0 

-- | Convert a list of pairs @(index, value)@ to a @Vector@
fromSparseList :: [(Int, b)] -> Vector b
fromSparseList = Vector . IM.fromList

-- | Convert a  @Vector@ to a list of pairs @(index, value)@
toSparseList :: Vector b -> [(Int, b)]
toSparseList = IM.toList . unvector

