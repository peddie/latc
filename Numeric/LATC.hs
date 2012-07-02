{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LATC.LA
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file latc/LICENSE)
-- 
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- Type classes for linear algebra -- flexible backends.
-----------------------------------------------------------------------------

module Numeric.LATC where

-- You have to separately import this to do anything with
-- ConstraintKinds.

import GHC.Prim (Constraint)

-- Import list instances

import qualified Numeric.LATC.NestedList as NL

-- Data.Vector is another alternative instance

import qualified Data.Vector as DV
import qualified Numeric.LATC.NestedVector as NV

-- Get HMatrix stuff.

import qualified Data.Packed.Vector as PV
import qualified Data.Packed.Matrix as PM
import qualified Numeric.Container as NC
import Foreign.Storable (Storable)

-- IntMaps for making sparse vectors and matrices

import qualified Data.IntMap as IM -- Can't wait for containers-0.5
                                   -- to work with everything else!

-- Repa arrays for maximum parallel

import qualified Data.Array.Repa as DR

-- Finally, hide Prelude stuff to make things less confusing.

import qualified Prelude as P
import Prelude (Num(..), ($), (.), Int, sqrt, Floating(..), (==), error, Functor)

-------------
-- Classes --
-------------

-- | Here's the class for a Vector.  VBox is an associated type
-- synonym (see Type Families).  We declare it to have kind Constraint
-- (see ConstraintKinds and KindSignatures; ``::'' here declares a
-- kind, because VBox is a type), which informs the compiler that we
-- intend to use it like a typeclass constraint, even though it's not
-- a typeclass.  This ATS allows us to leave the element type out of
-- the class head (``Class Vector v where'') and still limit the types
-- we can use as elements, e.g. for the HMatrix vectors, which require
-- their elements to be instances of Storable.  We also declare a
-- default VBox type of (), which tells the compiler that if we don't
-- declare the type of VBox when we define an instance, then there are
-- no constraints on the type of the element.

type Vec v e = (Vector v, VBox v e)
class Vector v where
    type VBox v e :: Constraint
    type VBox v e = (Vector v)
    fromList :: Vec v e => [e] -> v e
    toList :: Vec v e => v e -> [e]
    length :: Vec v e => v e -> Int
    map :: (Vec v e, Vec v f) => (e -> f) -> v e -> v f
    vbinary :: (Vec v e, Vec v f, Vec v g) => (e -> f -> g) -> v e -> v f -> v g
    vindex :: Vec v e => v e -> Int -> e
    vconcat :: Vec v e => v e -> v e -> v e

-- | Matrix works similarly to Vector.

type Mat m e = (Matrix m, MBox m e)
class Matrix m where
    type MBox m e :: Constraint
    type MBox m e = (Matrix m)
    fromLists :: Mat m e => [[e]] -> m e
    toLists :: Mat m e => m e -> [[e]]
    size :: Mat m e => m e -> (Int, Int)
    mmap :: (Mat m e, Mat m f) => (e -> f) -> m e -> m f
    transpose :: Mat m e => m e -> m e
    mbinary :: (Mat m e, Mat m f, Mat m g) => (e -> f -> g) -> m e -> m f -> m g
    mindex :: Mat m e => m e -> (Int, Int) -> e
    mconcatrows :: Mat m e => m e -> m e -> m e
    mconcatcols :: Mat m e => m e -> m e -> m e

-- | Related types for matrices and vectors.  These methods involve
-- both structures, but they make no additional demands on the type of
-- the element.

type MatVec m v e = (Matrix m, Vector v, MVBox m v e)
class (Matrix m, Vector v) => MV m v where
    type MVBox m v e :: Constraint
    type MVBox m v e = (MBox m e, VBox v e)
    fromRows :: MatVec m v e => [v e] -> m e
    toRows :: MatVec m v e => m e -> [v e]
    fromCols :: MatVec m v e => [v e] -> m e
    toCols :: MatVec m v e => m e -> [v e]
    mCol :: MatVec m v e => m e -> Int -> v e
    mRow :: MatVec m v e => m e -> Int -> v e

-- | Linear algebra on matrices and vectors.  These methods involve
-- both structures, and they require operands to have numeric
-- elements.

type LA m v e = (Vec v e, Mat m e, MatVec m v e, LinAlgBox m v e)
class MV m v => LinAlg m v where
    type LinAlgBox m v e :: Constraint 
    type LinAlgBox m v e = (MBox m e, VBox v e, Num e)
    mv :: LA m v e => m e -> v e -> v e
    vm :: LA m v e => v e -> m e -> v e
    mm :: LA m v e => m e -> m e -> m e
    outer :: LA m v e => v e -> v e -> m e
    inner :: LA m v e => v e -> v e -> e

-- | Sparse vectors.  A sparse vector backend must be an instance of
-- Vector as well; this class simply provides additional methods
-- for construction and destruction.

type SVec v e = (SVBox v e, SVector v)
class Vector v => SVector v where
    type SVBox v e :: Constraint
    type SVBox v e = (Vector v, SVector v)
    svFromList :: SVec v e => [(e, Int)] -> Int -> v e
    svToList :: SVec v e => v e -> [(e, Int)]

-- | Sparse matrices.  A sparse matrix backend must be an instance of
-- Matrix as well; this class simply provides additional methods
-- for construction and destruction.

type SMat m e = (SMBox m e, SMatrix m)
class Matrix m => SMatrix m where
    type SMBox m e :: Constraint
    type SMBox m e = (Matrix m, SMatrix m)
    smFromList :: SMat m e => [(e, Int, Int)] -> Int -> m e
    smToList :: SMat m e => m e -> [(e, Int, Int)]

---------------
-- Instances --
---------------

-- List instances

-------------------
-- WARNING WARNING WARNING
-- 
-- You have to explicitly give the associated data type definition for
-- EVERY INSTANCE, even though all the classes have sane defaults.  I
-- think this is a GHC bug.

instance Vector NL.Vector where
    type VBox NL.Vector e = ()
    fromList = NL.fromList
    toList = NL.toList
    length = NL.length
    map = NL.map

instance Matrix NL.Matrix where
    type MBox NL.Matrix e = ()
    fromLists = NL.fromLists
    toLists = NL.toLists
    size = NL.size
    mmap = NL.mmap
    transpose = NL.transpose

instance MV NL.Matrix NL.Vector where
    type MVBox NL.Matrix NL.Vector e = ()
    fromCols = NL.fromCols
    toCols = NL.toCols
    fromRows = NL.fromRows
    toRows = NL.toRows

instance LinAlg NL.Matrix NL.Vector where
    type LinAlgBox NL.Matrix NL.Vector e = Num e 
    mv = NL.matvec
    vm = NL.vecmat
    mm = NL.matmat
    inner = NL.inner
    outer = NL.outer

-- Data.Vector instances

-- I have the same problem with nesting Data.Vector.Vector to form a
-- matrix.

-- type NestedVector = DV.Vector :. DV.Vector
-- instance Matrix NestedVector where

instance Vector DV.Vector where
    type VBox DV.Vector e = ()
    fromList = DV.fromList
    toList = DV.toList
    length = DV.length
    map = DV.map

instance Matrix NV.Matrix where
    type MBox NV.Matrix e = ()
    fromLists = NV.fromLists
    toLists = NV.toLists
    size = NV.size
    mmap = NV.mmap
    transpose = NV.transpose

instance MV NV.Matrix DV.Vector where
    type MVBox NV.Matrix DV.Vector e = ()
    fromCols = NV.fromCols
    toCols = NV.toCols
    fromRows = NV.fromRows
    toRows = NV.toRows

instance LinAlg NV.Matrix DV.Vector where
    type LinAlgBox NV.Matrix DV.Vector e = Num e 
    mv = NV.matvec
    vm = NV.vecmat
    mm = NV.matmat
    inner = NV.inner
    outer = NV.outer

-- HMatrix instances

-- All the HMatrix instances work without much fuss.

instance Vector PV.Vector where
    type VBox PV.Vector b = (Vector PV.Vector, Storable b)
    fromList = PV.fromList
    toList = PV.toList
    length = PV.dim
    map = PV.mapVector

instance Matrix PM.Matrix where
    type MBox PM.Matrix b = (Matrix PM.Matrix, PM.Element b)
    fromLists = PM.fromLists
    toLists = PM.toLists
    size a = (PM.rows a, PM.cols a)
    mmap = PM.mapMatrix
    transpose = PM.trans

-- | The nice part about this approach is how the default constraint
-- type for MVBox simply intersects the MBox and VBox constraint types
-- for the declared matrix and vector instances, so we don't have to
-- declare anything in spite of HMatrix's annoying shit.
instance MV PM.Matrix PV.Vector where
    fromRows = PM.fromRows
    toRows = PM.toRows
    fromCols = PM.fromColumns
    toCols = PM.toColumns

-- | We weren't as lucky here as with MV.  Although the default
-- LinAlgBox type requires the element to be a Num instance, we have
-- to add our own constraint for NC.Product, because as usual, HMatrix
-- decided to add some more random-ass typeclass constraints.
instance LinAlg PM.Matrix PV.Vector where
    type LinAlgBox PM.Matrix PV.Vector e = (MBox PM.Matrix e, VBox PV.Vector e, Num e, NC.Product e)
    mv = NC.mXv
    vm = NC.vXm
    mm = NC.mXm
    inner = (NC.<.>)
    outer = NC.outer
