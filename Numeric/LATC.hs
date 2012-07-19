{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--
-- Regarding the Repa backend: there is a lot going on with Repa's
-- execution model, and I've bypassed it almost entirely here, just
-- for the sake of demonstrating that one can write correct and
-- well-typed programs that run on multiple backends.  
-----------------------------------------------------------------------------

module Numeric.LATC where

-- You have to separately import this to do anything with
-- ConstraintKinds.

import GHC.Prim (Constraint)

-- Data.Vector backend

import qualified Data.Vector as DV

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
import Data.Array.Repa ((:.)(..), Z(..), All(..))
import qualified Data.Array.Repa.Eval as DRE
import qualified Data.Array.Repa.Shape as DRS
import qualified Data.Array.Repa.Repr.Unboxed as DRU
import Data.Array.Repa.Repr.Unboxed (U)

-- Finally, hide Prelude stuff to make things less confusing.

import qualified Prelude as P
import Prelude (Num(..), ($), (.), Int, sqrt, Floating(..), (==), error, Functor, otherwise, Ord(..))
import qualified Data.List as DL

-------------
-- Classes --
-------------

-- | Here's the class for a @Vector@, an immutable, finite-length
-- collection of elements.
--
-- A note on the complex types: @VBox@ is an associated type synonym
-- (see @TypeFamilies@).  We declare it to have kind @Constraint@ (see
-- @ConstraintKinds@ and @KindSignatures@; @::@ here declares a kind,
-- because @VBox@ is a type), which informs the compiler that we
-- intend to use it like a typeclass constraint, even though it's not
-- a typeclass.  This ATS allows us to leave the element type out of
-- the class head (@Class Vector v where@) and still limit the types
-- we can use as elements, e.g. for the @HMatrix@ vectors, which
-- require their elements to be instances of @Storable@.  We also
-- declare a default @VBox@ type of @()@, which tells the compiler
-- that if we don't declare the type of @VBox@ when we define an
-- instance, then there are no constraints on the type of the element.
--
-- Minimal complete definition: @fromList@, @toList@.  The default
-- instance implementations simply convert to a list, perform the
-- operation and convert back, so you should expect them to be slow.
--
-- Where possible, we try to make default implementations using other
-- methods, e.g. @vconcat = foldl1' vappend@, so that a faster
-- @vappend@ will yield a faster @vconcat@ for free.  This also
-- happens in @fold@ functions -- if you implement @vfoldr@, you get a
-- native implementation of the other folds (although you can almost
-- always do better on your own).

type Vec v e = (Vector v, VBox v e)
class Vector v where
    type VBox v e :: Constraint
    type VBox v e = (Vector v)
    -- | Convert a list of elements into a vector
    fromList :: Vec v e => [e] -> v e
    -- | Convert a vector into a list of elements
    toList :: Vec v e => v e -> [e]
    -- | Return how many elements are in a vector
    length :: Vec v e => v e -> Int
    length v = P.length $ toList v
    -- | Apply a function to each element of a vector and return the
    -- results in a vector.
    vmap :: (Vec v e, Vec v f) => (e -> f) -> v e -> v f
    vmap f v = fromList $ P.map f $ toList v
    -- | Apply a binary operation, element-wise, to a pair of vectors,
    -- and return the results in a vector.
    vbinary :: (Vec v e, Vec v f, Vec v g) => (e -> f -> g) -> v e -> v f -> v g
    vbinary f a b = fromList $ P.zipWith f (toList a) (toList b)
    -- | Return the element at the given position within the vector:
    -- @vindex (fromList [22]) 0 == 22@
    vindex :: Vec v e => v e -> Int -> e
    vindex v i = (toList v) P.!! i
    -- | Append one vector to the end of another: @vappend (fromList
    -- [22,23]) (fromList [24, 25]) == fromList [22..25]@
    vappend :: Vec v e => v e -> v e -> v e
    vappend a b = fromList $ (toList a) P.++ (toList b)
    -- | Concatenate a finite list of vectors together end-to-end.
    vconcat :: Vec v e => [v e] -> v e
    vconcat = DL.foldl1' vappend
    
    -- Functional usage
    -- | The first element in a vector
    vhead :: Vec v e => v e -> e
    vhead = P.head . toList
    -- | The last element in a vector
    vlast :: Vec v e => v e -> e
    vlast = P.last . toList
    -- | The tail of a vector
    vtail :: Vec v e => v e -> v e
    vtail = fromList . P.tail . toList
    -- | All the elements but the last of a vector
    vinit :: Vec v e => v e -> v e
    vinit = fromList . DL.init . toList
    -- | Reverse a vector
    vreverse :: Vec v e => v e -> v e
    vreverse = fromList . P.reverse . toList
    -- | Left fold across a vector
    vfoldl :: Vec v e => (a -> e -> a) -> a -> v e -> a
    vfoldl f i v = vfoldr step P.id v i
        where step x g a = g (f a x)
    -- | Left fold across a vector using the head of the vector as the
    -- initial element
    vfoldl1 :: Vec v e => (e -> e -> e) -> v e -> e
    vfoldl1 f v = vfoldl f (vhead v) (vtail v)
    -- | Right fold across a vector
    vfoldr :: Vec v e => (e -> a -> a) -> a -> v e -> a
    vfoldr f i = DL.foldr f i . toList
    -- | Right fold across a vector using the head of the vector as
    -- the initial element
    vfoldr1 :: Vec v e => (e -> e -> e) -> v e -> e
    vfoldr1 f v = vfoldr f (vhead v) (vtail v)

-- | Matrix works similarly to Vector.
-- 
-- Minimal complete definition: @fromLists@, @toLists@.  The default
-- instance implementations simply convert to a nested list, perform
-- the operation and convert back, so you should expect them to be
-- slow.
--
-- Where possible, we try to make default implementations using other
-- methods, e.g. @mconcatrows = foldl1' mappendrows@, so that a faster
-- @mappend@ will yield a faster @mconcat@ for free.  
--
-- This also happens in @fold@ functions -- if you define @vfoldr@,
-- you get a native implementation of the rest for free.
type Mat m e = (Matrix m, MBox m e)
class Matrix m where
    type MBox m e :: Constraint
    type MBox m e = (Matrix m)
    -- | Convert a row-major nested list of elements into a matrix.
    fromLists :: Mat m e => [[e]] -> m e
    -- | Convert a matrix into a row-major nested list of elements.
    toLists :: Mat m e => m e -> [[e]]
    -- | Return how many (rows, columns) are in a matrix.
    size :: Mat m e => m e -> (Int, Int)
    size m = (P.length a, case P.take 1 a of
                            [] -> 0
                            x -> P.length x)
        where a = toLists m
    -- | Apply a function to each element of a matrix and return the
    -- results in a matrix.
    mmap :: (Mat m e, Mat m f) => (e -> f) -> m e -> m f
    -- I'd love to give this default in terms of a fold, but it's in a
    -- different class because it needs an associated Vector type.
    mmap f m = fromLists $ P.map (P.map f) $ toLists m
    -- | Transpose a matrix.
    transpose :: Mat m e => m e -> m e
    transpose = fromLists . DL.transpose . toLists
    -- | Apply a binary operation, element-wise, to a pair of
    -- matrices, and return the results in a new matrix.
    mbinary :: (Mat m e, Mat m f, Mat m g) => (e -> f -> g) -> m e -> m f -> m g
    mbinary f a b = fromLists $ P.zipWith (P.zipWith f) (toLists a) (toLists b)
    -- | Return the element at the given position within the matrix:
    -- @mindex (fromLists [[22]]) (0, 0) == 22@
    mindex :: Mat m e => m e -> (Int, Int) -> e
    mindex m (r, c) = (toLists m P.!! r) P.!! c
    -- | Append one matrix to another so that their rows are now
    -- concatenated (i.e. left-to-right).
    mappendrows :: Mat m e => m e -> m e -> m e
    mappendrows a b = fromLists $ P.zipWith (P.++) a' b'
        where (a', b') = (toLists a, toLists b)
    -- | Append one matrix to another so that their columns are now
    -- concatenated (i.e. top-to-bottom).
    mappendcols :: Mat m e => m e -> m e -> m e
    mappendcols a b = fromLists . DL.transpose $ P.zipWith (P.++) a' b'
        where (a', b') = (DL.transpose $ toLists a, DL.transpose $ toLists b)
    -- | Concatenate a list of matrices left-to-right so that their
    -- rows are now concatenated.
    mconcatrows :: Mat m e => [m e] -> m e
    mconcatrows = DL.foldl1' mappendrows
    -- | Concatenate a list of matrices top-to-bottom so that their
    -- columns are now concatenated.
    mconcatcols :: Mat m e => [m e] -> m e
    mconcatcols = DL.foldl1' mappendcols
    
    -- Functional usage of matrices
    -- | The row-wise tail of a matrix (i.e. a matrix of all rows but
    -- the first)
    mtailr :: Mat m e => m e -> m e
    mtailr = fromLists . P.tail . toLists
    -- | The row-wise init of a matrix (i.e. a matrix of all rows but
    -- the last)
    minitr :: Mat m e => m e -> m e
    minitr = fromLists . DL.init . toLists
    -- | The column-wise tail of a matrix (i.e. a matrix of all
    -- columns but the first)
    mtailc :: Mat m e => m e -> m e
    mtailc = fromLists . P.tail . DL.transpose . toLists
    -- | The column-wise init of a matrix (i.e. a matrix of all
    -- columns but the last)
    minitc :: Mat m e => m e -> m e
    minitc = fromLists . DL.init . DL.transpose . toLists
    -- | Reverse the order of rows within a matrix (equivalent to
    -- reversing all the column vectors)
    mreverser :: Mat m e => m e -> m e
    mreverser = fromLists . P.reverse . toLists
    -- | Reverse the order of columns within a matrix (equivalent to
    -- reversing all the row vectors)
    mreversec :: Mat m e => m e -> m e
    mreversec = fromLists . DL.transpose . P.reverse . DL.transpose . toLists
                   
-- | Related types for matrices and vectors.  These methods involve
-- both structures, but they make no additional demands on the type of
-- the element.  
--
-- Minimal complete definition: none!  Also, for folds, a minimal
-- native implementation is @mfoldrr@.  (Of course, you can probably
-- do better by doing them all natively yourself.)
type MatVec m v e = (Matrix m, Vector v, MVBox m v e)
class (Matrix m, Vector v) => MV m v where
    type MVBox m v e :: Constraint
    type MVBox m v e = (MBox m e, VBox v e)
    -- | Form a matrix from a list of row vectors.
    fromRows :: MatVec m v e => [v e] -> m e
    -- We must give a type signature for the default method
    -- implementation that specifies all the proper constraints.
    -- Without this, someone could override the default associated
    -- type definition, and these default implementations might have
    -- incorrect type constraints (i.e. might not be applicable).
    default fromRows :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => [v e] -> m e
    fromRows = fromLists . (P.map toList)
    -- | Split a matrix into a list of row vectors.
    toRows :: MatVec m v e => m e -> [v e]
    default toRows :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> [v e]
    toRows = P.map fromList . toLists
    -- | Return the row vector at the specified index within the
    -- matrix: @mRow (fromLists [[22]]) 0 == fromList [22]@
    mRow :: MatVec m v e => m e -> Int -> v e
    default mRow :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> Int -> v e
    mRow m i = fromList $ (toLists m) P.!! i
    -- | Form a matrix from a list of column vectors.
    fromCols :: MatVec m v e => [v e] -> m e
    default fromCols :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => [v e] -> m e
    fromCols = fromLists . DL.transpose . P.map toList
    -- | Split a matrix into a list of column vectors.
    toCols :: MatVec m v e => m e -> [v e]
    default toCols :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> [v e]
    toCols = P.map fromList . DL.transpose . toLists
    -- | Return the column vector at the specified index within the
    -- matrix: @mCol (fromLists [[22]]) 0 == fromList [22]@
    mCol :: MatVec m v e => m e -> Int -> v e
    default mCol :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> Int -> v e
    mCol m i = fromList $ (DL.transpose $ toLists m) P.!! i

    -- Functional usage of matrices -- folds etc. depend on a related
    -- @Vector@ type

    -- | The first row of a matrix
    mheadr :: MatVec m v e => m e -> v e
    default mheadr :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> v e    
    mheadr = fromList . P.head . toLists
    -- | The last row of a matrix
    mlastr :: MatVec m v e => m e -> v e
    default mlastr :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> v e
    mlastr = fromList . P.last . toLists
    -- | The first column of a matrix
    mheadc :: MatVec m v e => m e -> v e
    default mheadc :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> v e
    mheadc = fromList . P.head . DL.transpose . toLists
    -- | The last column of a matrix
    mlastc :: MatVec m v e => m e -> v e
    default mlastc :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => m e -> v e
    mlastc = fromList . P.last . DL.transpose . toLists
    -- | Left fold across the row vectors (i.e. top to bottom) of a
    -- matrix
    mfoldlr :: MatVec m v e => (a -> v e -> a) -> a -> m e -> a
    default mfoldlr :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (a -> v e -> a) -> a -> m e -> a
    -- mfoldlr f i = DL.foldl' (\a m -> f a $ fromList m) i . toLists
    mfoldlr f i m = mfoldrr step P.id m i
        where step x g a = g (f a x)
    -- | Left fold across the row vectors (i.e. top to bottom) of a
    -- matrix, using the first row vector as an initial argument
    mfoldl1r :: MatVec m v e => (v e -> v e -> v e) -> m e -> v e
    default mfoldl1r :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (v e -> v e -> v e) -> m e -> v e
    mfoldl1r f m = mfoldlr f (mheadr m) (mtailr m)
    -- | Left fold across the column vectors (i.e. left to right) of a
    -- matrix
    mfoldlc :: MatVec m v e => (a -> v e -> a) -> a -> m e -> a
    default mfoldlc :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (a -> v e -> a) -> a -> m e -> a
    mfoldlc f i m = mfoldlr f i $ transpose m
    -- | Left fold across the column vectors (i.e. left to right) of a
    -- matrix, using the first column vector as an initial argument
    mfoldl1c :: MatVec m v e => (v e -> v e -> v e) -> m e -> v e
    default mfoldl1c :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (v e -> v e -> v e) -> m e -> v e
    mfoldl1c f m = mfoldlc f (mheadr m) (mtailr m)
    -- | Right fold across the row vectors (i.e. top to bottom) of a
    -- matrix
    mfoldrr :: MatVec m v e => (v e -> a -> a) -> a -> m e -> a
    default mfoldrr :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (v e -> a -> a) -> a -> m e -> a
    mfoldrr f i = DL.foldr (\v a -> f (fromList v) a) i . toLists
    -- | Right fold across the row vectors (i.e. top to bottom) of a
    -- matrix, using the first row vector as an initial argument
    mfoldr1r :: MatVec m v e => (v e -> v e -> v e) -> m e -> v e
    default mfoldr1r :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (v e -> v e -> v e) -> m e -> v e
    mfoldr1r f m = mfoldrr f (mheadr m) (mtailr m)
    -- | Right fold across the column vectors (i.e. left to right) of a
    -- matrix
    mfoldrc :: MatVec m v e => (v e -> a -> a) -> a -> m e -> a
    default mfoldrc :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (v e -> a -> a) -> a -> m e -> a
    mfoldrc f i m = mfoldrr f i $ transpose m
    -- | Right fold across the column vectors (i.e. left to right) of a
    -- matrix, using the first column vector as an initial argument
    mfoldr1c :: MatVec m v e => (v e -> v e -> v e) -> m e -> v e
    default mfoldr1c :: (Matrix m, Vector v, MBox m e, VBox v e, MVBox m v e) => (v e -> v e -> v e) -> m e -> v e
    mfoldr1c f m = mfoldrc f (mheadr m) (mtailr m)

-- | Linear algebra on matrices and vectors.  These methods involve
-- both structures, and they require operands to have numeric
-- elements.  
--
-- Minimal complete definition: none!

type LA m v e = (Vec v e, Mat m e, MatVec m v e, LinAlgBox m v e)
class MV m v => LinAlg m v where
    type LinAlgBox m v e :: Constraint 
    type LinAlgBox m v e = (MBox m e, VBox v e, Num e)
    -- | Multiply a matrix by a column vector.
    mv :: LA m v e => m e -> v e -> v e
    default mv :: (Num e, LA m v e) => m e -> v e -> v e
    mv m v = fromList $ P.map (P.sum . P.zipWith (*) (toList v)) $ toLists m
    -- | Multiply a row vector by a matrix.
    vm :: LA m v e => v e -> m e -> v e
    default vm :: (Num e, LA m v e) => v e -> m e -> v e
    vm v m = fromList $ P.map (P.sum . P.zipWith (*) (toList v)) $ DL.transpose $ toLists m
    -- | Multiply two matrices.
    mm :: LA m v e => m e -> m e -> m e
    -- jesus flying spaghetti monster christ
    default mm :: (Num e, LA m v e) => m e -> m e -> m e
    mm (a :: m e) (b :: m e) = fromCols $ P.map (mv a) $ (toCols b :: (Num e, LA m v e) => [v e])

    -- | Form a matrix by the outer product of two vectors.
    outer :: LA m v e => v e -> v e -> m e
    default outer :: (Num e, LA m v e) => v e -> v e -> m e
    outer a b = fromLists $ P.map (\x -> [x*v | v <- (toList a)]) (toList b)
    -- | Compute the inner (dot) product of two vectors.
    inner :: LA m v e => v e -> v e -> e
    default inner :: (Num e, LA m v e) => v e -> v e -> e
    inner a b = P.sum $ P.zipWith (*) (toList a) (toList b)

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

-------------------
-- WARNING WARNING WARNING
-- 
-- You have to explicitly give the associated data type definition for
-- EVERY INSTANCE, even though all the classes have sane defaults.  I
-- think this is a GHC bug.

-- Below are instances for existing backends.  I'm trying to avoid
-- orphan instances, but these would be a lot nicer in a different
-- module, one for each backend.

-- | @Data.Vector@ instance
instance Vector DV.Vector where
    type VBox DV.Vector e = ()
    fromList = DV.fromList
    toList = DV.toList
    length = DV.length
    vmap = DV.map
    vbinary = DV.zipWith
    vindex = (DV.!)
    vappend = (DV.++)

-- | HMatrix (@Data.Packed.Vector@) instance
instance Vector PV.Vector where
    type VBox PV.Vector b = (Vector PV.Vector, Storable b)
    fromList = PV.fromList
    toList = PV.toList
    length = PV.dim
    vmap = PV.mapVector
    vbinary = PV.zipVectorWith
    vindex = (PV.@>)
    vappend a b = PV.join [a, b]

-- | HMatrix (@Data.Packed.Matrix@) instance
instance Matrix PM.Matrix where
    type MBox PM.Matrix b = (Matrix PM.Matrix, PM.Element b)
    fromLists = PM.fromLists
    toLists = PM.toLists
    size a = (PM.rows a, PM.cols a)
    mmap = PM.mapMatrix
    transpose = PM.trans
    mbinary = PM.liftMatrix2 . vbinary
    mindex = (PM.@@>)
    mappendrows a b = PM.fromBlocks [[a, b]]
    mappendcols a b = PM.fromBlocks [[a], [b]]

-- | HMatrix instance.  The nice part about this approach is how the
-- default constraint type for @MVBox@ simply intersects the @MBox@
-- and @VBox@ constraint types for the declared matrix and vector
-- instances, so we don't have to declare anything in spite of
-- HMatrix's annoying shit.
instance MV PM.Matrix PV.Vector where
    fromRows = PM.fromRows
    toRows = PM.toRows
    mRow m i = (PM.toRows m) P.!! i
    fromCols = PM.fromColumns
    toCols = PM.toColumns
    mCol m i = (PM.toColumns m) P.!! i

-- | HMatrix instance.  We weren't as lucky here as with @MV@.
-- Although the default @LinAlgBox@ type requires the element to be a
-- @Num@ instance, we have to add our own constraint for @NC.Product@,
-- because as usual, HMatrix decided to add some more random-ass
-- typeclass constraints.
instance LinAlg PM.Matrix PV.Vector where
    type LinAlgBox PM.Matrix PV.Vector e = (MBox PM.Matrix e, VBox PV.Vector e, Num e, NC.Product e)
    mv = NC.mXv
    vm = NC.vXm
    mm = NC.mXm
    inner = (NC.<.>)
    outer = NC.outer

-- | Minimal Repa instance.
instance Vector (DR.Array r DR.DIM1) where
    type VBox (DR.Array r DR.DIM1) e = (Vector (DR.Array r DR.DIM1), DRE.Target r e, DR.Source r e)
    fromList l = DRE.fromList (DRS.shapeOfList [P.length l]) l
    toList = DR.toList

-- | Minimal Repa instance.
instance Matrix (DR.Array r DR.DIM2) where
    type MBox (DR.Array r DR.DIM2) e = (Matrix (DR.Array r DR.DIM2), DRE.Target r e, DR.Source r e)
    fromLists ls = DRE.fromList (DRS.shapeOfList [r, c]) $ P.concat ls
        where (r, c) = (P.length ls, case P.take 1 ls of
                                       [] -> 0
                                       x -> P.length x)
    toLists m = P.reverse $ grp c [] fl
        where grp _ acc [] = acc
              grp n acc xx | P.length xx > n = grp n (xh : acc) xt
                           | P.length xx == n = xx : acc
                           | otherwise = error "Serious problem in list conversion with the LATC Repa backend."
                  where (xh, xt) = DL.splitAt n xx
              fl = DR.toList m
              (_, c) = case DRS.listOfShape $ DR.extent $ m of
                         [x, y] -> (x, y)
                         _ -> error "Serious problem in shape handing with the LATC Repa backend."


-- | Minimal Repa instance.
instance MV (DR.Array r DR.DIM2) (DR.Array r DR.DIM1) where

-- | Minimal Repa instance, for only @Unboxed@ arrays.  I have to
-- provide two method implementations for these; when you have
-- something other than the default for the associated type synonym,
-- these methods' default implementations are rejected by the type
-- checker, because they use only @m@ or only @v@ in the type.
instance LinAlg (DR.Array U DR.DIM2) (DR.Array U DR.DIM1) where
    type LinAlgBox (DR.Array U DR.DIM2) (DR.Array U DR.DIM1) e = (Num e, DRU.Unbox e, DRE.Elt e)
    inner a b = DR.sumAllS $ DR.zipWith (+) a b
    mm a b = DR.sumS (DR.zipWith (*) aRepl bRepl)
        where
          t = DR.transpose b
          aRepl = DR.extend (Z :. All :. colsB :. All) a
          bRepl = DR.extend (Z :. rowsA :. All :. All) t
          (Z :._ :.rowsA) = DR.extent a
          (Z :.colsB :._) = DR.extent b

