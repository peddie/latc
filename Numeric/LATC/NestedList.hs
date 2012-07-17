{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

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
                                -- ** Structural functions on @Vector@s
                               , fromList
                               , toList
                               , length
                               , vmap
                               , vbinary
                               , vindex
                               , vappend
                               , vconcat
                               , vlift
                               , vlift2
                                -- ** Structural functions on @Matrix@es
                               , fromLists
                               , toLists
                               , size
                               , mmap
                               , transpose
                               , mbinary
                               , mindex
                               , mappendrows
                               , mappendcols
                               , mconcatrows
                               , mconcatcols
                               , mlift
                               , mlift2
                               , mliftr
                               , mliftc
                               , mlift2r
                               , mlift2c
                                -- ** Structural functions between @Vector@s and @Matrix@es
                               , toRows
                               , fromRows
                               , toCols
                               , fromCols
                               , mCol
                               , mRow
                                -- * Functional operations
                                -- ** Functional operations on @Vector@s
                               , vhead
                               , vlast
                               , vtail
                               , vinit
                               , vreverse
                               , vfoldl
                               , vfoldl1
                               , vfoldr
                               , vfoldr1
                                -- ** Functional operations on @Matrix@es
                               , mtailr
                               , minitr
                               , mtailc
                               , minitc
                               , mreverser
                               , mreversec
                                -- ** Functional operations between @Vector@s and @Matrix@es
                               , mheadr
                               , mlastr
                               , mheadc
                               , mlastc
                               , mfoldlr
                               , mfoldl1r
                               , mfoldlc
                               , mfoldl1c
                               , mfoldrr
                               , mfoldr1r
                               , mfoldrc
                               , mfoldr1c
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

-- imports for instances
import Data.Hashable ( Hashable(..) )
import Data.Data (Data(..), Typeable(..))
import GHC.Generics (Generic(..))
import Control.Monad (ap, liftM)
import Control.Applicative (Applicative(..))
import Data.Foldable (Foldable(..))
import Test.QuickCheck (Arbitrary(..), listOf, vectorOf, choose)

import qualified Numeric.LATC as LATC

-- | A vector abstraction for lists
newtype Vector a = Vector {unvector :: [a]} deriving (Eq, Ord, Data, Typeable, Generic)

instance Show a => Show (Vector a) where
    show (Vector v) = "Vector " ++ show v

instance Hashable a => Hashable (Vector a) where
    hash = hash

-- | All mathematical instances for Vector are element-wise.
instance Num a => Num (Vector a) where
    (+) = vbinary (+)
    (*) = vbinary (*)
    (-) = vbinary (-)

    abs = vmap abs
    signum = vmap signum

instance Fractional a => Fractional (Vector a) where
    (/) = vbinary (/)

instance Floating a => Floating (Vector a) where
    exp = vmap exp
    sqrt = vmap sqrt
    log = vmap log
    (**) = vbinary (**)
    logBase = vbinary logBase
    sin = vmap sin
    tan = vmap tan
    cos = vmap cos
    asin = vmap asin
    atan = vmap atan
    acos = vmap acos
    sinh = vmap sinh
    tanh = vmap tanh
    cosh = vmap cosh
    asinh = vmap asinh
    atanh = vmap atanh
    acosh = vmap acosh

instance Functor Vector where
    fmap = vmap

instance Applicative Vector where
    pure = return
    (<*>) = ap

instance Monad Vector where
    return = fromList . return
    x >>= f = vconcat $ toList $ vmap f x

instance Foldable Vector where
    foldr = vfoldr
    foldl = vfoldl

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = liftM fromList $ listOf $ arbitrary

-- | A matrix abstraction for nested lists
newtype Matrix a = Matrix {unmatrix :: [[a]]} deriving (Eq, Ord, Data, Typeable, Generic)

instance Show a => Show (Matrix a) where
    show (Matrix m) = "Matrix " ++ show m

instance Hashable a => Hashable (Matrix a) where
    hash = hash

-- | All mathematical instances for Matrix are element-wise.
instance Num a => Num (Matrix a) where
    (+) = mbinary (+)
    (*) = mbinary (*)
    (-) = mbinary (-)

    abs = mmap abs
    signum = mmap signum

instance Fractional a => Fractional (Matrix a) where
    (/) = mbinary (/)

instance Floating a => Floating (Matrix a) where
    exp = mmap exp
    sqrt = mmap sqrt
    log = mmap log
    (**) = mbinary (**)
    logBase = mbinary logBase
    sin = mmap sin
    tan = mmap tan
    cos = mmap cos
    asin = mmap asin
    atan = mmap atan
    acos = mmap acos
    sinh = mmap sinh
    tanh = mmap tanh
    cosh = mmap cosh
    asinh = mmap asinh
    atanh = mmap atanh
    acosh = mmap acosh

instance Functor Matrix where
    fmap = mmap

instance Applicative Matrix where
    pure = return
    (<*>) = ap

instance Monad Matrix where
    return = fromLists . return . return
    x >>= f = mconcatcols $ DL.map mconcatrows $ toLists $ mmap f x

instance Arbitrary a => Arbitrary (Matrix a) where
    arbitrary = do r <- choose (0, 100)
                   c <- choose (0, 100)
                   l <- vectorOf (r*c) arbitrary
                   return $ fromLists l

-- Should a Foldable instance for Matrix use a column fold or a row
-- fold?

-- | LATC instances
instance LATC.Vector Vector where
    type VBox Vector e = ()
    fromList = fromList
    toList = toList
    length = length
    vmap = vmap
    vbinary = vbinary
    vindex = vindex
    vappend = vappend
    vconcat = vconcat
    vhead = vhead
    vlast = vlast
    vtail = vtail
    vinit = vinit
    vreverse = vreverse
    vfoldl = vfoldl
    vfoldl1 = vfoldl1
    vfoldr = vfoldr
    vfoldr1 = vfoldr1

instance LATC.Matrix Matrix where
    type MBox Matrix e = ()
    fromLists = fromLists
    toLists = toLists
    size = size
    mmap = mmap
    transpose = transpose
    mbinary = mbinary
    mindex = mindex
    mconcatrows = mconcatrows
    mconcatcols = mconcatcols
    mappendrows = mappendrows
    mappendcols = mappendcols
    minitr = minitr
    mtailr = mtailr
    minitc = minitc
    mtailc = mtailc
    mreverser = mreverser
    mreversec = mreversec

instance LATC.MV Matrix Vector where
    type MVBox Matrix Vector e = ()
    fromCols = fromCols
    toCols = toCols
    fromRows = fromRows
    toRows = toRows
    mCol = mCol
    mRow = mRow
    mheadr = mheadr
    mlastr = mlastr
    mheadc = mheadc
    mlastc = mlastc
    mfoldlr = mfoldlr
    mfoldl1r = mfoldl1r
    mfoldlc = mfoldlc
    mfoldl1c = mfoldl1c
    mfoldrr = mfoldrr
    mfoldr1r = mfoldr1r
    mfoldrc = mfoldrc
    mfoldr1c = mfoldr1c

instance LATC.LinAlg Matrix Vector where
    type LinAlgBox Matrix Vector e = Num e 
    mv = matvec
    vm = vecmat
    mm = matmat
    inner = inner
    outer = outer

-------- Vectors

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
vmap :: (a -> b) -> Vector a -> Vector b
vmap f (Vector v) = Vector $ P.map f v

-- | Apply a binary function @f@ to two @Vector@s
vbinary :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
vbinary f (Vector v1) (Vector v2) = Vector $ P.zipWith f v1 v2

-- | Return the element at the given position in the vector.
-- Zero-indexed.
vindex :: Vector a -> Int -> a
vindex (Vector v) i = v !! i

-- | Append one vector to the end of another.
vappend :: Vector a -> Vector a -> Vector a
vappend (Vector v1) (Vector v2) = Vector $ v1 ++ v2

-- | Concatenate a finite list of vectors into a single vector.
vconcat :: [Vector a] -> Vector a
vconcat = DL.foldl1' vappend 

-- | Lift a unary list operation to a @Vector@ operation
vlift :: ([a] -> [b]) -> Vector a -> Vector b
vlift f = Vector . f . unvector

-- | Lift a binary list operation to a @Vector@ operation
vlift2 :: ([a] -> [b] -> [c]) -> Vector a -> Vector b -> Vector c
vlift2 f a b = Vector $ f (unvector a) (unvector b)

-- | Return the first element of a vector.
vhead :: Vector a -> a
vhead = head . unvector

-- | Return the last element of a vector.
vlast :: Vector a -> a
vlast = last . unvector

-- | Return the tail of a vector.
vtail :: Vector a -> Vector a
vtail = vlift tail

-- | Return all elements of a vector but the last.
vinit :: Vector a -> Vector a
vinit = vlift DL.init

-- | Reverse the order of elements in a vector.
vreverse :: Vector a -> Vector a
vreverse = vlift reverse

-- | Left fold across a vector (strict).
vfoldl :: (a -> e -> a) -> a -> Vector e -> a
vfoldl f i (Vector v) = DL.foldl' f i v

-- | Left fold across a vector (strict), using the first element as a
-- starting point.
vfoldl1 :: (e -> e -> e) -> Vector e -> e
vfoldl1 f (Vector v) = DL.foldl1' f v

-- | Right fold across a vector.
vfoldr :: (e -> a -> a) -> a -> Vector e -> a
vfoldr f i (Vector v) = DL.foldr f i v

-- | Right fold across a vector, using the first element as a starting
-- point
vfoldr1 :: (e -> e -> e) -> Vector e -> e
vfoldr1 f (Vector v) = DL.foldr1 f v

------- Matrices

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
mCol (Matrix m) i = Vector $ DL.transpose m !! i

-- | Lift a unary nested-list operation to a @Matrix@ operation
mlift :: ([[a]] -> [[b]]) -> Matrix a -> Matrix b
mlift f = Matrix . f . unmatrix

-- | Lift a unary list operation to a @Matrix@ operation on its rows
mliftr :: ([a] -> [b]) -> Matrix a -> Matrix b
mliftr f m = Matrix $ DL.map f $ unmatrix m

-- | Lift a unary list operation to a @Matrix@ operation on its
-- columns
mliftc :: ([a] -> [b]) -> Matrix a -> Matrix b
mliftc f m = Matrix $ DL.transpose $ DL.map f $ DL.transpose $ unmatrix m

-- | Lift a binary nested-list operation to a @Matrix@ operation
mlift2 :: ([[a]] -> [[b]] -> [[c]]) -> Matrix a -> Matrix b -> Matrix c
mlift2 f a b = Matrix $ f (unmatrix a) (unmatrix b)

-- | Lift a binary list operation to a @Matrix@ operation on its rows
mlift2r :: ([a] -> [b] -> [c]) -> Matrix a -> Matrix b -> Matrix c
mlift2r f a b = Matrix $ zipWith f a' b'
    where (a', b') = (unmatrix a, unmatrix b)

-- | Lift a binary list operation to a @Matrix@ operation on its columns
mlift2c :: ([a] -> [b] -> [c]) -> Matrix a -> Matrix b -> Matrix c
mlift2c f a b = Matrix $ DL.transpose $ zipWith f a' b'
    where (a', b') = fmap DL.transpose (unmatrix a, unmatrix b)

-- | Concatenate two matrices so that their rows are now concatenated
mappendrows :: Matrix a -> Matrix a -> Matrix a
mappendrows = mlift2r (++)

-- | Concatenate two matrices so that their columns are now
-- concatenated
mappendcols :: Matrix a -> Matrix a -> Matrix a
mappendcols = mlift2c (++)

-- | Concatenate a finite list of matrices so that their rows are now
-- concatenated.
mconcatrows :: [Matrix a] -> Matrix a
mconcatrows = DL.foldl1' mappendrows

-- | Concatenate a finite list of matrices so that their columns are
-- now concatenated.
mconcatcols :: [Matrix a] -> Matrix a
mconcatcols = DL.foldl1' mappendcols

-- | The row-wise head (first row) of a matrix.
mheadr :: Matrix a -> Vector a
mheadr = Vector . head . unmatrix

-- | The last row of a matrix.
mlastr :: Matrix a -> Vector a
mlastr = Vector . last . unmatrix

-- | The row-wise tail of a matrix.
mtailr :: Matrix a -> Matrix a
mtailr = mliftr tail

-- | The row-wise init of a matrix.
minitr :: Matrix a -> Matrix a
minitr = mliftr init

-- | The column-wise head (first column) of a matrix.
mheadc :: Matrix a -> Vector a
mheadc = Vector . head . DL.transpose . unmatrix

-- | The last column of a matrix.
mlastc :: Matrix a -> Vector a
mlastc = Vector . last . DL.transpose . unmatrix

-- | The column-wise tail of a matrix.
mtailc :: Matrix a -> Matrix a
mtailc = mliftc tail

-- | The column-wise init of a matrix.
minitc :: Matrix a -> Matrix a
minitc = mliftc init

-- | Reverse the order of rows within the matrix (reverse all the
-- column vectors).
mreverser :: Matrix a -> Matrix a
mreverser = mliftc reverse

-- | Reverse the order of columns within the matrix (reverse all the
-- row vectors).
mreversec :: Matrix a -> Matrix a
mreversec = mliftr reverse

-- | A strict left-fold across the rows of a matrix.
mfoldlr :: (a -> Vector e -> a) -> a -> Matrix e -> a
mfoldlr f i = DL.foldl' (\a v -> f a $ Vector v) i . unmatrix

-- | A strict left-fold across the rows of a matrix, using the first
-- row as an initial value
mfoldl1r :: (Vector e -> Vector e -> Vector e) -> Matrix e -> Vector e
mfoldl1r f = DL.foldl1' f . DL.map Vector . unmatrix

-- | A strict left-fold across the columns of a matrix.
mfoldlc :: (a -> Vector e -> a) -> a -> Matrix e -> a
mfoldlc f i = DL.foldl' (\a v -> f a $ Vector v) i . unmatrix . transpose

-- | A strict left-fold across the columns of a matrix, using the
-- first column as an initial value
mfoldl1c :: (Vector e -> Vector e -> Vector e) -> Matrix e -> Vector e
mfoldl1c f = DL.foldl1' f . DL.map Vector . unmatrix . transpose

-- | A right-fold across the rows of a matrix.
mfoldrr :: (Vector e -> a -> a) -> a -> Matrix e -> a
mfoldrr f i = DL.foldr (\v a -> f (Vector v) a) i . unmatrix

-- | A right-fold across the rows of a matrix, using the first row as
-- an initial value
mfoldr1r :: (Vector e -> Vector e -> Vector e) -> Matrix e -> Vector e
mfoldr1r f = DL.foldr1 f . DL.map Vector . unmatrix

-- | A right-fold across the columns of a matrix.
mfoldrc :: (Vector e -> a -> a) -> a -> Matrix e -> a
mfoldrc f i = DL.foldr (\v a -> f (Vector v) a) i . unmatrix . transpose

-- | A right-fold across the columns of a matrix, using the first
-- column as an initial value
mfoldr1c :: (Vector e -> Vector e -> Vector e) -> Matrix e -> Vector e
mfoldr1c f = DL.foldr1 f . DL.map Vector . unmatrix . transpose

--------- math functions

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
