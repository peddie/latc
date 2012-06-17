> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

> {-# OPTIONS_GHC -Wall #-}
> 
> module CKLA where

You have to separately import this to do anything with
ConstraintKinds.

> import GHC.Prim (Constraint)

We want the :. operator to use nested type constructors.

> import Control.Compose ((:.)(..))

Data.Vector is another alternative instance

> import qualified Data.Vector as DV

Get HMatrix stuff.

> import qualified Data.Packed.Vector as PV
> import qualified Data.Packed.Matrix as PM
> import qualified Numeric.Container as NC
> import Foreign.Storable (Storable)

Finally, hide Prelude stuff to make things less confusing.

> import qualified Prelude as P
> import Prelude (Num(..), ($), (.), Int, sqrt, Floating(..))

Here's the class for a Vector.  VBox is an associated type synonym
(see Type Families).  We declare it to have kind Constraint (see
ConstraintKinds and KindSignatures; ``::'' here declares a kind,
because VBox is a type), which informs the compiler that we intend to
use it like a typeclass constraint, even though it's not a typeclass.
This ATS allows us to leave the element type out of the class head
(``Class Vector v where'') and still limit the types we can use as
elements, e.g. for the HMatrix vectors, which require their elements
to be instances of Storable.  We also declare a default VBox type of
(), which tells the compiler that if we don't declare the type of VBox
when we define an instance, then there are no constraints on the type
of the element.

> type Vec v e = (Vector v, VBox v e)
> class Vector v where
>     type VBox v e :: Constraint
>     type VBox v e = (Vector v)
>     fromList :: Vec v e => [e] -> v e
>     toList :: Vec v e => v e -> [e]
>     length :: Vec v e => v e -> Int
>     map :: (Vec v e, Vec v f) => (e -> f) -> v e -> v f

Matrix works similarly to Vector.

> type Mat m e = (Matrix m, MBox m e)
> class Matrix m where
>     type MBox m e :: Constraint
>     type MBox m e = (Matrix m)
>     fromLists :: Mat m e => [[e]] -> m e
>     toLists :: Mat m e => m e -> [[e]]
>     size :: Mat m e => m e -> (Int, Int)
>     mmap :: (Mat m e, Mat m f) => (e -> f) -> m e -> m f

> -- | Related types for matrices and vectors.  These methods involve
> -- both structures, but they make no additional demands on the type of
> -- the element
> type MatVec m v e = (Matrix m, Vector v, MVBox m v e)
> class (Matrix m, Vector v) => MV m v where
>     type MVBox m v e :: Constraint
>     type MVBox m v e = (MBox m e, VBox v e)
>     fromRows :: MatVec m v e => [v e] -> m e
>     toRows :: MatVec m v e => m e -> [v e]
>     fromCols :: MatVec m v e => [v e] -> m e
>     toCols :: MatVec m v e => m e -> [v e]
> 
> -- | Linear algebra on matrices and vectors.  These methods involve
> -- both structures, and they require operands to have numeric
> -- elements.
> type LA m v e = (Vec v e, Mat m e, MatVec m v e, LinAlgBox m v e)
> class MV m v => LinAlg m v where
>     type LinAlgBox m v e :: Constraint 
>     type LinAlgBox m v e = (MBox m e, VBox v e, Num e)
>     mv :: LA m v e => m e -> v e -> v e
>     vm :: LA m v e => v e -> m e -> v e
>     mm :: LA m v e => m e -> m e -> m e
>     outer :: LA m v e => v e -> v e -> m e
>     inner :: LA m v e => v e -> v e -> e

Here I started to define List instances for both Vector and Matrix,
and add class default methods in terms of fromList, toList and the
methods for these instances.  This way, a minimal (but inefficient)
instance of these classes is merely "fromList" and "toList", and
better implementations can be added incrementally.  Unfortunately, I
don't know how to tell the compiler that I want to use nested lists as
matrices.  I can use the NestedList type synonym, but I can't
implement any methods!

> instance Vector [] where
>     fromList = P.id 
>     toList = P.id 
>     length = P.length
>     map = P.map
> 
> type NestedList = [] :. []
> instance Matrix NestedList where
>
> instance Vector DV.Vector where
>     fromList = DV.fromList
>     toList = DV.toList
>     length = DV.length
>     map = DV.map

I have the same problem with nesting Data.Vector.Vector to form a
matrix.

> type NestedVector = DV.Vector :. DV.Vector
> instance Matrix NestedVector where

Aww shit.  I can't make any methods for this instance.

All the HMatrix instances work without much fuss.

> instance Vector PV.Vector where
>     type VBox PV.Vector b = (Vector PV.Vector, Storable b)
>     fromList = PV.fromList
>     toList = PV.toList
>     length = PV.dim
>     map = PV.mapVector
> 
> instance Matrix PM.Matrix where
>     type MBox PM.Matrix b = (Matrix PM.Matrix, PM.Element b)
>     fromLists = PM.fromLists
>     toLists = PM.toLists
>     size a = (PM.rows a, PM.cols a)
>     mmap = PM.mapMatrix
> 
> instance MV PM.Matrix PV.Vector where

The nice part about this approach is how the default constraint type
for MVBox simply intersects the MBox and VBox constraint types for the
declared matrix and vector instances, so we don't have to declare
anything in spite of HMatrix's annoying shit!

>     fromRows = PM.fromRows
>     toRows = PM.toRows
>     fromCols = PM.fromColumns
>     toCols = PM.toColumns
>
> instance LinAlg PM.Matrix PV.Vector where

We weren't as lucky here as with MV.  Although the default LinAlgBox
type requires the element to be a Num instance, we have to add our own
constraint for NC.Product, because as usual, HMatrix decided to add
some more random-ass typeclass constraints.

>     type LinAlgBox PM.Matrix PV.Vector e = (MBox PM.Matrix e, VBox PV.Vector e, Num e, NC.Product e)
>     mv = NC.mXv
>     vm = NC.vXm
>     mm = NC.mXm
>     inner = (NC.<.>)
>     outer = NC.outer

