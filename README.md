Linear algebra typeclasses
========

We want to write linear algebra code in terms of typeclasses, because
we'd like to allow it to be used with multiple backends, including

 - Symbolic tensors (Data.Vector, lists)
 - HMatrix (BLAS/ATLAS) matrices
 - Repa arrays
 - Data.Array.Accelerate?

Current status
--------------

This works, and the type signature is correctly inferred:

    vcos a = vmap cos a

    *Numeric.LATC> :t vcos
    vcos :: (VBox v f, Floating f, Vector v) => v f -> v f
    
This works if you disable the monomorphism restriction: 

    vcos = vmap cos

    *Numeric.LATC> :t vcos
    vcos :: (VBox v f, Floating f, Vector v) => v f -> v f

This works too, with the monomorphism restriction: 

    vcos :: (Vec v e, Floating e) => v e -> v e
    vcos = vmap cos

The cool part is that `vcos` can be passed symbolic or numeric values
in lists, symbolic or numeric values in `Data.Vector` vectors, numeric
values in `HMatrix` vectors or numeric values in `Repa` arrays.

The other cool part is that the minimal interface you must implement
to add a new backend is `fromList`, `toList`, `fromLists`, `toLists`,
`mm` (matrix-matrix multiply) and `inner` (vector inner product), plus
any type class constraints you have for your backend
(e.g. `Storable`).  Everything else has a derived default
implementation and correctly propagates all the type class
constraints.  The default implementation relies on lists to implement
everything; thus, it is slow, and big speedups can be had by defining
a few things, like `vfoldr` and `mfoldrr` to speed up all the fold
methods.

See the code for examples of how to build and/or add backends.  We
know it usually sucks to say "see the code," but we think it is simple
and well-documented -- and if it's not, please ask!

Further questions
----------------

 - How to extend this to general tensors?  It seems like a way with
 associated type synonyms that gives default synonyms as lists of the
 successively lower-order tensors might subsume the `MV` class, and
 implementations can simply replace low-order tensors with custom
 types (like `PV.Vector` and `PM.Matrix`).

 - How to accommodate Repa's rank-in-the-type for arbitrary tensors?

 - How to accommodate staged/monadic computations like in Repa and
   Accelerate?

 - How to remain open to (in the future) units, static sizes and
 coordinate systems (and transforming arbitrary-rank tensors between
 coordinate systems),

 - Nice infix operators for linear algebraic multiplication (the Mul
 typeclass?) and element-wise multiplications and broadcasting.

