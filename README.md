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

    cosmap a = map cos a

    *CKLA> :t cosmap 
    cosmap :: (VBox v f, Floating f, Vector v) => v f -> v f
    
This works if you disable the monomorphism restriction: 

    cosmap = map cos

    *CKLA> :t cosmap
    cosmap :: (VBox v f, Floating f, Vector v) => v f -> v f

This works too: 

    cosmap :: (Vec v e, Floating e) => v e -> v e
    cosmap = map cos

Further questions
----------------

Beyond nested types for Matrix instances and filling in a ton more
methods:

 - How to extend this to general tensors?  It seems like a way with
 associated type synonyms that gives default synonyms as lists of the
 successively lower-order tensors might subsume the MV class, and
 implementations can simply replace low-order tensors with custom
 types (like PV.Vector and PM.Matrix).

 - How to accommodate Repa's rank-in-the-type for arbitrary tensors?

 - How to remain open to (in the future) units, static sizes and
 coordinate systems (and transforming arbitrary-rank tensors between
 coordinate systems),

 - How to integrate properly with Expr from DVDA?

 - Nice infix operators for linear algebraic multiplication (the Mul
 typeclass?) and element-wise multiplications and broadcasting.

