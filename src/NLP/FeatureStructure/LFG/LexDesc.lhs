> module NLP.FeatureStructure.LFG.LexDesc
> (
> ) where
> 
> import qualified Data.Set as S

Let us first assume that there is a Feat datatype, which represent
a feature, as well as Val, which represents atomic values.

> data Feat
> data Val

We will try to find here a proper data type for the representation
of lexical descriptions.  In the simplest case, a lexical description
consists of a list/set of path constraints.

> data LexDesc = S.Set Path
> data Path = ([Feat], Val)

But, in practice, the structure of path constraints can be much more
complex.  First of all, there are two basic types of path equations:
defining and constraining.

> data Path = ([Feat], Eq, Val)
> data Eq = DefEq | ConEq

Apart from regular path equations, there are also: negative equations,
existential constraints and negative existential constraints.
It is also worth noting at this point that values, in some cases,
should probably take a more complex structure (e.g. alternative
of atomic values).

> data Path
>   = DefPath [Feat] Val    -- ^ defining equation
>   | ConPath [Feat] Val    -- ^ constraining equation
>   | NegPath [Feat] Val    -- ^ negative equation
>   | ExPath  [Feat]        -- ^ existential equation
>   | NexPath [Feat]        -- ^ negative existential equation

Furthermore, it is possible to build logical expressions over equations.
It is quite a tricky case.  

> data LexDesc = BExpr Path
> data BExpr a  -- something like this, at least
>   = BAnd (BExpr a) (BExpr a)
>   | BOr  (BExpr a) (BExpr a)
>   | BAtom a

The way coreferences play together with boolean expressions is not obvious.
Let A be a set of equations linked by a conjunction relation (which is the
default interpretation by the way, i.e. a set of equations is by default
interpreted as a conjunction -- all must be satisfied).  Since all the elements
must be satisfied, we can safely assume that the scope of the "coreferences" is
over all the equations in the set.

Let's consider a simple disjunction of two equations, A | B.  In this case
either A or B must be satisfied (but not both).  The interpretation of
disjunctions w.r.t. coreferences is not obvious -- it may be that a given FS
satisfies both A and B, but from its unification with either A or B arise
different (and perhaps incompatible!) structures.  The principled solutions
seems to be to consider all the disjunction elements and for each element
create a resulting FS.  The scope of coreferences shoud be then limitied to
individual elements (since only one of them needs to be satisfied).  Or maybe
a more principled idea would be to consider all the This leads us to another
issue -- even if A is compatible with the given FS, the result of A <-> FS
unification may be discarded in the later phases of the parsing process!


CONTROL QUESTIONS: let's say that we have a feature graph G and a set of
equations.  Each equation can be thought as a graph, but together they
constitute a graph as well.  Now the question proper: is it better to join the
graph G with the paths individualy, one after the other, or maybe it is better
to unify G with the paths joined into a graph beforehand?


Other issues:
* Off-path constraints make the structure of the constraints more complex.
  But how?  It is not clear at the moment. 
* Because of coreferences even two simple path equations can define not
  only a tree, but even a graph!
