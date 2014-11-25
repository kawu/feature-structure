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



> -- >>>>>>>>>>>>>>>>>>>
> -- BOOLEAN EXPRESSIONS
> -- >>>>>>>>>>>>>>>>>>>



Furthermore, it is possible to build logical expressions over equations.
It is quite a tricky case.  

> -- something like this, at least:
> data LexDesc = BExpr Path
> data BExpr a  
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
different (and perhaps incompatible!) structures.  The principled solution
seems to be to consider all the disjunction elements and for each element
create a resulting FS (if possible).  The scope of coreferences shoud be then
limitied to individual elements (since only one of them needs to be satisfied).
This leads us to another question -- what if in the set of results (trees with
corresponding FSs) there are two elements constructed through the use of the
two elements of the same disjuntive rule?  It seems highly unreasonable.  The
reasonable solution, then, would be to consider individual disjunction elements
in a sequential manner.  In other words, we would like to apply the following
rule: as long as the A element is not discarded in the parsing process, do not
consider using the B element form the A | B rule.  This is still not very
precise, though, A can be discarded in some "parsing paths" but not in others.
For now, it seems easier to consider every element of any given disjunctive
rule.

COROLLARY: For a given disjunctive rule of the A_1 | A_2 | ... form we
consider individually each element of the rule and use it to construct
a separate version of the resultant graph.  Note, however, that in the
final version of the parsing algorithm this should be improved.

NOTE: It seems that it doesn't make sense to convert a general logical formula
into a disjunctive normal form because, in the general case, it can lead to an
exponential explosion of the size of the formula.  Therefore, it seems more
reasonable to stay with the formula given in the grammar and try to interpret
it more or less directly.

QUESTION: Let's consider a simple ((A | B) & C) formula.  We can either
interpret it sequentially and first try A & C and only then B & C.
Alternatively, we could check the compatibility of the structure with C and
only afterwards try A and (if necessary) B.  It can be an important
improvement in some cases, but wheter or not such permutation should be allowed
to be performed by the compiler is not clear.

COROLLARY: For the ((A | B) & C) the "paths" will be considered in the given
order.  In other words, we will not consider (at this point) optimizations of
the compiler which automatically try to find the best permutation (and change,
for example, the given formula into (C & (A | B))).

How could we interpret a boolean expression in the general case?  The boolean
expression can be represented as a tree where each level represents,
alternately, a conjunction and a disjunction.  The tree is traversed in a given
order (to be decided) and at each node representing a disjunction, a copy of
the currently considered FS has to be copied for each of its daughter nodes.

NOTE: All is probably simpler for the constraining equations.  They do not
interact in any way with FSs so there is no need to copy a given FS and create
a new one.



> -- >>>>>>>>>>>>>>>>>>>
> -- REGULAR EXPRESSIONS
> -- >>>>>>>>>>>>>>>>>>>



A path in LFG can be any regular equation over features.  The case of
constraining equations seems not hard to implement, so let's move to defining
equations.

Quotations from XLE docs:
* "Kaplan and Zaenen (1989b) assign an existential meaning in this situation"
* "Functional uncertainties are `nonconstructive`. This means that all of the
  attributes that are within a regular operator other than concatenation only
  match against existing attributes in the f-structure, they never construct
  attributes where none existed before"

From the citations above we can conclude that a general equation with regular
path over features has an existential meaning in XLE.  Moreover, complex
regular path-expressions (or, to be more precise, their parts) are
non-constructuve!  Nevertheless, the right-hand part of the equation (the
"value") can be defining.

NOTE: It would be actually easy to implement "regular equations" if only their
meaning was universal, which is the default way the cycles in feature
structures are interpreted.  In other words, if the interpretation of the
kleene stare was universal, we could easly rewrite e.g. ((f A*) = x) as an AVM
and, therefore, also as a feature graph.

NOTE: When looking at a given FS, it should be remebered that the full form of
it will be available only after the parsing of a given sentence is finished.

The question is how the "uncertainty" constraints should be stored together
with regular constraints.  Let's consider a very simple example where there are
two constraints on a given FS f: ((f A) = x) and ((f A*) = y).  The f feature
structure can still be extended in the later phases of the parsing process,
thus we do not know if 'x' should be unified with 'y' or not.  Keep in mind,
that functional uncertainties have an existential interpretation.  We could,
in this particular case, decide that ((f A) = x) and ((f A*) = y) unify to
[(f A) = x ^ y] *or* [(f A) = x, (f A A*) = y].

COROLLARY: It looks like the unification of two FSs can lead, in the general
case and in the light of extensions like functional uncertainties, to a set
of FSs.  The problem is that this can lead to an explosion of the number of
FSs constructed and stored during the parsing process.

This leads us to another view on feature structures.  They can be seen as
collections of information and constraints which, finally (when the parsing
process is finished), lead to a consistent feature graph.  In the meantime,
though, this property of consistency between individual constraints can be
impossible to enforce, since some constraints will only have sense when parsing
is finished.

Yet we will try to develop a view in which existential equations are consistent
with the designed formalism and do not require additional parsing phases.
While it may not lead to a light-speed LFG parser, it will probably help in
understanding the details of LFG.

Let's note that it is actually possible to convert any existential equation
into an alternative.  Take ((f A*) = x) for example.  It can be recursively
translated to (f = x) | ((f A A*) = x) -> (f = x) | ((f A) = x) | ((f A A A*)
= x) | (...).  Such an alternative is obviously infinite, but it can be
generated lazily in practice.  Similarly, ((f (A)) = x) /where the feature A
is optional/ can be compiled to (f = x) | ((f A) = x).

Let's now consider a simple case in which there are two equations assigned to
a given node: ((f A) = x) and ((f A*) = y) (an example we have already
considered above).  Now, since we can translate the existential equation to an
alternative and each element of the alternative is a simple equation, we can
unify the first equation ((f A) = x) with the generated list and, this way,
obtain the resulting alternative-feature-structure.  Alternatively, if we
decide to explicitely represent conjunction, just as we are doing with
alternative, we could just leave the two equations in a conjunctive relation,
((f A) = x) && ((f A*) = y), which would probably have positive influence on
the parsing process (in this case, at least, because the influence in general
would be hard to determine).


> -- >>>>>>>>>>>>>>>>>>>>>>>>>>>
> -- NON-DISTRIBUTIVE ATTRIBUTES
> -- >>>>>>>>>>>>>>>>>>>>>>>>>>>


> -- >>>>>>>>>>>>>>>>>>>
> -- MISC
> -- >>>>>>>>>>>>>>>>>>>



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
