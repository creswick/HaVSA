# Version Space Algebra #

Version Space Algebra is a set of operations that allow multiple
version spaces to be combined.  Before examining these operations we
should first talk about version spaces themselves.

## Version Spaces ##

A version space is simply a set of hypotheses.  

Given a collection of examples we would like to form a hypothesis that
explains the data.  Hopefully, there is one hypothesis that perfectly
explains all data seen, and all data that will appear in the future
(for that problem).

Version Spaces are collections of all possible hypotheses for a given
class of problems.  These collections are narrowed down as data is
provided, eliminating hypotheses that are inconsistent with the
example data (these hypotheses don't adequately explain those
examples).

Atomic version spaces (or just "Version Spaces") are typically very
large -- potentially containing an uncountably infinite number of
hypotheses.  This poses a problem when narrowing hypotheses, unless
some criteria are met.  

(Cite boundary-set-representable paper.)

__________ shows that version spaces can be efficiently represented
and narrowed by ordering the hypotheses by some generality criteria.
This ordering needs to be engineered such that the boundaries move
gradually inward as examples are seen.

## Composing Version Spaces ##

Tessa Lau developed a technique for composing version spaces with
three different operations:

   * Transform
   * Union
   * Join

The detailed explanation of these operations is given in [ML VS
paper], but a brief overview is given below.

Version Space algebra helps to move the complexity of atomic version
spaces (outlined above) from within the specific version spaces, and
into the *structure* of composite version spaces.  Atomic
version spaces are still used, but they are trivially simple when used
in Version Space Algebra.

### Transform ###

Transform changes the input and output types of a version space to
match a new type.  Transforms can be used to:

   * Generate complex structures from tuple types
   * Convert types to match the expectations of a composite version space.

Transformations are performed with `VS.transform(VS, Transform)`.

    -- | Transform to adapt Rational VSs to Integral VSs
    intFromRatTr :: VersionSpace Rational Rational -> VersionSpace Int Int
    intFromRatTr # Tr fromIntegral fromIntegral round


### Union ###

The simplest means to combine version spaces is with union.  Unioning
two version spaces generates a composite version space that, as
expected, contains the set of hypotheses in both version spaces.

Union is useful for generating a complex version space that consists
of hypotheses that generate results of the same *type* but through
much different *means*.  For example, an offset in a region can be
expressed as a fixed offset from one end or a ratio of the length of
the enclosing region.  Both approaches generate the same thing -- an
integer offset -- but they operate much differently, so the version
spaces should be separate.

This is used in sizeVS:

    -- | Version space that learns fixed or relative
    -- offsets into an input region:
    sizeVS :: VersionSpace Int Int
    sizeVS = (VS AnyInt) `union` (intFromRatTr $ VS AnyRat)

`union` takes two version spaces with the same input and
output types and generates a version space that encompasses both of
the provided version spaces.

In this example the input version spaces have types that are not
directly compatible, so a transform is needed to create version spaces
that take and produce the correct outputs.

### Join ###

Two version spaces can also be combined with a *join* operation.

Joining version spaces effectively generates the cartesian product of
the two sets of hypotheses (one from each joined version space),
although the full set of hypotheses is only created when a version
space is executed.

Because `join` is defined in general terms, the input and
output types are simply a tuple type of the inputs and outputs of the
component types.  Since tuples are generally of limited use, it is
typical to apply a transform to the result of a join in order to add
structure (and type information) to the resulting composite version
space.

The following snippet shows the construction of the `rectangle` version space from composite version spaces:


    -- | Define some types to simplify the syntax and add semantics:
    data Rectangle = Rect Int Int Int Int
    type Region1D = (Int, Int)
    
    -- | Rectangle VS learns rectangles contained in a rectangular region.
    -- This is simply a join of two 1-D regions, wrapped in a transform.
    rectangleVS :: VersionSpace Rectangle Rectangle
    rectangleVS = rectTr $ region1d `join` region1d
                  where 
                    rectTr = Tr decompose decompose compose
                    compose ((x, w), (y, h)) = Rect x y w h
                    decompose (Rect x y w h) = ((x, w), (y, h))

    -- | The core components of RectangleVS: (1-D regions)
    region1d :: VersionSpace Region1D Region1D
    region1d = sizeVS `join` sizeVS -- offset and width.
