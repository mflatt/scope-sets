#lang scribble/base
@(require "common.rkt"
          (prefix-in core: (submod "model/core-model.rkt" pict)))

@title[#:tag "hygiene"]{Defining Hygiene}

Most previous work on hygiene has focused on
expansion algorithms, but some work
addresses the question of what @defterm{hygiene} means
independent of a particular algorithm. In his dissertation,
@citet[herman-dissertation] addresses the question through a type
system that constrains and exposes the binding structure of macro
expansions, so that α-renaming can be applied to unexpanded programs.
More recently, @citet[essence-of-hygiene] defines hygienic
macro-expansion steps as obeying invariants that are expressed in
terms of renaming via nominal logic@~cite[nominal-logic], and the
concept of equivariance plays an important role in characterizing
hygienic macro transformers.

Since our notion of binding is based on scope sets instead of
renaming, previous work on defining hygiene via renaming
does not map directly into our setting. A related obstacle
is that our model transforms a syntax object to a syntax
object, instead of directly producing an AST; that difference is
necessary to support local and partial expansion, which in turn is
needed for definition contexts. A more technical obstacle
is that we have specified expansion in terms of a
meta-function (i.e., a big-step semantics) instead of as a rewriting
system (i.e., a small-step semantics).

Adams's approach to defining hygiene nevertheless seems applicable to
our notion of binding. We leave a full exploration for future work,
but we can offer an informed guess about how that exploration will
turn out.

Although our model of expansion does not incorporate renaming as a
core concept, if we make assumptions similar to Adams (including
omitting the @racket[quote] form), then a renaming property seems
useful and within reach. For a given set of scopes and a point during
expansion (exclusive of macro invocations), the symbol can be
swapped in every identifier that has a superset of the given set of
scopes; such a swap matches the programmer's intuition that any
variable can be consistently renamed within a binding region, which
corresponds to a set of scopes. Hygienic expansion then means that the
@core:tm[parse] of the continued expansion after swapping is
α-equivalent to what it would be without swapping. An individual
transformer could be classified as hygienic based on all introduced
identifiers having a fresh scope, so that they cannot bind any
non-introduced identifiers; the fresh scope ensures an analog to
Adams's equivariance with respect to binders.

Note that swapping @racket[x] with @racket[y] for the scope set
@set[s_1df s_2i1] would @emph{not} produce an equivalent program for
the expansion in @secref["pattern-ambiguous"], because it would
convert an ambiguous reference @id[x s_1df s_2i1 s_3i2] to an
unambiguous @id[y s_1df s_2i1 s_3i2]. This failure should not suggest
that the pattern-matching macros in that example are non-hygienic in
themselves, but that the (implicit) definition-context macro is
potentially non-hygienic. That is, a macro in a definition context can
introduce an identifier that is captured at the macro-use site, since
the definition and use sites can be the same. That potential for
non-hygienic expansion appears to be one of the trade-offs of
providing a context that allows a mixture of mutually recursive macro
and variable definitions.

If macro bindings are constrained to @racket[letrec-syntax], and if
macro implementations are constrained use
@racket[syntax-case], @racket[free-identifier=?], and
@racket[syntax->datum] (not @racket[bound-identifier=?] or
@racket[datum->syntax]), then we expect that all expansion steps will
be provably hygienic and all macro transformers will be provably
hygienic by the definitions sketched above.
