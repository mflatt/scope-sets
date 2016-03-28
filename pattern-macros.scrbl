#lang scribble/base
@(require racket/list
          "common.rkt")

@title[#:tag "pattern-macros"]{Scope Sets for Pattern-Based Macros}

Like previous models of macro expansion, our set-of-scopes expander
operates on a program from the outside in. The expander detects bindings,
macro uses, and references as part of the outside-to-inside traversal.
The difference in our expander is the way that bindings and macro
expansions are recorded and attached to syntax fragments during
expansion.

@; --------------------------------------------------
@section{Scope Sets}

A @defterm{scope} corresponds to a binding context, and every
identifier in a program has a set of scopes. For example, if we treat
@racket[let] and @racket[lambda] as primitive binding forms, then in
the fully expanded expression
@;
@racketblock[
(let ([x 1])
  (lambda (y)
    z))
]
@;
the @racket[let] form corresponds to a scope @|s_1lt|, and the
@racket[lambda] form corresponds to @|s_2lm|. That is, everything in
the @racket[let]'s body is in @|s_1lt|, and everything in the
inner @racket[lambda]'s body is in @|s_2lm|; the set of scopes
associated with @racket[z] is @set[s_1lt s_2lm].@narrow-audience-only[@note{We
might say that ``the environment of @racket[z] is @set[s_1lt
s_2lm],'' but for macro expansion, we also use the word
``environment'' to refer to a mapping from bindings to variables and
compile-time values. To avoid confusion, we'll refrain from using
``environment'' to mean a set of scopes.}] (Notation: the subscripts on
@|s_1lt| and @|s_2lm| are just part of the names that we use to
refer to abstract scope tokens; they have no meaning
beyond indicating the scope's origin.)

In a macro-extensible language, expanding a use of a macro creates a
new scope in the same way that a binding form creates a new scope.
Starting with
@;
@racketblock[
(let ([x 1])
  (let-syntax ([m (syntax-rules ()
                    [(m) x])])
    (lambda (x)
       (m))))
]
@;
the right-hand side of the @racket[m] binding has the scope set
@set[s_1lt], while the final @racket[m] has scope set
@set[s_1lt s_2ls s_3lm] corresponding to the
@racket[let], @racket[let-syntax], and @racket[lambda] forms.
We can write the scope sets next to each @racket[x] and @racket[m]
at the point where macro expansion reaches the @racket[(m)] form:
@;
@racketblock[
(let ([@#,id[x s_1lt] 1])
  (let-syntax ([@#,id[m s_1lt s_2ls] (syntax-rules ()
                            [(m) #'@#,id[x s_1lt]])])
    (lambda (@#,id[x s_1lt s_2ls s_3lm])
      (@#,id[m s_1lt s_2ls s_3lm]))))
]
@;
The expansion of @racket[(m)] produces
@racket[x] with the scope set @set[s_1lt s_4i], where
@|s_4i| is a new scope for identifiers that are introduced by the
macro's expansion:
@;
@racketblock[
(let ([@#,id[x s_1lt] 1])
  (let-syntax ([@#,id[m s_1lt s_2ls] (syntax-rules ()
                            [(m) #'@#,id[x s_1lt]])])
    (lambda (@#,id[x s_1lt s_2ls s_3lm])
      @#,id[x s_1lt s_4i])))
]
@;
The absence of @|s_3lm| on the final @racket[x] explains why it
doesn't refer to the inner binding of @racket[x]. At the same time, if
a different @racket[m] places a macro-introduced @racket[x] in a binding position around
an @racket[x] from a macro use @racket[(m x)], the @racket[x] from the use is not
macro-introduced and doesn't have the scope @|s_4i|, so it wouldn't
refer to the macro-introduced binding.

Lexical scoping corresponds to sets that are constrained to a
particular shape: For any given set, there's a single scope @|s_i| that
implies all the others (i.e., the ones around @|s_i| in the
program). As a result, @|s_i| by itself is enough information to
identify a binding for a given reference. We normally
describe lexical scope in terms of the closest such @|s_i| for
some notion of ``closest.'' Given scope sets instead of individual
scopes, we can define ``closest'' as the largest relevant set.

More generally, we can define binding based on @emph{subsets}: A
reference's binding is found as one whose set of scopes is the largest subset of
the reference's own scopes (in addition to having the same symbolic
name). The advantage of using sets of scopes is that macro expansion
creates scope sets that overlap in more general ways; there's not
always a @|s_i| that implies all the others. Absent a determining
@|s_i|, we can't identify a binding by a single scope, but we can
identify it by a set of scopes.

If arbitrary sets of scopes are possible, then two different bindings
might have overlapping scopes, neither might be a subset of the other,
and both might be subsets of a particular reference's scope set. In
that case, the reference is ambiguous. Creating an ambiguous reference with only
pattern-based macros is possible, but it requires a definition context
that supports mingled macro definitions and uses; we provide an
example in @secref["pattern-ambiguous"].

@; --------------------------------------------------
@section{Bindings}

When macro expansion encounters a primitive binding form, it
@;
@itemlist[

 @item{creates a new scope;}

 @item{adds the scope to every identifier in binding position, as well
       as to the region where the bindings apply; and}

 @item{extends a global table that maps a @tuple[@elem{symbol} @elem{scope set}]
       pair to a representation of a binding.}

]
@;
In a simplified language where bindings are local, an identifier with
its scope set could be its own representation of a binding. In a more
complete language, bindings can also refer to module imports@narrow-audience-only{, and
rename transformers (see @secref["Rename Transformers"]) can map
combinations to the same binding}. We therefore represent a local
binding with a unique, opaque value (e.g., a gensym).

For example,
@;
@racketblock[
(let ([x 1])
  (let-syntax ([m (syntax-rules ()
                    [(m) x])])
    (lambda (x)
      (m))))
]
@;
more precisely expands after several steps to
@pdf-codeblock{
@tabular[
#:sep @hspace[1]
#:column-properties '(right-border ())
(list
 (list
  (if (not narrow-column?)
  ;; Wider, more consistent for HTML:
  @racketblock[
(let ([@#,id[x s_1lt] 1])
  (let-syntax ([@#,id[m s_1lt s_2ls] (syntax-rules ()
                            [(m) #'@#,id[x s_1lt]])])
    (lambda (@#,id[x s_1lt s_2ls s_3lm])
       @#,id[x s_1lt s_4i])))
       ]
  ;; Narrower for PDF:
  @racketblock[
(let ([@#,id[x s_1lt] 1])
  (let-syntax ([@#,id[m s_1lt s_2ls]
                (syntax-rules ()
                  [(m) #'@#,id[x s_1lt]])])
    (lambda (@#,id[x s_1lt s_2ls s_3lm])
       @#,id[x s_1lt s_4i])))
       ])
    (tabular
     #:column-properties '(baseline baseline baseline)
     #:sep 'nbsp
     (list (list @id[x s_1lt] " → " @racket[x4])
           (list @id[m s_1lt s_2ls] " → " @racket[m8])
           (list @id[x s_1lt s_2ls s_3lm] " → " @racket[x16])))))
]}
@;
where the compile-time environment along the way (not shown) maps @racket[x4] to a
variable, @racket[m8] to a macro, and
@racket[x16] to another variable. The reference @id[x s_1lt s_4i] has the binding
@racket[x4], because @id[x s_1lt] is the mapping for @racket[x] in the binding
table that has the largest subset of @set[s_1lt s_4i].

The distinction between the binding table and the compile-time
environment is important for a purely ``syntactic'' view of binding,
where a term can be expanded, manipulated, transferred to a new
context, and then expanded again. Some approaches to macros, such as
syntactic closures@~cite[syntactic-closures] and explicit
renaming@~cite[explicit-renaming], tangle the binding and environment
facets of expansion so that terms cannot be manipulated with the same
flexibility.
           
The binding table can grow forever, but when a particular scope
becomes unreachable (i.e., when no reachable syntax object includes a
reference to the scope), then any mapping that includes the scope
becomes unreachable. This weak mapping can be approximated by
attaching the mapping to the scope, instead of using an actual global
table. Any scope in a scope set can house the binding, since the
binding can only be referenced using all of the scopes in the set.
Attaching to the most recently allocated scope is a good heuristic,
because the most recent scope is likely to be maximally distinguishing
and have the shortest lifetime.

@narrow-audience-only{When a syntax object is serialized, the serialization must pull along
the fragment of the binding table that is relevant for the syntax
object's scopes. Again, that extraction happens more or less
automatically when the binding-table entries are implemented by
attaching them to scopes, but an explicit ``garbage collection'' of
the scopes is useful in practice to make serialized syntax more compact.
Deserializing a syntax object creates fresh
representations for every serialized scope, but preserving sharing
ensures that binding relationships are preserved among identifiers in
the syntax object---or more generally, among syntax objects within a module.}

@; --------------------------------------------------
@section[#:tag "use-site"]{Recursive Macros and Use-Site Scopes}

So far, our characterization of macro-invocation scopes works only for
non-recursive macro definitions. To handle recursive macro
definitions, in addition to a fresh scope to distinguish forms that
are introduced by a macro, a fresh scope is needed to distinguish
forms that are present at the macro use site.

Consider the following @racket[letrec-syntax] expression, whose
meaning depends on whether a use-site identifier captures a macro-introduced
identifier:
@;
@racketblock[
(letrec-syntax ([identity (syntax-rules ()
                            [(_ misc-id)
                             (lambda (x)
                               (let ([misc-id 'other])
                                 x))])])
   (identity x))
]
@;
Assuming that the @racket[letrec-syntax] form creates a scope
@|s_1ls|, the scope must be added to both the right-hand side and
body of the @racket[letrec-syntax] form to create a recursive
binding:
@;
@racketblock[
(letrec-syntax ([identity (syntax-rules ()
                            [(_ misc-id)
                             (lambda (@#,id[x s_1ls])
                               (let ([misc-id 'other])
                                 @#,id[x s_1ls]))])])
   (identity @#,id[x s_1ls]))
]
@;
If we create a scope only for introduced forms in a macro expansion,
then expanding @racket[(identity @#,id[x s_1ls])] creates the scope set
@|s_2i| and produces
@;
@racketblock[
(lambda (@#,id[x s_1ls s_2i])
  (let ([@#,id[x s_1ls] 'other])
    @#,id[x s_1ls s_2i]))
]
@;  
where @|s_2i| is added to each of the two introduced @racket[x]s.
The @racket[lambda] introduces a new scope @|s_3lm|, and
@racket[let] introduces @|s_4lt|, producing
@;
@racketblock[
(lambda (@#,id[x s_1ls s_2i s_3lm])
  (let ([@#,id[x s_1ls s_3lm s_4lt] 'other])
    @#,id[x s_1ls s_2i s_3lm s_4lt]))
]
@;
At this point, the binding of the innermost @racket[x] is ambiguous:
@set[s_1ls s_2i s_3lm s_4lt] is a superset of both @set[s_1ls s_2i
s_3lm] and @set[s_1ls s_3lm s_4lt], neither of which is a subset of
the other. Instead, we want @racket[x] to refer to the @racket[lambda]
binding.

Adding a scope for the macro-use site corrects this problem. If we
call the use-site scope @|s_5u|, then we start with
@;
@racketblock[(identity @#,id[x s_1ls s_5u])]
@;
which expands to
@;
@racketblock[
(lambda (@#,id[x s_1ls s_2i])
  (let ([@#,id[x s_1ls s_5u] 'other])
    @#,id[x s_1ls s_2i]))
]
@;  
which ends up as
@;
@racketblock[
(lambda (@#,id[x s_1ls s_2i s_3lm])
  (let ([@#,id[x s_1ls s_3lm s_4lt s_5u] 'other])
    @#,id[x s_1ls s_2i s_3lm s_4lt]))
]
@;
There's no ambiguity, and the final @racket[x] refers to the
@racket[lambda] binding as intended. In short, each macro expansion
needs a use-site scope as the symmetric counterpart to the
macro-introduction scope.

@; --------------------------------------------------
@section[#:tag "intdef"]{Use-Site Scopes and Macro-Generated Definitions}

In a binding form such as @racket[let] or @racket[letrec], bindings
are clearly distinguished from uses by their positions
within the syntactic form. In addition to these forms, Racket (like Scheme) supports
definition contexts that mingle binding forms and expressions. For
example, the body of a module contains a mixture of definitions and
expressions, all in a single recursive scope. Definitions can include
macro definitions, expressions can include uses of those same macros,
and macro uses can even expand to further definitions.

With set-of-scopes macro expansion, macro
definitions and uses within a single context interact badly with
use-site scopes. For example, consider a @racket[define-identity]
macro that is intended to expand to a definition of a given identifier
as the identity function:
@;
@racketblock[
  (define-syntax-rule (define-identity id)
    (define id (lambda (x) x)))

  (define-identity f)
  (f 5)
]
@;
If the expansion of @racket[(define-identity f)] adds a scope to the
use-site @racket[f], the resulting definition does not bind the
@racket[f] in @racket[(f 5)].

The underlying issue is that a definition context must treat
use-site and introduced identifiers asymmetrically as binding
identifiers. In
@;
@racketblock[
  (define-syntax-rule (define-five misc-id)
    (begin
     (define misc-id 5)
     x))

  (define-five x)
]
@;
the introduced @racket[x] should refer to an @racket[x] that is defined
in the enclosing scope, which turns out to be the same @racket[x] that
appears at the use site of @racket[define-five]. But in
@;
@racketblock[
  (define-syntax-rule (define-other-five misc-id)
    (begin
     (define x 5)
     misc-id))

  (define-other-five x)
]
@;
the @racket[x] from the use site should not refer to the
macro-introduced binding @racket[x].

To support macros that expand to definitions of given identifiers, a
definition context must keep track of scopes created for macro uses,
and it must remove those scopes from identifiers that end up in
binding positions. In the @racket[define-identity] and
@racket[define-five] examples, the use-site scope is removed from the
binding identifiers @racket[x] and @racket[f], so they are treated the
same as if their definitions appeared directly in the source.

This special treatment of use-site scopes adds complexity to the macro
expander, but it is of the kind of complexity that mutually recursive
binding contexts create routinely (e.g., along the same lines as
ensuring that variables are defined before they are referenced).
Definition contexts in Racket have proven convenient and expressive
enough to be worth the extra measure of complexity.

@aside{

As an optimization, a use-site scope is needed only when a macro is
expanded in a definition context where the macro's definition is in
the same definition context. (Otherwise, the use is in a more nested
scope, and thus already distinguished from introduced identifiers.)
That combination happens regularly in a module body, but it is much
less common than macro uses generally. This ``optimization'' is
visible if the macro system provides certain operations, such as
forcing the expansion of a sub-form, but it is invisible to
pattern-based macros.

}

@; --------------------------------------------------
@section[#:tag "pattern-ambiguous"]{Ambiguous References}

The combination of use-site scopes to solve local-binding problems
(as in @secref["use-site"]) versus reverting use-site scopes to
accommodate macro-generated definitions (as in @secref["intdef"])
creates the possibility of generating an identifier whose binding is
ambiguous.

The following example defines @racket[m] through a @racket[def-m]
macro, and it uses @racket[m] in the same definition context:
@;
@racketblock[
(define-syntax-rule (def-m m given-x)
  (begin
    (define x 1)
    (define-syntax-rule (m)
      (begin
        (define given-x 2)
        x))))
code:blank
(def-m m x)
(m)
]
@;
The expansion, after splicing @racket[begin]s, ends with an ambiguous
reference:
@;
@racketblock[
  (define-syntax-rule (@#,id[def-m s_1df] ....) ....)
  (define @#,id[x s_1df s_2i1] 1)
  (define-syntax-rule (@#,id[m s_1df])
    (begin
      (define @#,id[x s_1df s_2u1] 2)
      @#,id[x s_1df s_2i1]))
  (define @#,id[x s_1df s_3i2] 2)
  @#,id[x s_1df s_2i1 s_3i2]
]
@;
The scope @|s_1df| corresponds to the definition context,
@|s_2i1| and @|s_2u1| correspond to the expansion of @racket[def-m],
@|s_3i2| corresponds to the expansion of @racket[m]. The final
reference to @racket[x] is ambiguous, because it was introduced
through both macro layers.

Unlike the ambiguity that is resolved by use-site scopes, this
ambiguity arguably reflects an inherent ambiguity in the macro. Absent
the @racket[(define x 1)] definition generated by @racket[def-m], the
final @racket[x] reference should refer to the definition generated
from @racket[(define given-x 2)]; similarly, absent the definition
generated from @racket[(define given-x 2)], the final @racket[x]
should refer to the one generated from @racket[(define x 1)]. Neither
of those definitions is more specific than the other, since they are
generated by different macro invocations, so our new expander rejects
the reference as ambiguous.

Our previous model of macro expansion to cover definition
contexts@~cite[expmodel] would treat the final @racket[x] always as a
reference to the definition generated from @racket[(define x 1)] and
never to the definition generated from @racket[(define given-x 2)].
@;
@narrow-audience-only{
More compactly, that model rejects as having an unbound
identifier the example
@;
@racketblock[
  (define-syntax-rule (def-m m orig-x)
    (define-syntax-rule (m)
      (begin
        (define orig-x 2)
        x)))
  (def-m m x)
  (m)
]
@;
The set-of-scopes expander accepts this example. The set-of-scopes
expander is arguably more consistent, considering that both expanders
allow the reference with other combinations of introductions for the
definition and reference:
@;
@racketblock[  
  (define-syntax-rule (def-m m)
    (define-syntax-rule (m)
      x))
  (define x 2)
  (def-m m)
  (m)

  (define-syntax-rule (def-m m orig-x)
    (begin
      (define orig-x 2)
      (define-syntax-rule (m)
        x)))
  (def-m m x)
  (m)

  (define-syntax-rule (def-m m)
    (define-syntax-rule (m orig-x)
      (begin
        (define orig-x 2)
        x)))
  (def-m m)
  (m x)
]
@;
}@;
So far, we have not encountered a practical example that exposes the
difference between the expanders' treatment of pattern-based macros in
definition contexts.
