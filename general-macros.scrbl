#lang scribble/base
@(require racket/list
          "common.rkt"
          (for-label racket/block))

@title[#:tag "general-macros"]{@(if to-pdf? "" "Scope Sets for ")Procedural Macros and Modules}

Although our set-of-scopes expander resolves bindings differently than
in previous models, it still works by attaching information to
identifiers, and so it can provide a smooth path from pattern-matching
macros to procedural macros in the same way as
@racket[syntax-case]@~cite[syntax-case]. Specifically,
@racket[(@#,racket[syntax] _form)] quotes the S-expression @racket[_form]
while preserving its scope-set information, so that @racket[_form] can
be used to construct the result of a macro.

More precisely, a primitive @racket[(quote-syntax _form)] quotes
@racket[_form] with its scope sets in Racket. The derived
@racket[(@#,racket[syntax] _form)] detects uses of pattern variables
and replaces them with their matches while quoting any non-pattern
content in @racket[_form] with @racket[quote-syntax]. A
@racket[(@#,racket[syntax] _form)] can be abbreviated
@racket[#'_form], and when @racket[_form] includes no pattern variables,
@racket[#'_form] is equivalent to @racket[(quote-syntax _form)].
@;
The quaisquoting variant @racket[#`_form] (which uses a backquote instead of
a regular quote) allows escapes within @racket[_form] as
@RACKET[#,_expr], which inserts the result of evaluating
@racket[_expr] in place of the escape.

The result of a @racket[quote-syntax] or @racket[syntax] form is a
@defterm{syntax object}. When a syntax object's S-expression component
is just a symbol, then the syntax object is an @defterm{identifier}.

@; --------------------------------------------------
@section{Identifier Comparisons with Scope Sets}

Various compile-time functions work on syntax objects and identifiers.
Two of the most commonly used functions are @racket[free-identifier=?]
and @racket[bound-identifier=?], each of which takes two identifiers.
The @racket[free-identifier=?] function is used to recognize a
reference to a known binding, such as recognizing a use of
@racket[else] in a conditional. The @racket[bound-identifier=?]
function is used to check whether two identifiers would conflict as
bindings in the same context, such as when a macro that expands to a
binding form checks that identifiers in the macro use are suitably
distinct.

These two functions are straightforward to implement with scope sets.
A @racket[free-identifier=?] comparison on identifiers checks whether
the two identifiers have the same binding by consulting the global
binding table. A @racket[bound-identifier=?] comparison checks that
two identifiers have exactly the same scope sets, independent of the
binding table.

@aside{

Note that @racket[(bound-identifier=? _x _y)] does not completely
answer the question ``would @racket[_x] bind @racket[_y]?'' A
@racket[#t] result answers that question in the affirmative, but
@racket[_x] might bind @racket[_y] even if the result is @racket[#f].
The same is true in Racket's old macros system as well
as implementations like Chez Scheme, which (like the set-of-scopes
expander) print @racket[#f] but produce @racket[1] for the following
example:
@;
@racketblock[
(let ()
  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ a b)
       (begin
         (printf "~s\n" (bound-identifier=? #'a #'b))
         #'(begin
             (define a 1)
             b))]))
  (define-syntax n
    (syntax-rules ()
      [(_ id) (m id x)]))
  (n x))
]


}

@; --------------------------------------------------
@section{Local Bindings and Syntax Quoting}

The set-of-scopes approach to binding works the same as previous
models for macros that are purely pattern-based,
but the set-of-scopes approach makes finer distinctions among identifiers than
would be expected by existing procedural Racket macros that use
@racketmetafont{@literal{#'}} or @racket[quote-syntax]. To be
consistent with the way that Racket macros have been written,
@racket[quote-syntax] must discard some scopes.

For example, in the macro
@;
@RACKETBLOCK[
(lambda (stx)
  (let ([id #'x])
     #`(let ([x 1])
         #,id)))
]
@;
the @racket[x] that takes the place of @RACKET[#,id] should refer to
the binding @racket[x] in the generated  @racket[let] form. The
@racket[x] identifier that is bound to @racket[id], however, is not in the scope
that is created for the compile-time @racket[let]:
@;
@RACKETBLOCK[
(lambda (@UNSYNTAX[@id[stx s_1lm]])
  (let ([@UNSYNTAX[@id[id s_1lm s_2lt]] #'@(UNSYNTAX @id[x s_1lm])])
     #`(let ([@UNSYNTAX[@id[x s_1lm s_2lt]] 1])
         #, @UNSYNTAX[@id[id s_1lm s_2lt]])))
]
@;
If @racket[quote-syntax] (implicit in @racketmetafont{@literal{#`}}) preserves all scopes on
an identifier, then with set-of-scopes binding, the @racket[x] that
replaces @RACKET[#,id] will not refer to the @racket[x] in the
generated @racket[let]'s binding position.

It's tempting to think that the compile-time @racket[let] should
introduce a phase-specific scope that applies only for compile-time references, in
which case it won't affect @racket[x] as a run-time reference. That
adjustment doesn't solve the problem in general, since a macro can
generate compile-time bindings and references just as well as run-time
bindings and references.

A solution is for the expansion of @racket[quote-syntax] to discard
certain scopes on its content. The discarded scopes are those from
binding forms that enclosed the @racket[quote-syntax] form up to a
phase crossing or module top-level, as well as any use-site scopes
recorded for macro invocations within those binding forms. In the
case of a @racket[quote-syntax] form within a macro binding's
right-hand side, those scopes cover all of the scopes introduced on
the right-hand side of the macro binding.

The resulting macro system is different than the old Racket macro
system. Experiments suggest that the vast majority of macro
implementations work either way, but it's easy to construct an example
that behaves differently:
@;
@racketblock[
(free-identifier=? (let ([x 1]) #'x)
                   #'x)
]
@;
In Racket's old macro system, the result is @racket[#f].
The set-of-scopes system with a scope-pruning @racket[quote-syntax]
produces @racket[#t], instead, because the @racket[let]-generated
scope is stripped away from @racket[#'x].

@aside{

Note: Racket's macro system matches @citet[syntax-case], where both
@racket[free-identifier=?] and @racket[bound-identifier=?] produce
@racket[#f] for the above arguments, and @racket[bound-identifier=?]
always implies @racket[free-identifier=?]. The current @exec{psyntax}
implementation, as used by Chez Scheme and other implementations and
as consistent with @citet[essence-of-hygiene], produces @racket[#f]
and @racket[#t] for @racket[free-identifier=?] and
@racket[bound-identifier=?], respectively; as the example illustrates,
@racket[bound-identifier=?] does not imply @racket[free-identifier=?].
The set-of-scopes system produces @racket[#t] and @racket[#t] for
@racket[free-identifier=?] and @racket[bound-identifier=?],
respectively, and @racket[bound-identifier=?] always implies
@racket[free-identifier=?].

}

If @racket[quote-syntax] did not prune scopes, then not only would the
result above be @racket[#f], but @racket[bound-identifier=?] would produce
@racket[#f] for both @racket[(let ([x 1]) #'x)] and @racket[(let ([y 1]) #'x)]. Those
results reflect the switch to attaching identifier-independent
scopes to identifiers, instead of attaching identifier-specific
renamings.

Arguably, the issue here is the way that pieces of syntax from
different local scopes are placed into the same result syntax object,
with the expectation that all the pieces are treated the same way. In
other words, Racket programmers have gotten used to an unusual variant
of @racket[quote-syntax], and most macros could be written just as
well with a non-pruning variant.@narrow-audience-only{Then again, the pruning variant of
@racket[quote-syntax] tends to discard information about local
bindings that is usually unwanted but preserved by the old
@racket[quote-syntax].}

@aside{

There's precedent for a variant of @racket[syntax-case] that does not
support assembling pieces as in the example. An early version of van
Tonder's macro expander@~cite[van-tonder] had that property as a
result of making the evaluation of @racket[syntax] generate a fresh
context.

}

Supplying a second, non-pruning variant of @racket[quote-syntax]
poses no problems. Our set-of-scopes implementation for Racket implements the
non-pruning variant when a @racket[#:local] keyword is added to a
@racket[quote-syntax] form. For example,
@;
@(if (not narrow-column?)
@racketblock[
(free-identifier=? (let ([x 1]) (quote-syntax x #:local))
                   (quote-syntax x #:local))
]
@racketblock[
(free-identifier=? (let ([x 1])
                     (quote-syntax x #:local))
                   (quote-syntax x #:local))
])
@;
produces @racket[#f] instead of @racket[#t], because the scope
introduced by @racket[let] is preserved in the body's syntax object.
The non-pruning variant of @racket[quote-syntax] is useful for
embedding references in a program's full expansion that are meant to
be inspected by tools other than the Racket compiler; Typed Racket's
implementation uses the @racket[#:local] variant of
@racket[quote-syntax] to embed type declarations (including
declarations for local bindings) in a program's expansion for use by
its type checker.

@; --------------------------------------------------
@section[#:tag "nonempty-scope"]{Ensuring Distinct Bindings}

A Racket macro's implementation can arrange for an identifier
introduced by a macro expansion to have an empty scope set.@note{Avoiding
a macro-introduction scope involves using a
@racket[syntax-local-introduce] function.} More generally, a macro can
arrange for identifiers that are introduced in different contexts to
have the same symbol and scope set.
@;
If those identifiers appear as bindings via @racket[lambda],
@racket[let], or @racket[let-syntax], then the new scope created for the
binding form will ensure that the different identifiers
produce different bindings. That is, the binding scope is always
created after any expansion that introduced the bound identifier,
so all bindings are kept distinct by those different binding scopes.

For example, assuming that @racket[make-scopeless] creates an
identifier that has no scopes in an expansion, then the
@racket[let-x] forms in
@;
@RACKETBLOCK[
(define-syntax (let-x stx)
  (syntax-case stx ()
    [(_ rhs body)
     #`(let ([#,(make-scopeless 'x) rhs])
         body)]))

(let-x 5
  (let-x 6
    0))
]
@;
create intermediate @racket[x] identifiers that each have an empty scope set,
but the full expansion becomes
@;
@racketblock[
(let ([@#,id[x s_1lt] 5])
  (let ([@#,id[x s_2lt] 6])
    0))
]
@;
where @|s_1lt| and @|s_2lt| are created by each @racket[let] (as a primitive
binding form), and they distinguish the different @racket[x] bindings.

In a definition context (see @secref["intdef"]), macro expansion can
introduce an identifier to a binding position @emph{after} the scope
for the definition context is created (and after that scope is applied
to the definition context's original content). That ordering risks a
collision among bindings in different definition contexts, where
identifiers introduced into different definition contexts all have the
same symbol and set of scopes.

For example, using a @racket[block] form that creates a definition
context and that we treat here as a primitive form, the uses of
@racket[def-x] in
@;
@RACKETBLOCK[
(define-syntax (def-x stx)
  (syntax-case stx ()
    [(_ rhs)
     #`(define #,(make-scopeless 'x) rhs)]))

(block
  (define y 1)
  (def-x 5))
(block
  (define y 2)
  (def-x 6))
]
@;
risk expanding as
@;
@racketblock[
(block
  (define @#,id[y s_1def] 1)
  (define @#,id[x] 5))
(block
  (define @#,id[y s_2def] 2)
  (define @#,id[x] 6))
]
@;
with conflicting bindings of @racket[x] for the empty scope set.

To avoid the possibility of such collisions, in a definition context that
supports both definitions and macro expansion, the context is
represented by a pair of scopes: an @deftech{outside-edge scope} that is
added to the original content of the definition context, and an
@deftech{inside-edge scope} that is added to everything that appears in the
definition context through macro expansion. The outside-edge scope
distinguishes original identifiers from macro-introduced identifiers,
while the inside-edge scope ensures that every binding created for
the definition context is distinct from all other bindings.

Thus, the preceding example expands as
@;
@racketblock[
(block
  (define @#,id[y s_1out s_1in] 1)
  (define @#,id[x s_1in] 5))
(block
  (define @#,id[y s_2out s_2in] 2)
  (define @#,id[x s_2in] 6))
]
@;
where the @tech{inside-edge scopes} @|s_1in| and @|s_2in| distinguish
the two @racket[x] bindings. Meanwhile, if the definitions of @racket[y]
instead used the name @racket[x], they would remain distinguished from
the macro-introduced @racket[x]s by the @tech{outside-edge scopes}
@|s_1out| and @|s_2out|.

@; --------------------------------------------------
@section[#:tag "fc-intdef"]{First-Class Definition Contexts}

Racket exposes the expander's support for definition contexts (see
@secref["intdef"]) so that new macros can support definition contexts
while potentially changing the meaning of a macro or variable
definition. For example, the @racket[class] macro allows local macro
definitions in the @racket[class] body while it rewrites specified
function definitions to methods and other variable definitions to
fields. The @racket[unit] form similarly rewrites variable definitions
to a mixture of private and exported definitions with a component.

Implementing a definition context starts with a call to
@racket[syntax-local-make-definition-context], which creates a
first-class (at compile time) value that represents the definition context.
A macro can force expansion of forms in the definition
context, it can add variable bindings to the definition context, and
it can add compile-time bindings and values that are referenced by
further macro expansion within the definition context.
To a first approximation, a first-class definition context corresponds
to an @tech{inside-edge scope} that is added to any form expanded within the definition
context and that houses the definition context's bindings. A
definition context also has a compile-time environment frame
(extending the context of the macro use) to house the mapping of
bindings to variables and compile-time values.

Like other definition contexts (see @secref["intdef"]), the
compile-time environment must track use-site scopes that are generated
for macro expansions within a first-class definition context. If the
macro moves any identifier into a binding position in the overall
expansion, then the macro normally must remove accumulated use-site
scopes (for the current definition context only)
by applying @racket[syntax-local-identifier-as-binding] to the
identifier. For example, the @racket[unit] form implements a
definition context that is similar to the body of a @racket[lambda],
but variables are internally transformed to support mutually recursive
references across unit boundaries.
@;
@racketblock[
(unit (import)
      (export)
 (define x 1)
 x)
]
@;
In this example, @racket[(define x 1)] is expanded to @racket[(define-values
(x) 1)] with a use-site scope on @racket[x], but the intent is for
this definition of @racket[x] to capture the reference at the end of
the @racket[unit] form. If the @racket[unit] macro simply moved the
binding @racket[x] into a @racket[letrec] right-hand side, the
@racket[x] would not capture the final @racket[x] as moved into the
@racket[letrec] body; the use-site scope on the definition's
@racket[x] would prevent it from capturing the use. The solution is
for the @racket[unit] macro to apply
@racket[syntax-local-identifier-as-binding] to the definition's
@racket[x] before using it as a @racket[letrec] binding. Macros that
use a definition context and @racket[bound-identifier=?] must
similarly apply @racket[syntax-local-identifier-as-binding] to
identifiers before comparing them with @racket[bound-identifier=?].

Even if a macro does not create a first-class definition context, some
care is needed if a macro forces the expansion of subforms and moves
pieces of the result into binding positions. Such a macro probably should not use
@racket[syntax-local-identifier-as-binding], but it should first
ensure that the macro use is in an expression context before forcing
any subform expansions. Otherwise, the subform expansions could
interact in unexpected ways with the use-site scopes of an
enclosing definition context.

Use-site scopes associated with a first-class definition context are
not stored directly in the compile-time environment frame for the
definition context. Instead, they are stored in the closest frame that
is not for a first-class definition context, so that the scopes are
still tracked when the definition context is discarded (when the macro
returns, typically). The scope for the definition context itself is
similarly recorded in the closest such frame, so that
@racket[quote-syntax] can remove it, just like other binding scopes.

@; ----------------------------------------
@; Omit remaining sections for a wide audience:
@(if wide-audience?
     null
     @list{
     
@; --------------------------------------------------
@section[#:tag "Rename Transformers"]{Rename Transformers}

Racket's macro API includes support for binding aliases through
@defterm{rename transformers}. A compile-time binding to the result of
@racket[make-rename-transformer] is similar to a binding to a macro
transformer that replaces the binding's identifier with the aliased
identifier. In addition, however, binding to a rename transformer
causes @racket[free-identifier=?] to report @racket[#t] for the
original identifier and its alias.

With set-of-scopes binding, a binding alias is supported through an
extension of the binding table. The mapping from a
@tuple[@elem{symbol} @elem{scope set}] pair is to a
@tuple[@elem{binding} @elem{maybe-aliased}] pair, where an
@elem{maybe-aliased} is either empty or another identifier (i.e., a
symbol and scope set) to which the mapped identifier should be
considered @racket[free-identifier=?]. When a transformer-binding form
such as @racket[define-syntax] or @racket[letrec-syntax] detects that
the value to be installed for a binding as a rename transformer, it
updates the binding table to register the identifier within the
transformer as an @elem{optional-alias}.

The implementation of @racket[free-identifier=?] must follow alias
chains. Cycles are possible, and they cause the aliased identifier to
be treated as unbound.

@; ----------------------------------------
@; Omit remaining sections for a wide audience:
})

@; --------------------------------------------------
@section{Modules and Phases}

The @racket[module] form creates a new scope for its body. More
precisely, a @racket[module] form creates an @tech{outside-edge
scope} and an @tech{inside-edge scope}, like any other context
that allows both definitions and macro expansion.

A @racket[(module* _name #f ....)] submodule form, where @racket[#f]
indicates that the enclosing module's bindings should be visible,
creates an additional scope in the obvious way. For other
@racket[module*] and @racket[module] submodule forms, the macro
expander prevents access to the enclosing module's bindings by
removing the two scopes of the enclosing module.

A module distinguishes bindings that have the same name but
different phases. For example, @racket[lambda] might have one meaning
for run-time code within a module, but a different meaning for
compile-time code within the same module. Furthermore, instantiating a
module at a particular phase implies a phase shift in its syntax
literals. Consider the module
@;
@racketblock[
(define x 1)
(define-for-syntax x 2)

(define id #'x)
(define-for-syntax id #'x)

(provide id (for-syntax id))
]
@;
and suppose that the module is imported both normally and for compile
time, the latter with a @racketidfont{s:} prefix. In a compile-time
context within the importing module, both @racket[id] and
@racket[s:id] will be bound to an identifier @racket[x] that had the
same scopes originally, but they should refer to different @racket[x]
bindings (in different module instances with different values).

Among the possibilities for distinguishing phases, having per-phase
sets of scopes on an identifier makes the phase-shifting operation
most natural. A local binding or macro expansion can add scopes at all
phases, while @racket[module] adds a distinct @tech{inside-edge scope} to
every phase (and the same @tech{outside-edge scope} to all phases). Since
every binding within a module is forced to have that module's
phase-specific @tech{inside-edge scopes}, bindings at different scopes will
be appropriately distinguished.

@aside{

Racket constrains operations that inspect and adjust scopes on syntax
objects to those that add, remove, or flip sets of scopes relative to
some other syntax object. As a result, all of the phase-specific
scopes for a module's inside edge are added to or removed from a syntax
object together.

}

Having a distinct ``root'' scope for each phase makes most local
bindings phase-specific. That is, in
@;
@racketblock[
(define-for-syntax x 10)
(let ([x 1])
  (let-syntax ([y x])
    ....))
]
@;
the @racket[x] on the right-hand side of @racket[let-syntax] sees the
top-level phase-1 @racket[x] binding, not the phase-0 local binding.
This is a change from Racket's old approach to binding and phases,
but the only programs that are affected are ones that would trigger an
out-of-context error in the old system. Meanwhile, macros can
construct identifiers that have no module scope, so out-of-context errors
are still possible.

@; ----------------------------------------
@; Omit remaining sections for a wide audience:
@(if wide-audience?
     null
     @list{
     
@; ----------------------------------------
@section[#:tag "top-level"]{The Top Level}

A @deftech{namespace} in Racket is a top-level evaluation context.
Each call to @racket[eval] uses a particular namespace (either the
@deftech{current namespace} or one supplied to @racket[eval]), and
each @racket[read]--@racket[eval]--@racket[print] loop works in a
particular namespace. Namespaces are first-class values in Racket. A
namespace can be created as fresh (e.g., for a sandbox), or it can be extracted from a
module instantiation to simulate further evaluation in the module's
body.

As the connection to modules may suggest, a top-level namespace
corresponds to a pair of scopes in the same way that a module has a
scope. Each top-level namespace has the same @tech{outside-edge scope}, but a
distinct @tech{inside-edge scope} where bindings reside.

@narrow-audience-only{

The interactive and incremental nature of a top-level context poses
certain semantic challenges when macro and variable definitions and
re-definitions are allowed. For example, a reference to an unbound
identifier within a function cannot be rejected out-of-hand, because
it might be defined later within the namespace before the function is
called. Similarly, a reference might be resolved as a variable when a
function is created, but a later definition could change the
identifier's binding to a macro, so the function must either continue
to refer to a variable or be somehow reinterpreted to have a macro
use. These challenges are compounded when macros expand to a mixture
of variable and macro definitions. Overall, the top level is hopeless:
it cannot provide a treatment of binding that is as consistent as
@racket[module] while also performing its job as an interactive,
exploratory evaluation context. In Racket, we accept top-level
compromises and put all ``real'' code in modules.

Fortunately, top-level compromises pose little trouble for set-of-scopes
binding. Supporting an incremental and redefinition-capable top-level
context requires only that the binding table allow updates of existing
bindings, which is straightforward.

}

A @narrow-audience-only{more} troublesome aspect of top-level namespaces in Racket is that a
form might be captured (via @racket[quote-syntax]), expanded, or
compiled in one namespace, and then evaluated in another namespace.
Historically, top-level bindings have been equated with ``unbound,''
so that expanded and compiled forms originating in a top-level context
could move freely among namespaces. This treatment as ``unbound'' has
been fuzzy, however, and forms that originate from module namespaces
have been treated differently from forms that originate in a
non-module namespace.

To accommodate top-level namespaces with as much consistency (of
binding treatment) and convenience (of moving forms among top-level
namespaces) as possible, we introduce one more dimension to syntax
objects. Instead of having a single set of scopes per phase, each
syntax object has a sequence of scope sets per phase. When a syntax
object is introduced to a top-level context that is not already
included in its scope set (at a gven phase), the current scope set is
cloned as a new first item of the list of sets; all further scope-set
manipulations affect that first item. When looking up an identifier's
binding, however, the sequence is traversed until a binding is found.
In other words, all but the first item in the list act as fallbacks
for locating a binding. In practice, this fallback mechanisms is
consistent with most existing code without otherwise interfering with
scope management (since the fallbacks apply only when an identifier is
otherwise unbound).

@; ----------------------------------------
@section{The Syntax-Function Zoo}

Compared to @citet[syntax-case] or even @citet[expmodel], Racket adds
many functions for manipulating syntax objects during macro expansion
in ways that are sensitive to the expansion context. We have mentioned
first-class definition context and rename transformers, but Racket
provides many more tools:

@itemlist[

 @item{The @racket[syntax-local-introduce] function lets a macro
       explicitly toggle the introduction status of a syntax object by
       flipping the mark (specific to the current macro invocation) that
       distinguishes use-site and macro-introduced identifiers.

       With the set-of-scopes expander, the mark is replaced by a
       scope, and @racket[syntax-local-introduce] flips both the
       introduction scope and the use-site scope (if any) of the
       current expansion.}

 @item{The @racket[make-syntax-introducer] function generates a
       function that works like @racket[syntax-local-introduce], but
       for a fresh mark/scope.

       As a new feature, and unlike @racket[syntax-local-introduce],
       the generated function accepts an additional argument to select
       the mode: @racket['flip] (the default) to flip the scope's
       presence, @racket['add] to add the scope if not present
       already, and @racket['remove] to remove the scope if it is
       currently present.}

 @item{The @racket[make-syntax-delta-introducer] function accepts two
       arguments, and it creates a function similar to the one
       produced by @racket[make-syntax-introducer], but instead of
       operating on a fresh mark/scope, it operates on all
       marks/scopes on the first syntax object that are not present on
       the second syntax object.

       With the set-of-scopes expander, the generated function accepts
       a @racket['flip], @racket['add], or @racket['remove] mode. This
       operation gives macro implementors relatively fine-grained
       control over scopes, but without exposing individual scopes, so
       the macro expander still can perform certain optimizations and
       make certain representation choices (e.g., due to the fact that
       the phase-specific ``inside'' scopes of a module are added or
       removed together).}

 @item{The questionable @racket[syntax-local-make-delta-introducer]
       function, which finds the difference between a reference and
       its binding so that the difference can be applied to another
       syntax object, is no longer needed, because it can be
       implemented with @racket[make-syntax-delta-introducer].

       Since @racket[make-syntax-delta-introducer] for the previous
       macro expander manipulated only marks, and not renamings, it
       was insufficient for certain kinds of scope transfer. Unifying
       all binding through scopes makes
       @racket[make-syntax-delta-introducer] sufficient.}

 @item{The @racket[syntax-local-get-shadower] function in the old
       expander acts as an especially heavy hammer for non-hygienic
       binding. It synthesizes an identifier like a given one that will
       capture any identifiers that are original to the current
       expansion context.

       The main use of @racket[syntax-local-get-shadower] is to
       implement syntax parameters@~cite[stxparam]. In the Racket
       set-of-scopes expander, @racket[syntax-local-get-shadower] has
       been simplified so that it effectively serves only as a hook
       for implementing syntax parameters, while other former uses are
       better and more consistently implemented through
       @racket[make-syntax-delta-introducer].

       @aside{Implementing syntax parameters as a core feature of the
       macro expander would be sensible and slightly cleaner. We
       maintain the @racket[syntax-local-get-shadower] approach only
       because it's simpler with our current infrastructure.}}

]

As mentioned in @secref["fc-intdef"], a first-class definition context
is difficult to specify in terms of renamings. In that case, an
internal-definition context is backed by a renaming on syntax objects,
but the renaming can refer to itself or other renamings, and so the
binding-resolution process must handle a complex form of cycles. With
set-of-scopes binding, an internal-definition context is backed by a
scope for the context; an internal-definition context doesn't create
cyclic syntax-object structures, and it needs no special rules for
resolving references to bindings.

@; ----------------------------------------
})