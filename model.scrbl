#lang scribble/base
@(require racket/list
          "common.rkt"
          (prefix-in core: (submod "model/core-model.rkt" pict))
          (prefix-in phases: (submod "model/phases-model.rkt" pict))
          (prefix-in local: (submod "model/local-model.rkt" pict))
          (prefix-in defs: (submod "model/defs-model.rkt" pict)))

@(define model.zip @linkfile["model.zip"])

@title[#:tag "model"]{Model}

We present a formal model of set-of-scope expansion following the
style of @citet[expmodel].
@html-only{@note{Complete models, both in typeset form and executable form using PLT Redex, are available as @|model.zip|.}}@;
As a first step, we present a model where
only run-time expressions are expanded, and implementations of macros
are simply parsed. As a second step, we generalize the model to
include phase-specific scope sets and macro expansion at all phases.
The third step adds support for local expansion, and the fourth step
adds first-class definition contexts.
@;
The model does not cover modules or top-level namespaces.

@; ----------------------------------------
@section{Single-Phase Expansion}

Our macro-expansion model targets a language that includes with
variables, function calls, functions, atomic constants, lists, and
syntax objects:@html-note{This model is @filepath{core-model.rkt} in @|model.zip|.}
@;
@nested[#:style 'code-inset]{@core:eval-language-pict}
@;
Since the model is concerned with macro expansion and programmatic
manipulation of program terms, we carefully distinguish among
@;
@itemlist[

 @item{@defterm{names}, which are abstract tokens;}

 @item{@defterm{variables}, which correspond to function arguments and
       references in an AST and are formed by wrapping a name as
       @core:tm[(Var nam)];}

 @item{@defterm{symbols}, which are values with respect to the
       evaluator and are formed by prefixing a name with a quote; and}
 
 @item{@defterm{identifiers}, which are also values, are formed by
       combining a symbol with a set of scopes, and are a subset of
       @defterm{syntax objects}.}

]
@;
For a further explanation of the distinctions among these different
 uses of names, see @citet[(in-bib expmodel ", section 3.2.1")].

The model's evaluator is standard and relies on a @core:tm[δ] function
to implement primitives:
@;
@nested[#:style 'code-inset]{@core:eval-pict}
@;
Interesting primitives include the ones that manipulate syntax objects,
@;
@nested[#:style 'code-inset]{@core:prim-language-pict}
@;
where @core:tm[SE] extracts the content of a syntax object, and
@core:tm[MKS] creates a new syntax object with a given content and
the scopes of a given existing syntax object:
@;
@nested[#:style 'code-inset]{@core:δ-pict}

Macro expansion takes a program that is represented as as a syntax
object and produces a fully expanded syntax object. To evaluate the
program, the syntax object must be parsed into an AST. The parser uses
a @core:tm[resolve] metafunction that takes an identifier and a binding store,
@core:tm[Σ]. The names @core:tm[lambda], @core:tm[quote], and
@core:tm[syntax], represent the core syntactic forms, along with the
implicit forms of function calls and variable reference:
@;
@nested[#:style 'code-inset]{@core:parse-pict}
@;
The @core:tm[resolve] metafunction extracts an identifier's name
and its binding context. For now, we ignore phases and define a
binding context as simply a set of scopes. A binding store maps a name
to a mapping from scope sets to bindings, where bindings are
represented by fresh names.
@;
@nested[#:style 'code-inset]{@core:resolve-language-pict}
@;
The @core:tm[resolve] metafunction uses these pieces along with a
@core:tm[biggest-subset] helper function to select a binding. If
no binding is available in the store, the identifier's symbol's name
is returned, which effectively allows access to the four primitive
syntactic forms; the macro expander will reject any other unbound
reference.
@;
@nested[#:style 'code-inset]{@core:resolve-pict}


Finally, we're ready to define the @core:tm[expand] metafunction.
In addition to a syntax object (for a program to expand) and a binding
store, the expander needs an environment, @core:tm[env], that maps
bindings to compile-time meanings. The
possible meanings of a binding are the three primitive syntactic forms
recognized by @core:tm[parse], the @core:tm[let-syntax]
primitive form, a reference to a function argument, or a compile-time
value---where a compile-time function represents a macro transformer.
@;
@nested[#:style 'code-inset]{@core:expand-language-pict}
@;
The process of macro expansion creates new bindings, so the
@core:tm[expand] metafunction produces a tuple containing an
updated binding store along with the expanded program. For example, the simplest
case is for the @core:tm[quote] form, which leaves the body of the form
and the store as-is:
@;
@nested[#:style 'code-inset]{@core:expand-quote-pict}
@;
Since we are not yet dealing with expansion of compile-time terms, no
scope pruning is needed for @core:tm[syntax], and it can be essentially the
same as @core:tm[quote].
@;
@nested[#:style 'code-inset]{@core:expand-syntax-pict}
@;
Expansion of a @core:tm[lambda] form creates a fresh name and fresh
scope for the argument binding. Adding the new scope to the formal
argument (we define the @core:tm[add] metafunction later) creates
the binding identifier. The new binding is added to the store,
@core:tm[Σ], and it is also recorded in the compile-time environment,
@core:tm[env], as a variable binding. The body of the function is
expanded with those extensions after receiving the new scope, and the pieces are
reassembled into a @core:tm[lambda] form.
@;
@nested[#:style 'code-inset]{@core:expand-lambda-pict}
@;
When the generated binding is referenced (i.e., when resolving an
identifier produces a binding that is mapped as a variable), then the
reference is replaced with the recorded binding, so that the reference
is @racket[bound-identifier=?] to the binding in the expansion result.
@;
@nested[#:style 'code-inset]{@core:expand-var-pict}
@;
A local macro binding via @core:tm[let-syntax] is similar to
an argument binding, but the compile-time environment records a
macro transformer instead of a variable. The transformer is
produced by using @core:tm[parse] and then @core:tm[eval] on the
compile-time expression for the transformer. After the body is expanded,
the macro binding is no longer needed, so the body expansion is the
result.
@;
@nested[#:style 'code-inset]{@core:expand-let-syntax-pict}
@;
Finally, when the expander encounters an identifier that resolves to a
binding mapped to a macro transformer, the transformer is applied to the
macro use. Fresh scopes are generated to represent the use site, @core:tm[scp_u],
and introduced syntax, @core:tm[scp_i], where the introduced-syntax scope is applied
using @core:tm[flip] to both the macro argument and result, where
@core:tm[flip] corresponds to an exclusive-or operation to leave the
scope intact on syntax introduced by the macro (see below).
@;
@nested[#:style 'code-inset]{@core:expand-macro-app-pict}
@;
The only remaining case of @core:tm[expand] is to recur for
function-call forms, threading through the binding store using
an accumulator-style @core:tm[expand*] helper:
@;
@nested[#:style 'code-inset]{@core:expand-app-pict}
@;
For completeness, here are the @core:tm[add] and @core:tm[flip] metafunctions for
propagating scopes to all parts of a syntax object, where
@core:tm[(addremove scp ctx)] adds @core:tm[scp] to @core:tm[ctx] if
is not already in @core:tm[ctx] or removes it otherwise:
@;
@nested[#:style 'code-inset]{@core:add+flip-pict}

To take a program from source to value, use @core:tm[expand],
then @core:tm[parse], then @core:tm[eval].

@; ----------------------------------------
@section{Multi-Phase Expansion}

To support phase-specific scope sets, we change the definition of
@phases:tm[ctx] so that it is a mapping from phases to scope
sets:@html-note{This model is @filepath{phases-model.rkt} in @|model.zip|.}
@;
@nested[#:style 'code-inset]{@phases:language-delta-pict}
@;
With this change, many metafunctions must be indexed by the current
phase of expansion. For example, the result of @phases:tm[resolve]
depends on the current phase:
@;
@nested[#:style 'code-inset]{@phases:resolve-pict}
@;
Phase-specific expansion allows @phases:tm[let-syntax]
to expand the compile-time expression for a macro implementation,
instead of just parsing the expression. Note that the uses of
@phases:tm[expand] and @phases:tm[parse] on the transformer
expression are indexed by @phases:tm[(plus ph 1)]:
@;
@nested[#:style 'code-inset]{@phases:expand-let-syntax-pict}
@;
In addition to carrying a phase index, the revised @phases:tm[expand]
takes a set of scopes created for bindings. Those scopes are the ones
to be pruned from quoted syntax by the revised @phases:tm[syntax]
expansion:
@;
@nested[#:style 'code-inset]{@phases:expand-syntax-pict}
@;
The @phases:tm[prune] metafunction recurs through a syntax object
to remove all of the given scopes at the indicated phase:
@;
@nested[#:style 'code-inset]{@phases:prune-pict}


@; ----------------------------------------
@section{Local Expansion}

Environment inspection via @racket[syntax-local-value]
and local expansion via @racket[local-expand] are accommodated in
the model essentially as in @citet[expmodel], but since local
expansion can create bindings, the @local:tm[eval] metafunction
must consume and produce a binding store. The @local:tm[eval]
metafunction also must be index by the phase used for syntax
operations.

Local expansion needs the current macro expansion's introduction
scope, if any. In addition, local expansions that move identifiers
into binding positions need
@racket[syntax-local-identifier-as-binding], which requires
information about scopes in the current expansion context. Local
expansion, meanwhile, can create new such scopes. To support those
interactions, @local:tm[eval] and @local:tm[expand] must
both consume and produce scope sets for the current use-site scopes,
and binding scopes must also be available for local expansion of
@local:tm[syntax] forms. To facilitate threading through all of that
information, we define @local:tm[maybe-scp] as an optional current
scope and @local:tm[Σ*] as an extended store:@html-note{This
model is @filepath{local-model.rkt} in @|model.zip|.}
@;
@nested[#:style 'code-inset]{@local:language-delta-pict}
@;
The second part of a @local:tm[Σ*] tuple is a set of scopes to be
pruned at @local:tm[syntax] forms. The third part is a subset of
those scopes that are the current expansion context's use-site scopes,
which are pruned by @racket[syntax-local-identifier-as-binding].
The different parts of a @local:tm[Σ*] tuple vary in different ways:
the @local:tm[Σ] part is consistently threaded through evaluation and
expansion, while the scope-set parts are stack-like for expansion
and threaded through evaluation. In the case of a macro-application
step, the scope-set parts of the tuple are threaded through expansion,
too, more like evaluation.

In the model, the @local:tm[LOCAL-VALUE], @local:tm[LOCAL-EXPAND], and
@local:tm[LOCAL-BINDER] primitives represent
@racket[syntax-local-value], @racket[local-expand], and
@racket[syntax-local-identifier-as-binding], respectively:
@;
@nested[#:style 'code-inset]{@local:eval-pict}
@;

The implementation of @local:tm[LOCAL-EXPAND] uses a new
@local:tm[(TStop transform)] transformer to make an identifier a
stopping point for expansion while remembering the former
@local:tm[transform] mapping of the identifier. The @local:tm[unstop]
helper function strips away a @local:tm[TStop] constructor:
@;
@nested[#:style 'code-inset]{@local:unstop-pict}
@;
The expander must recognize @local:tm[TStop] transformers to
halt expansion at that point:
@;
@nested[#:style 'code-inset]{@local:expand-stop-pict}
@;
The revised macro-application rule for @local:tm[expand] shows
how the use-site scopes component of @local:tm[Σ*] is
updated and how the current application's macro-introduction
scope is passed to @local:tm[eval]:
@;
@nested[#:style 'code-inset]{@local:expand-macro-app-pict}
@;
In contrast, the revised @local:tm[lambda] rule shows how the pruning
scope set is extended for expanding the body of the function, the
use-site scope set is reset to empty, and all extensions are discarded
in the expansion's resulting store tuple.
@;
@nested[#:style 'code-inset]{@local:expand-lambda-pict}

@; ----------------------------------------
@section{First-Class Definition Contexts}

Supporting first-class definition contexts requires no further changes
to the @defs:tm[expand] metafunction, but the @defs:tm[eval]
metafunction must be extended to implement the @defs:tm[NEW-DEFS] and
@defs:tm[DEF-BIND] primitives, which model the
@racket[syntax-local-make-definition-context] and
@racket[syntax-local-bind-syntaxes] functions. @html-note{This model is
@filepath{defs-model.rkt} in @|model.zip|.}

The @defs:tm[NEW-DEFS] primitive allocates a new scope to represent
the definition context, and it also allocates a mutable reference to a
compile-time environment that initially references the current
environment. The two pieces are combined with a @defs:tm[Defs] value
constructor:
@;
@nested[#:style 'code-inset]{@defs:eval-new-defs-pict}
@;
The @defs:tm[DEF-BIND] primitive works in two modes. In the first mode,
it is given only a definition context and an identifier, and it creates
a new binding for the identifier that includes the definition context's
scope. The new binding is mapped to variable in an updated environment
for definition context:
@;
@nested[#:style 'code-inset]{@defs:eval-def-bind-var-pict}
@;
When @defs:tm[DEF-BIND] is given an additional syntax object, it
expands and evaluates the additional syntax object as a compile-time
expression, and it adds a macro binding to the definition context's
environment:
@;
@nested[#:style 'code-inset]{@defs:eval-def-bind-syntax-pict}
@;
Note that @defs:tm[DEF-BIND] in this mode defines a potentially
recursive macro, since the definition context's scope is added to
compile-time expression before expanding and
parsing it.

Finally, a definition context is used to expand an expression by
providing the definition context as an extra argument to
@defs:tm[LOCAL-EXPAND]. The implementation of the new case for
@defs:tm[LOCAL-EXPAND] is similar to the old one, but the definition
context's scope is applied to the given syntax object before expanding
it, and the definition context's environment is used for expansion.
@;
@nested[#:style 'code-inset]{@defs:eval-local-expand-pict}
