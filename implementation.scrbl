#lang scribble/base
@(require racket/list
          "common.rkt")

@title[#:tag "implementation"]{Implementation and Experience}

Scope sets have an intuitive appeal as a model of binding, but a true
test of the model is whether it can accommodate a Racket-scale use of
macros---for constructing everything from simple syntactic abstractions
to entirely new languages. Indeed, the set-of-scopes model
was motivated in part by a fraying of Racket's old macro expander at
the frontiers of its implementation, e.g., for
submodules@~cite[submodules].@note{For an example of a bug report about
submodules, see
@x-hyperlink["http://bugs.racket-lang.org/query/?debug=&database=default&cmd=view+audit-trail&cmd=view&pr=14521"]{problem report 14521}.
The example program fails with the old expander, due to
problems meshing mark-oriented module scope with renaming-oriented
local scope, but the example works with the set-of-scopes expander.}

We released the new macro expander as part of Racket version 6.3
(released November 2015), while Racket developers started using the
expander about four months earlier. Compared to the previous release,
build times, memory use, and bytecode footprint were essentially
unchanged compared to the old expander. Getting performance on par
with the previous system required about two weeks of performance tuning, which
we consider promising in comparison to a system that has been tuned
over the past 15 years.

@; ----------------------------------------
@section[#:tag "compat-init"]{Initial Compatibility Results}

At the time that Racket developers switched to the new expander,
packages in Racket's main distribution had been adjusted to build
without error (including all documentation), and most tests in the
corresponding test suite passed; 43 out of 7501 modules failed.
Correcting those failures before version 6.3 required small changes to
accommodate the new macro expander.

@html-only{@margin-note{See the
           @hyperlink["http://www.cs.utah.edu/plt/popl16/"]{POPL'16
           artifact} for a detailed record of the initial
           compatibility results.}}

Achieving the initial level of success required small changes to 15
out of about 200 packages in the distribution, plus several
substantial macro rewrites in the core package:
@;
@itemlist[

 @item{Changed macros in the core package include the @racket[unit],
       @racket[class], and @racket[define-generics] macros, all of
       which manipulate scope in unusual ways.}

 @item{The Typed Racket implementation, which is generally sensitive
       to the details of macro expansion, required a handful of
       adjustments to deal with changed expansions of macros and the
       new scope-pruning behavior of @racket[quote-syntax].}

 @item{Most other package changes involve languages implementations
       that generate modules or submodules and rely on a
       non-composable treatment of module scopes by the old
       expander (which creates trouble for submodules in other
       contexts). }

]
@;
In about half of all cases, the adjustments for set-of-scopes
expansion are compatible with the existing expander. In the other
half, the macro adjustments were incompatible with the previous
expander and the two separate implementations seem substantially
easier to produce than one unified implementation.

Besides porting the main Racket distribution to a set-of-scopes
expander, we tried building and testing all packages registered at
@url{http://pkgs.racket-lang.org/}. There were 46 failures out of about 400 packages, as opposed to
to 21 failures for the same set of packages with the previous
Racket release. @narrow-audience-only{Many failures involved
packages that implement non-S-expression @defterm{readers} and relied on
namespace-interaction details (as discussed in @secref["top-level"])
that change with scope sets; the language implementations were
adjusted to use a different technique that is compatible with both
expanders.@note{See the
@x-hyperlink["https://groups.google.com/d/msg/racket-dev/6khgHKygmS4/cDIfw5cimDEJ"]{discussion
on compatibility of a reader implementation on the Racket mailing
list}.}} All of those packages were repaired before the version 6.3
release.

@; ----------------------------------------
@; Omit some sections for a wide audience:
@(if wide-audience?
     null
     @list{

@; ----------------------------------------
@section[#:tag "compat"]{Longer-Term Compatibility Considerations}

As the initial experiments confirmed, most Racket programs expand and
run the same with a set-of-scope expander as with the old
expander. Pattern-based macros are rarely affected. When changes are needed to accommodate the
set-of-scopes expander, those changes often can be made compatible
with the existing expander. In a few cases,
incompatibilities appear unavoidable.

Macros that manipulate bindings or scope in unusual ways can easily
expose the difference between the macro systems.
@;
As an example, the following program produces @racket[1] with
Racket's old expander, but it provokes an ambiguous-binding error
with the set-of-scopes expander:
@;
@racketblock[
         (define-syntax-rule (define1 id)
          (begin
            (define x 1)
            (code:comment @#,elem{stash a reference to the introduced identifier:})
            (define-syntax id #'x)))
    
         (define-syntax (use stx)
           (syntax-case stx ()
             [(_ id)
              (with-syntax ([old-id (syntax-local-value #'id)])
                #'(begin
                    (define x 2)
                    (code:comment @#,elem{reference to @racket[old-id] ends up ambiguous:})
                    old-id))]))
    
         (define1 foo)
         (use foo)
]
@;
In the set-of-scopes model, @racket[define1] and @racket[use]
introduce bindings from two separate macro expansions, and they also
arrange for a reference to be introduced by @emph{both} of those
macros, hence the ambiguity. Arguably, in this case, the @racket[use]
macro is broken@narrow-audience-only{, as illustrated in a variant of the program without
@racket[define1] that produces @racket[2] with both expanders:
@;
@racketblock[    
         (begin
           (define x 1)
           (define-syntax foo #'x))
    
         (define-syntax (use stx)
           (syntax-case stx ()
             [(_ id)
              (with-syntax ([old-id (syntax-local-value #'id)])
                #'(begin
                    (define x 2)
                    old-id))]))
    
         (use foo)
]}@;
@;
The @racket[use] macro can be fixed @narrow-audience-only{for both expanders and both
contexts} by applying @racket[syntax-local-introduce] to the result of
@racket[(syntax-local-value #'id)], which cancels the macro-introduction scope on the identifier, since the identifier conceptually
exists outside of this macro's expansion. Such an application of
@racket[syntax-local-introduce] is typically needed and typically
present in existing Racket macros that bring stashed identifiers into
a new context.

The example above illustrates a typical level of macro complexity
needed to expose differences between the existing and set-of-scopes expanders.
@(if wide-audience?
     @list{Other existing Racket macros that may fail with
           the set-of-scopes expander include macros that expand to nested
           @racket[module] forms, macros that use explicit
           internal-definition contexts, and macros that introduce
           identifiers that originate in different modules
           but expect capture among the identifiers.}
     @list{Here are some specific other ways in with existing Racket code may fail with
           a set-of-scopes expander:
@;
@itemlist[

 @item{In the old macro system, a @racket[module] form for a
       submodule is expanded by first discarding @emph{all} lexical
       context. The set-of-scopes expander instead removes only the
       scope of the enclosing module. As a result, some macros that
       expand to submodules must more precisely manage their contexts.

       In the old expander, removing all lexical context ensures
       that no binding outside the module can be referenced directly,
       but to support re-expansion of the submodule, a property is
       added on a module to disable context stripping on future
       expansions and to skip over the module when adding context for
       an enclosing module. No special treatment is needed for
       re-expansion in the set-of-scopes expander, but the more
       limited context stripping means that certain (non-hygienic)
       submodule-producing macros no longer work.

       For example, the macro
       @;
       @racketblock[
         (define-syntax-rule (gen e)
           (module generated racket/base e))
       ]
       @;
       used to expand so that @racket[racket/base] is available for
       reference by @racket[e], but with the set-of-scopes expander,
       @racket[racket/base] retains its macro-introduced scope and
       does not bind the use-site replacement for @racket[e].

       At the same time, with the set-of-scopes expander, a macro from
       one module that expands to a submodule in another module runs
       the risk of provoking an out-of-context error, since the
       macro's module context is not removed form the generated
       submodule.}

 @item{Along the same lines as expanding to a submodule form, a
       pattern-matching macros that expands to a @racket[unit] form
       can behave differently if a mentioned signature or definition
       are not both introduced by the macro or from the macro use
       site. In other words, adjustments to the @racket[unit] macro to
       work with the set-of-scopes expander have regularized
       questionable scoping behavior of the @racket[unit] form itself,
        particularly as it interacts with other macros.}

 @item{Macros that use explicit internal-definition contexts are among
       the most likely to need adaptation. As described in
       @secref["fc-intdef"], such macros typically need to use
       @racket[syntax-local-identifier-as-binding] on identifiers that
       are inspected and manipulated as bindings. Macros that use
       internal-definition contexts to create unusual binding patterns
       (e.g., @racket[splicing-let-syntax]) may need more radical
       changes, since internal-definition contexts formerly made
       distinctions among specific identifiers---the ones explicitly
       registered to create renamings---while the distinction now is
       more uniform. Some such macros can switch to a simpler creation
       of a fresh scope (formerly ``mark''), while others require a
       completely different strategy.}

 @item{In the old macro system, if unbound identifiers with the
       same symbolic name are pulled from different modules into a new
       one, and if the introducing macros arrange for the identifiers
       to have no distinct macro-introduction marks (e.g., by using
       @racket[syntax-local-introduce]), then either of those
       identifiers can bind the other (since neither had a binding).
       With the set-of-scopes system, the two identifiers do no bind
       each other, since they have different scopes from their
       original modules.}

 @item{With the old macro expander, the @racket[#%top] form is
       implicitly wrapped around any use of an identifier outside a
       module when the identifier does not refer to a macro. The new
       expander uses @racket[#%top] only for identifiers that have no
       binding (which makes top-level expansion slightly more
       consistent with module expansion).}

]})

The documentation for Racket's old macro system avoids references
to the underlying mark-and-rename model. As a result, the documentation is often too
imprecise to expose differences created by a change to set-of-scope
binding. One goal of the new model is to allow the specification and
documentation of Racket's macro expander to be tightened; scope sets
are precise enough for specification, but abstract enough to allow
high-level reasoning.

@; ----------------------------------------
@section[#:tag "new-macros"]{Benefits for New Macros}

Certain existing macros in the Racket distribution had to be
reimplemented wholesale for the set-of-scopes expander. A notable
example is the @racket[package] macro, which simulates the module
system of Chez Scheme@~cite[Chez-modules]. The implementation of
@racket[package] for the old Racket macro expander uses
first-class definition contexts, rename transformers, and a facility
for attaching mark changes to a rename transformer (to make an
introduced name have marks similar to the reference). The
implementation with the set-of-scopes expander is considerably
simpler, using only scope-set operations and basic rename
transformers. Scope sets more directly implement the idea of packages
as nested lexical environments. The new implementation is 345 lines
versus 459 lines for the original implementation; both versions share
much of the same basic structure, and the extra 100 lines of the old
implementation represent especially complex pieces.

A similar example was discussed on the Racket mailing list. The
@racket[in-package] form is intended to simulate Common Lisp
namespaces, where definitions are implicitly prefixed with a package
name, a package can import unprefixed names from a different package
with @racket[use-package], and a package can @emph{stop} using
unprefixed names for the remainder its body with
@racket[unuse-package]. In this case, an implementation for the
old expander @html-only{(@linkfile{in-package.rkt})} uses marks, but the
implementation is constrained so that macros exported by one package
cannot expand to definitions in another package. Again, the
set-of-scopes expander @html-only{(@linkfile{in-package-scopes.rkt})} is
conceptually simpler, more directly reflects binding regions with
scopes, and allows definition-producing macros to be used across
package boundaries. The version for the old expander also works
with the set-of-scopes expander, although with the same limitations as
for the old expander; in fact, debugging output from the
set-of-scopes expander was instrumental in making that version of
@racket[in-package] work.

These two anecdotes involve similar macros that better fit the
set-of-scopes model for essentially the same reason, but out
experience with others macros---the @racket[unit] macro,
@racket[class] macro, and @racket[define-generics] macro---has been
similarly positive. In all cases, the set-of-scopes model has felt
easier to reason about, and the expander could more readily provide
tooling in support of the conceptual model.

@; ----------------------------------------
})

@; ----------------------------------------
@section[#:tag "debugging"]{Debugging Support}

Although the macro debugger@~cite[macro-debugger] has proven to be a
crucial tool for macro implementors, binding resolution in Racket's
old macro expander is completely opaque to macro implementers.
When something goes wrong, the expander or macro debugger can report
little more than ``unbound identifier'' or ``out of context'', because
the process of replaying renamings and the encodings used for the
renamings are difficult to unpack and relate to the programmer.

A set-of-scopes expander is more frequently in a position to report
``unbound identifier, but here are the identifier's scopes, and here
are some bindings that are connected to those scopes.'' In the case of
ambiguous bindings, the expander can report the referencing
identifier's scopes and the scopes of the competing bindings. These
details are reported in a way similar to stack traces: subject to
optimization and representation choices, and underspecified as a
result, but invaluable for debugging purposes.

@narrow-audience-only{

For example, when placed in a module named @racketidfont{m}, the
ambigious-reference error from @secref["compat"] produces an error
like this one:
@;
@pdf-smaller{
@verbatim[#:indent 2]|{
x: identifier's binding is ambiguous
  context...:
   #(1772 module) #(1773 module m 0) #(2344 macro)
   #(2358 macro)
  matching binding...:
   #<module-path-index:()>
   #(1772 module) #(1773 module m 0) #(2344 macro)
  matching binding...:
   #<module-path-index:()>
   #(1772 module) #(1773 module m 0) #(2358 macro)
  in: x
}|}
@;
Each scope is printed as a Racket vector, where the vector starts with
a number that is distinct for every scope. A symbol afterward provides
a hint at the scope's origin: @racket['module] for a @racket[module]
scope, @racket['macro] for a macro-introduction scope,
@racket['use-site] for a macro use-site scope, or @racket['local] for
a local binding form. In the case of a @racket['module] scope that
corresponds to the inside edge, the module's name and a phase (since
an inside-edge scope is generated for each phase) are shown.

The @tt{#<module-path-index:()>}s in the error correspond to the
binding, and they mean ``in this module.'' Overall, the message shows
that @racket[x] has scopes corresponding to two different macro
expansions, and it's bound by definitions that were produced by
the expansions separately.

}

@; ----------------------------------------
@section[#:tag "sweet.js"]{Scope Sets for JavaScript}

Although the set-of-scopes model of binding was developed with Racket
as a target, it is also intended as a more understandable model of
macros to facilitate the creation of macro systems for other
languages. In fact, the Racket implementation was not the first
implementation of the model to become available.
@;
Based on an early draft of this report, Tim Disney revised the
Sweet.js macro implementation for JavaScript@~cite[disney2014sweeten
sweet.js #:sort? #f]@note{See
@x-hyperlink["https://github.com/mozilla/sweet.js/pull/461"]{pull
request 461}.} to use scope sets even before the initial Racket
prototype was complete. Disney reports that the implementation of
hygiene for the macro expander is now ``mostly understandable'' and
faster.
