#lang scribble/sigplan
@(require racket/list
          "common.rkt")

@title[#:style (if to-pdf?
                   #f
                   manual-doc-style)
       #:version ""]{Binding as Sets of Scopes@html-only{@aux{
       @linebreak[]
       @smaller{Notes on a new model of macro expansion for Racket}}}}
@authorinfo["Matthew Flatt" "University of Utah, USA" "mflatt@cs.utah.edu"]

@pdf-only{@abstract{

Our new macro expander for Racket builds on a novel approach to
hygiene. Instead of basing macro expansion on variable renamings that
are mediated by expansion history, our new expander tracks binding
through a @defterm{set of scopes} that an identifier acquires from
both binding forms and macro expansions. The resulting model of macro
expansion is simpler and more uniform than one based on renaming, and
it is sufficiently compatible with Racket's old expander to be
practical.

}

@category["D.3.3" "Programming Languages" "Language Constructs and Features"]
@keywords["Macros, hygiene, binding, scope"]}

@pdf-only{@section[#:tag "intro"]{Introduction: Lexical vs. Macro Scope}}

@html-only{@margin-note{This is an extended version of a
           @hyperlink["http://www.cs.utah.edu/plt/publications/popl16-f.pdf"]{POPL'16
           paper}. Executable models from the paper are available as
           @linkfile["model.zip"]. The
           @hyperlink["http://www.cs.utah.edu/plt/popl16/"]{POPL'16
           artifact} provides additional supporting material,
           including archived implementations of the old and new
           expanders.}}

Racket supports a family of languages---including the main Racket
language, Typed Racket, teaching languages, a documentation language,
an Algol 60 implementation, and more---where the languages interoperate
in natural and useful ways. Furthermore, programmers
can create new languages and language extensions with the same level
of support and interoperability as the predefined languages. Racket's
support for multiple languages and extensions is enabled by its
macro system, which provides a representation of syntax
fragments and constructs for manipulating and composing
those fragments.

Racket's representation of syntax builds on a long line of
work on macros in Lisp and Scheme@~cite[hygiene macro-by-example
macros-that-work syntax-case #:sort? #f]. A result of that line of
work is often referenced by the shorthand of @defterm{hygienic macro
expansion}, meaning that macros can both introduce bindings and refer
to binding from the macro's definition context, and macro expansion will not
trigger accidental name capture in the way that naive textual
expansion would. Although Racket does not restrict language extension
to forms that can be expressed as hygienic macros, a representation of
syntax fragments that supports hygienic expansion is an essential
ingredient to more general, composable language extensions.

Roughly, hygienic macro expansion is desirable for the same reason as
lexical scope: both enable local reasoning about binding so that
program fragments compose reliably. The analogy suggests specifying
hygienic macro expansion as a kind of translation into lexical-scope
machinery. In that view, variables must be @emph{renamed} to match the
mechanisms of lexical scope as macro expansion proceeds. A
specification of hygiene in terms of renaming accommodates simple
binding forms well, but it becomes unwieldy for recursive definition
contexts@~cite[(in-bib expmodel ", section 3.8")], especially for
contexts that allow a mixture of macro and non-macro definitions. The
renaming approach is also difficult to implement compactly and
efficiently in a macro system that supports ``hygiene bending''
operations, such as @racket[datum->syntax], because a history of
renamings must be recorded for replay on an arbitrary symbol.

In a new macro expander for Racket, we discard the renaming approach
and start with a generalized idea of macro scope, where lexical scope
is just a special case of macro scope when applied to a language
without macros. Roughly, every binding form and macro expansion
creates a @defterm{scope}, and each fragment of syntax acquires a
@defterm{set of scopes} that determines binding of identifiers within
the fragment. In a language without macros, each scope set is
identifiable by a single innermost scope. In a language with macros,
identifiers acquire scope sets that overlap in more general ways.

Our experience is that this set-of-scopes model is simpler
to use than the old macro expander, especially for macros that work with recursive-definition
contexts or create unusual binding patterns. Along similar lines, the
expander's implementation is simpler than the old one based on
renaming, and the implementation avoids bugs that were
difficult to repair in the old expander.
Finally, the new macro expander is able to provide more helpful
debugging information when binding resolution fails.
All of these benefits reflect the way that scope sets
are precise enough for specification but abstract enough to allow
high-level reasoning.

This change to the expander's underlying model of binding can affect
the meaning of existing Racket macros. A small amount of
incompatibility seems acceptable and even desirable if it enables
easier reasoning overall. Drastic incompatibilities would be suspect,
however, both because the old expander has proven effective and because
large changes to code base would be impractical. Consistent with those
aims, purely pattern-based macros work with the new expander the same
as with the old one, except for unusual macro patterns within a
recursive definition context.
More generally, our experiments indicate that the majority of existing
Racket macros work unmodified, and other macros can be adapted with
reasonable effort.

@local-table-of-contents[]

@include-section["background.scrbl"]

@include-section["pattern-macros.scrbl"]

@include-section["general-macros.scrbl"]

@include-section["implementation.scrbl"]

@include-section["model.scrbl"]

@include-section["hygiene.scrbl"]

@include-section["related.scrbl"]

@include-section["end.scrbl"]

@; ----------------------------------------

@(if double-blind?
     null
     @list{
     
@section[#:style 'unnumbered]{Acknowledgments}

Thanks to Matthias Felleisen, Robby Findler, Sam Tobin-Hochstadt, Jay
 McCarthy, Ryan Culpepper, Brian Mastenbrook, Michael Adams, Michael Ballantyne, Tim
 Disney, Simon Peyton Jones, Shriram Krishnamurthi, and Eelco Visser
 for discussions about set-of-scopes binding.
 Joshua Grams provided helpful editing suggestions. Anthony Carrico
 and Chris Brooks helpfully pointed out some typos. Thanks also to anonymous
 POPL reviewers for helping to improve the presentation.
  This work was supported by grants from the NSF.
})

@generate-bibliography[#:sec-title "References"]
