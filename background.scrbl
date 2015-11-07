#lang scribble/base
@(require racket/list
          "common.rkt")

@title[#:tag "background"]{Background: Scope and Macros}

An essential consequence of hygienic macro expansion is to enable
macro definitions via
@deftech{patterns} and @deftech{templates}---also known as
@deftech{macros by example}@~cite[macro-by-example macros-that-work
#:sort? #f]. Although pattern-based macros are limited in various
ways, a treatment of binding that can accommodate patterns and
templates is key to the overall expressiveness of a hygienic macro
system, even for macros that are implemented with more general
constructs.

As an example of a pattern-based macro, suppose that a Racket library
implements a Java-like object system and provides a @racket[send] form,
where
@;
@racketblock[
  (send a-point rotate 90)
]
@;
evaluates @racket[a-point] to an object, locates a function mapped to
the symbol @racket['rotate] within the object, and calls the function
as a method by providing the object itself followed by the argument
@racket[90]. Assuming a @racket[lookup-method] function that locates a method
within an object, the @racket[send] form can be implemented by a
pattern-based macro as follows:
@;
@racketblock[
 (define-syntax-rule (send _obj-expr _method-name _arg)
   (let ([obj _obj-expr])
     ((lookup-method obj '_method-name) obj _arg)))
]
@;
The @racket[define-syntax-rule] form declares a @defterm{pattern} that
is keyed on an initial identifier; in this case, the
pattern is keyed on @racket[send]. The remaining identifiers in the
parenthesized @racket[send] pattern are @defterm{pattern variables}. The second
part of the definition specifies a @defterm{template} that replaces a
match of the pattern, where each use of a pattern variable in the
template is replaced with the corresponding part of the match.

With this definition, the example use of @racket[send] above matches
the pattern with @racket[a-point] as @racket[_obj-expr],
@racket[rotate] as @racket[_method-name], and @racket[90] as
@racket[_arg], so the @racket[send] use expands to
@;
@racketblock[
 (let ([obj a-point])
   ((lookup-method obj 'rotate) obj 90))
]
@;
Hygienic macro expansion ensures that the
identifier @racket[obj] is not accidentally referenced
in an expression that replaces @racket[_arg] in a use of
@racket[send]@~cite[hygiene]. For example, the body of
@;
@racketblock[
  (lambda (obj)
    (send a-point same? obj))
]
@;
must call the @racket[same?] method of @racket[a-point] with the function argument @racket[obj],
and not with @racket[a-point] itself as bound to @racket[obj] in the
macro template for @racket[send]. Along similar lines, a local binding of
@racket[lookup-method] at a use site of @racket[send] must
not affect the meaning of @racket[lookup-method] in @racket[send]'s
template. That is,
@;
@racketblock[
  (let ([lookup-method #f])
    (send a-point rotate 90))
]
@;
should still call the @racket[rotate] method of @racket[a-point].

Macros can be bound locally, and macros can even expand to definitions
of macros. For example, suppose that the library also provides
a @racket[with-method] form that performs a method lookup just once
for multiple sends:
@;
@racketblock[
  (with-method ([rotate-a-point (a-point rotate)]) (code:comment @#,elem[#:style 'roman]{find @racket[rotate]@(if narrow-column? "" " once")})
    (for ([i 1000000])
      (rotate-a-point 90))) (code:comment @#,elem[#:style 'roman]{send @racket[rotate] to @racket[point] many times})
]
@;
The implementation of @racket[with-method] can make
@racket[rotate-a-point] a local macro binding, where a use of
@racket[rotate-a-point] expands to a function call with
@racket[a-point] added as the first argument to the function. That is,
the full expansion is
@;
@racketblock[
  (let ([obj a-point])
    (let ([rotate-a-point-method (lookup-method obj 'rotate)])
      (for ([i 1000000])
        (rotate-a-point-method obj 90))))
]
@;
but the intermediate expansion is
@;
@racketblock[
  (let ([obj a-point])
    (let ([rotate-a-point-method (lookup-method obj 'rotate)])
      (let-syntax ([rotate-a-point (syntax-rules ()
                                    [(rotate-a-point _arg)
                                     (rotate-a-point-method obj _arg)])])
        (for ([i 1000000])
          (rotate-a-point 90)))))
]
@;
where @racket[let-syntax] locally binds the macro
@racket[rotate-a-point]. The macro is implemented by a
@racket[syntax-rules] form that produces an anonymous pattern-based
macro (in the same way that @racket[lambda] produces an anonymous
function).

In other words, @racket[with-method] is a binding form, it is a
macro-generating macro, it relies on local-macro binding, and the
macro that it generates refers to a private binding @racket[obj] that
is also macro-introduced.
@;
Nevertheless, @racket[with-method] is straightforwardly implemented as
a pattern-based macro:
@;
@(if (not narrow-column?)

;; Wider and more consistent variant for HTML:
@racketblock[
(define-syntax-rule (with-method ([_local-id (_obj-expr _method-name)])
                       _body)
  (let ([obj _obj-expr])
    (let ([method (lookup-method obj 'method-name)])
      (let-syntax ([_local-id (syntax-rules ()
                                [(_local-id _arg)
                                 (method obj _arg)])])
         _body))))
]

;; Narrower for PDF:
@racketblock[
(define-syntax-rule
  (with-method ([_local-id (_obj-expr _method-name)])
    _body)
  (let ([obj _obj-expr])
    (let ([method (lookup-method obj 'method-name)])
      (let-syntax ([_local-id (syntax-rules ()
                                [(_local-id _arg)
                                 (method obj _arg)])])
         _body))))
])
@;
Note that the @racket[obj] binding cannot be given a permanently
distinct name within @racket[with-method]. A distinct name must be
generated for each use of @racket[with-method], so that nested uses
create local macros that reference the correct @racket[obj].

In general, the necessary bindings or even the binding structure of a
macro's expansion cannot be predicted in advance of expanding the
macro. For example, the @racket[let] identifier that starts the
@racket[with-method] template could be replaced with a macro argument,
so that either @racket[let] or, say, a lazy variant of @racket[let]
could be supplied to the macro. The expander must accommodate such
macros by delaying binding decisions as long as possible. Meanwhile,
the expander must accumulate information about the origin of
identifiers to enable correct binding decisions.

Even with additional complexities---where the macro-generated macro is
itself a binding form, where uses can be nested so the different uses
of the generated macro must have distinct bindings, and so
on---pattern-based macros support implementations that are essentially
specifications@~cite[macro-by-example]. A naive approach to macros and
binding fails to accommodate the
specifications@~cite[(in-bib essence-of-hygiene ", sections 4.2-4.5")], while existing
formalizations of suitable binding rules detour into concepts of marks
and renamings that are distant from the programmer's sense of the
specification.

The details of a formalization matter more when moving beyond
pattern-matching macros to @deftech{procedural macros}, where the
expansion of a macro can be implemented by an arbitrary compile-time
function. The @racket[syntax-case] and @racket[syntax] forms provide
the pattern-matching and template-construction facilities,
respectively, of @racket[syntax-rules], but they work as expressions
within a compile-time function@~cite[syntax-case]. This combination
allows a smooth transition from pattern-based macros to procedural
macros for cases where more flexibility is needed. In fact, @racket[syntax-rules]
is itself simply a macro that expands to a procedure:
@;
@(define syntax-abbrev
   @elem[#:style 'roman]{@racket[#'__] @|'nbsp|is short for@|'nbsp| @racket[(@#,racket[syntax] __)]})@;
@;
@racketblock[
 (define-syntax-rule (syntax-rules _literals
                       [_pattern _template] ...)
   (lambda (stx)
     (syntax-case stx _literals
       [_pattern #'_template] (code:comment @#,syntax-abbrev)
       ...)))
]
@;
Besides allowing arbitrary computation mixed with pattern matching and
template construction, the @racket[syntax-case] system provides
operations for manipulating program representations as @deftech{syntax
objects}. Those operations include ``bending'' hygiene by attaching
the binding context of one syntax object to another. For example, a
macro might accept an identifier @racket[point] and synthesize the
identifier @racket[make-point], giving the new identifier the same
context as @racket[point] so that @racket[make-point] behaves as if it
appeared in the same source location with respect to binding.

Racket provides an especially rich set of operations on syntax objects
to enable macros that compose and cooperate@~cite[expmodel]. Racket's
macro system also relies on a module layer that prevents interference
between run-time and compile-time phases of a program, since interference would
make macros compose less reliably@~cite[macromod]. Finally, modules
can be nested and macro-generated, which enables macros and modules to
implement facets of a program that have different instantiation
times---such as the program's run-time code, its tests, and its
configuration metadata@~cite[submodules]. The module-level facets of
Racket's macro system are, at best, awkwardly accommodated by existing
models of macro binding; those models are designed for
expression-level binding, where α-renaming is straightforward, while
modules address a more global space of mutually recursive macro and
variable definitions. A goal of our new binding model is to more
simply and directly account for such definition contexts.
