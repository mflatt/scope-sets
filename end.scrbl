#lang scribble/base
@(require "common.rkt")

@title[#:tag "end"]{Conclusion}

Hygienic macro expansion is a successful, decades-old technology in
Racket and the broader Scheme community. Hygienic macros have also
found a place in some other languages, but the difficulties of
specifying hygiene, understanding macro scope, and implementing a
macro expander have surely been an obstacle to the broader adoption
and use of hygienic macros. Those obstacles, in turn, suggest that our
models of macro expansion have not yet hit the mark. Scope sets are an
attempt to move the search for a model of macro expansion to a
substantially different space, and initial results with Racket and
JavaScript show that this new space is promising.
