#lang scribble/base
@(require "common.rkt")

@title[#:tag "related"]{Other Related Work}

While our work shares certain goals with techniques for representing
resolved bindings, such as de Bruijn indices, higher-order abstract
syntax@~cite[hoas], and nominal sets@~cite[nominal-sets], those
techniques seem to be missing a dimension that is needed to
incrementally resolve bindings as introduced and manipulated by
macros. @citet[essence-of-hygiene] demonstrates how pairs of
conventional identifiers provide enough of an extra dimension for
hygienic macro expansion, but supporting @racket[datum->syntax] would
require the further extension of reifying operations on identifiers
(in the sense of marks and renamings). Scope sets provides the
additional needed dimension in a simpler way.

@deftech{Scope graphs}@~cite[scope-graphs] abstract over program syntax
to represent binding relationships---including support for constructs, such as modules and
class bodies, that create static scopes different than nested lexical
scopes. Binding resolution with macro expansion seems more dynamic, in
that a program and its binding structure evolve during expansion;
up-front scope graphs are not clearly applicable. Scope sets,
meanwhile, do not explicitly represent import relationships, relying
on macros that implement modular constructs to create scopes and
bindings that reflect the import structure. Further work on scope
graphs and scope sets seems needed to reveal the connections.

@citet[Romeo] build on the direction of @citet[herman-dissertation]
with Romeo, which supports program manipulations that respect scope by
requiring that every transformer's type exposes its effect on binding.
The resulting language is more general than Herman's macros, but
transformers are more constrained than hygienic macros in Scheme and
Racket.
