These models of the macro expander are implemented in PLT Redex and
run in Racket v6.2.900.x and later.

Each model corresponds to a section in the paper:

 * Section 6.1: "core-model.rkt" (typeset as "core-model.pdf", etc.)

 * Section 6.2: "phases-model.rkt"

 * Section 6.3: "local-model.rkt"

 * Section 6.4: "defs-model.rkt"

Running any <model> file runs its tests. See the "Examples" section
in each model. You can try new examples with

 (define-example box-example
   <input program>
   <expected expansion>)

Each model's examples run in later modules, so each model provides
its examples (in a macro) to the later modules.

If you intend to run a model module times, use `raco make <model>` to
make the model load much faster.


Running any model with `raco scribble <model>` produces a typeset form
of the model in HTML, although the typeset form is mainly intended for
PDF rendering. Render the model to PDF with `raco scribble --pdf
<model>`, but generating PDF requires LaTeX.
