
default:
	$(MAKE) model.zip
	$(MAKE) html

PDF_CONFIG = SCOPE_SETS_TO_PDF=0.7 SCOPE_SETS_TO_PDF_INLINE=0.8
pdf:
	raco make scope-sets.scrbl
	env $(PDF_CONFIG) raco scribble +m --pdf ++style style.tex scope-sets.scrbl

latex:
	raco make scope-sets.scrbl
	env $(PDF_CONFIG) raco scribble +m --latex ++style style.tex --dest /tmp/scope-sets-src scope-sets.scrbl

html:
	raco make scope-sets.scrbl
	raco scribble +m --redirect-main http://doc.racket-lang.org/ --htmls --dest /tmp scope-sets.scrbl

MODEL_FILES = model/core-model.rkt model/phases-model.rkt \
	      model/local-model.rkt model/defs-model.rkt \
              model/define-example.rkt model/doc.rkt \
              model/rewrites.rkt model/config.rkt model/viewer.rkt \
              model/core-model.pdf model/phases-model.pdf \
	      model/local-model.pdf model/defs-model.pdf \
              model/README.txt

model.zip: $(MODEL_FILES)
	zip model.zip $(MODEL_FILES)

RENDER_MODEL = env SCOPE_SETS_TO_PDF=0.8 raco scribble --pdf --dest model/
render-models:
	$(RENDER_MODEL) model/core-model.rkt
	$(RENDER_MODEL) model/phases-model.rkt
	$(RENDER_MODEL) model/local-model.rkt
	$(RENDER_MODEL) model/defs-model.rkt
