#lang at-exp racket/base
(require scribble/manual
         scriblib/footnote
         scribble/core
         scribble/racket
         scribble/html-properties
         racket/list
         "bib.rkt"
         (for-syntax racket/base)
         (for-label racket/base
                    racket/unit
                    racket/class
                    racket/generic
                    racket/stxparam))

(provide (all-defined-out)
         (all-from-out scribble/manual
                       scriblib/footnote
                       "bib.rkt")
         (for-label (all-from-out racket/base
                                  racket/unit
                                  racket/class
                                  racket/generic
                                  racket/stxparam)))

(define to-pdf? (getenv "SCOPE_SETS_TO_PDF"))
(define narrow-column? to-pdf?)
(define wide-audience? to-pdf?)
(define double-blind? (and to-pdf? #f))

(define (set . l) @elem["{" (add-between l ", ") "}"])
(define-syntax-rule (id x e ...)
  (elem (racket x) (elem #:style 'superscript (set e ...))))

(define (tuple . l) @elem["⟨" (add-between l ", ") "⟩"])

(define (scope-id pos s) (if pos
                             (let ([alpha (integer->char (+ (char->integer #\a) (sub1 pos)))])
                               @math[(format "~a_~a" alpha s)])
                             @math[s]))

(define s_1df @scope-id[1]{def})
(define s_1lt @scope-id[1]{let})
(define s_1ls @scope-id[1]{ls})
(define s_1lm @scope-id[1]{lam})
(define s_1def @scope-id[1]{def})
(define s_1in @scope-id[1]{in})
(define s_1out @scope-id[1]{out})
(define s_2lm @scope-id[2]{lam})
(define s_2ls @scope-id[2]{ls})
(define s_2lt @scope-id[2]{let})
(define s_2i @scope-id[2]{intro})
(define s_2i1 @scope-id[2]{intro1})
(define s_2u1 @scope-id[2]{use1})
(define s_2def @scope-id[2]{def})
(define s_2in @scope-id[2]{in})
(define s_2out @scope-id[2]{out})
(define s_3lm @scope-id[3]{lam})
(define s_3i2 @scope-id[3]{intro2})
(define s_4i @scope-id[4]{intro})
(define s_4lt @scope-id[4]{let})
(define s_5u @scope-id[5]{use})
(define s_i @scope-id[#f]{s})

(define (aside/proc get-l)
  (if wide-audience?
      null
      (nested #:style "smaller" (nested #:style 'inset (get-l)))))
;; Put `e ...` in a thunk so it's evaluated only if selected,
;; since things like `cite` have a side-effect.
(define-syntax-rule (aside e ...)
  (aside/proc (lambda () (list e ...))))

(define (linkfile path)
  (elem #:style (style #f (list (link-resource path))) path))

(define (html-only/proc get-l) (if to-pdf? null (get-l)))
(define (pdf-only/proc get-l) (if to-pdf? (get-l) null))
(define (narrow-audience-only/proc get-l) (if wide-audience? null (get-l)))

(define-syntax-rule (html-only e ...)
  (html-only/proc (lambda () (list e ...))))
(define-syntax-rule (pdf-only e ...)
  (pdf-only/proc (lambda () (list e ...))))
(define-syntax-rule (narrow-audience-only e ...)
  (narrow-audience-only/proc (lambda () (list e ...))))

(define (aux . l) @elem[#:style (style #f '(aux)) l])

(define-for-syntax (width-sel wide narrow)
  (make-element-id-transformer
   (lambda (stx)
     #`@racketidfont[(if narrow-column? #,narrow #,wide)])))

(define-syntax a-point (width-sel "a-point" "a-pt"))
(define-syntax rotate-a-point (width-sel "rotate-a-point" "rot-a-pt"))
(define-syntax rotate-a-point-method (width-sel "rotate-a-point-method" "rot-a-pt-m"))

(define (pdf-smaller . l)
  (if to-pdf?
      @nested[#:style "smaller" l]
      l))
(define (pdf-codeblock . l)
  (if to-pdf?
      @nested[#:style 'code-inset l]
      l))

(define (x-hyperlink given-url . l)
  (if to-pdf?
      @list{@l at @url[given-url]}
      (hyperlink given-url l)))

(define (html-note . l)
  (if to-pdf?
      null
      (note l)))
