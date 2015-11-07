#lang at-exp racket/base
(require scribble/base
         scribble/decode
         racket/list)

(provide make-model-doc
         skip)

(define (make-model-doc layer . l)
  (decode (cons @title{@layer Model}
                (add-between l skip))))

(define skip '("\n\n" nbsp "\n\n"))
