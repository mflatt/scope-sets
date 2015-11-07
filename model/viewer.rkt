#lang racket/base  
(require racket/gui/base
         racket/class
         pict)

(provide view)

(define (view . picts)
  (define f (new frame%
                 [label "Model"]))
  
  (define (pict->screen-bitmap p)
    (define (c n) (inexact->exact (ceiling n)))
    (define bm (make-screen-bitmap (c (pict-width p)) (c (pict-height p))))
    (define dc (send bm make-dc))
    (send dc clear)
    (draw-pict p dc 0 0)
    bm)
  
  (new message%
       [parent f]
       [label (pict->screen-bitmap (apply vc-append picts))])

  (send f show #t))
