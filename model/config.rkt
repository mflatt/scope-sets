#lang racket/base

(provide s s/inline narrow-mode?)

(define s (string->number (or (getenv "SCOPE_SETS_TO_PDF")
                              "1.2")))
(define s/inline (string->number (or (getenv "SCOPE_SETS_TO_PDF_INLINE")
                                     (number->string s))))

(define narrow-mode? (and (getenv "SCOPE_SETS_TO_PDF") #t))
