#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

;; A simpler variant of `in-package` that works with
;; the set-of-scopes expander. It's simpler because a package
;; can correspond to a scope, which is exactly what we
;; want.

(begin-for-syntax
  (define introducers (make-hash))
  
  (struct package (introducer ; (stx -> stx)
                   used-introducers)) ; (hash-of symbol (stx -> stx))
  
  (define (get-package name)
    (hash-ref! introducers
               name
               (lambda ()
                 (package (make-syntax-introducer)
                          (make-hash)))))
  
  (define (get-package-introducers name)
    (define p (get-package name))
    (values (package-introducer p)
            (lambda (stx)
              (for/fold ([stx stx]) ([(k i) (in-hash
                                             (package-used-introducers p))])
                (i stx)))))

  (define (add-package-introducers! name other-names)
    (define p (get-package name))
    (for ([other-name (in-list other-names)])
      (define other-p (get-package other-name))
      (hash-set! (package-used-introducers p)
                 other-name
                 (package-introducer other-p))))

  (define (remove-package-introducers! name other-names)
    (define p (get-package name))
    (for ([other-name (in-list other-names)])
      (hash-remove! (package-used-introducers p)
                    other-name))))
    

(define-syntax (in-package stx)
  (syntax-parse stx
    [(_ name:id form)
     (define-values (introducer-for-package introducer-for-used)
       (get-package-introducers (syntax-e #'name)))
     (define e (local-expand (introducer-for-used
                              (introducer-for-package
                               #'form))
                             (syntax-local-context)
                             (list #'use-package
                                   #'unuse-package
                                   #'define-values
                                   #'define-syntaxes
                                   #'#%app)))
     (syntax-parse e 
       #:literals (use-package
                   unuse-package)
       #:literal-sets (kernel-literals)
       [(begin form ...)
        (quasisyntax/loc e
          (begin (in-package name form) ...))]
       [(define-values (id ...) rhs)
        (with-syntax ([(id ...)
                       ;; Strip away used packages' scopes
                       ;; from the defined name:
                       (introducer-for-used #'(id ...))])
          (quasisyntax/loc e
            (begin
              (define-values (id ...) rhs)
              (define-prefixed name (id ...)))))]
       [(define-syntaxes (id ...) rhs)
        (with-syntax ([(id ...) (introducer-for-used #'(id ...))])
          (quasisyntax/loc e
            (begin
              (define-syntaxes (id ...) rhs)
              (define-prefixed name (id ...)))))]
       [(#%require form)
        ;; Strip away used package's scopes from imports:
        (introducer-for-used e)]
       [(#%provide form) e]
       [(use-package other-name:id ...)
        (add-package-introducers! (syntax-e #'name)
                                  (map syntax-e
                                       (syntax->list #'(other-name ...))))
        (syntax/loc e (void))]
       [(unuse-package other-name:id ...)
        (remove-package-introducers! (syntax-e #'name)
                                     (map syntax-e
                                          (syntax->list #'(other-name ...))))
        (syntax/loc e (void))]
       [_ e])]
    [(_ name:id form ...)
     (syntax/loc stx
       (begin (in-package name form) ...))]))

(define-syntax (define-prefixed stx)
  (syntax-parse stx
    [(_ name (id))
     (with-syntax ([prefixed-id (format-id #'name "~a:~a" #'name #'id)])
       #'(define-syntax prefixed-id (make-rename-transformer #'id)))]
    [(_ name (id ...))
     #'(begin (define-prefixed-id name (id)) ...)]))

(define-for-syntax (only-in-package stx)
  (raise-syntax-error #f
                      "allowed only immediate within `in-package`"
                      stx))
(define-syntax use-package only-in-package)
(define-syntax unuse-package only-in-package)

;; ----------------------------------------

(module+ test
  (in-package FOO
              (define x 1)
              (define-syntax-rule (m id) (define id 7))
              x)
  
  FOO:x
  
  (in-package BAR
              (use-package FOO)
              (define y 8)
              FOO:x
              (list y x))
  
  (in-package BAR
              (list 'again x)
              (m w)
              w
              (unuse-package FOO)
              'x) ; fails if unquoted
  
  (in-package BAZ
              (use-package BAR)
              w)
  
  BAR:y)
