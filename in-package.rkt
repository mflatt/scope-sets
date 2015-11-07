#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(begin-for-syntax
  ;; Infor on a package:
  (struct package (introducer ; (stx -> stx), for package-specific mark
                   [defns #:mutable] ; (listof id), without marks
                   uses)) ; (hash/c sym id), table of used packages

  ;; Table of implicitly declared packages:
  (define packages (make-hash))
  
  ;; Find a package by name:
  (define (lookup-package pkg-name)
    (hash-ref! packages (syntax-e pkg-name)
               (lambda ()
                 (package (make-syntax-introducer)
                          null
                          (make-hash)))))
  
  ;; Record definitions in a package:
  (define ((package-add-defined! p) id)
    ;; Remove local mark and package's mark; refers to a definition
    ;; that has the package's mark:
    (set-package-defns! p (cons ((package-introducer p)
                                 (syntax-local-introduce id))
                                (package-defns p))))
  
  ;; Create a list of bindings suitable for use with
  ;; `letrec-syntaxes`:
  (define ((defined-bindings wrt-p) pkg-name)
    (define p (lookup-package pkg-name))
    (with-syntax ([(ext-id ...)
                   ;; Bind names without source package's mark,
                   ;; but with enclosing package's mark:
                   (map (lambda (id)
                          (syntax-local-introduce
                           ((package-introducer wrt-p)
                            id)))
                        (package-defns p))]
                  [(def-id ...)
                   ;; Reference names with source package's mark:
                   (map (package-introducer p)
                        (package-defns p))])
      (syntax->list
       #`([(ext-id ...)
           (values
            (make-rename-transformer #'def-id) ...)]))))
  
  ;; Add a wrapper around an expression to import bindings:
  (define (add-used p stx)
    #`(letrec-syntaxes #,(apply
                          append
                          (map (defined-bindings p)
                               (hash-values (package-uses p))))
                       #,stx))
  
  ;; Add a package to the `use-package` list:
  (define (package-add-used! p other-names)
    (for ([other-name (in-list other-names)])
      (hash-set! (package-uses p)
                 (syntax-e other-name)
                 other-name)))

  ;; Remove a package from the `use-package` list:
  (define (package-remove-used! p other-names)
    (for ([other-name (in-list other-names)])
      (hash-remove! (package-uses p)
                    (syntax-e other-name)))))

(define-syntax (in-package stx)
  (syntax-parse stx
    [(_ name:id form)
     (define p (lookup-package #'name))
     (define e (local-expand (syntax-local-introduce
                              ((package-introducer p)
                               (syntax-local-introduce
                                #'form)))
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
        (for-each (package-add-defined! p) (syntax->list #'(id ...)))
        (quasisyntax/loc e
          (begin
            (define-values (id ...) #,(add-used p #'rhs))
            (define-prefixed name (id ...))))]
       [(define-syntaxes (id ...) rhs)
        (for-each (package-add-defined! p) (syntax->list #'(id ...)))
        (quasisyntax/loc e
          (begin
            (define-syntaxes (id ...) #,(add-used p #'rhs))
            (define-prefixed name (id ...))))]
       [(#%require form) e]
       [(#%provide form) e]
       [(use-package other-name:id ...)
        (package-add-used! p (syntax->list #'(other-name ...)))
        (syntax/loc e (void))]
       [(unuse-package other-name:id ...)
        (package-remove-used! p (syntax->list #'(other-name ...)))
        (syntax/loc e (void))]
       [_ (add-used p e)])]
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
              (define-syntax-rule (m) x)
              x)
  
  FOO:x
  
  (in-package BAR
              (use-package FOO)
              (define y 8)
              FOO:x
              (list y x))
  
  (in-package BAR
              (list 'again x)
              (m)
              (unuse-package FOO)
              'x) ; fails if unquoted
  
  BAR:y)
