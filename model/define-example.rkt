#lang scheme
(require redex/reduction-semantics)

(provide define-example-definer)

(define-syntax-rule (define-example-definer define-example 
                      mini wrap parser empty-ctx)
  (begin

    (...
     (define-metafunction mini
       as-syntax : any -> val
       [(as-syntax nam) (Stx (Sym nam) empty-ctx)]
       [(as-syntax number) (Stx number empty-ctx)]
       [(as-syntax prim) (Stx prim empty-ctx)]
       [(as-syntax tprim) (Stx tprim empty-ctx)]
       [(as-syntax (any ...)) (Stx (List (as-syntax any) ...) empty-ctx)]))

    (define-syntax-rule (define-example id form expected)
      (begin
        (define id
          (lambda (mode)
            (let ([t (wrap (term (as-syntax form)))])
              (case mode
                [(expand) t]
                [(parse) (term (parser ,t))]
                [else (error 'example "unknown mode: ~e" mode)]))))
        (test-equal (id 'parse) (term expected))))))
