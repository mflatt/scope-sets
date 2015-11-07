#lang racket
(require redex/reduction-semantics
         "define-example.rkt"
         slideshow/pict
         (rename-in (except-in "core-model.rkt"
                               flip add
                               bind
                               parse
                               resolve)
                    [subst core:subst]
                    [δ core:δ]
                    [strip core:strip]
                    [lookup core:lookup]
                    [extend core:extend]
                    [alloc-name core:alloc-name]
                    [alloc-scope core:alloc-scope]))

(provide Lph
         subst
         δ
         eval
         flip add strip prune update-ctx
         bind at-phase
         resolve
         parse
         lookup extend
         alloc-scope alloc-name
         expand expand*
         define-phases-examples
         (all-from-out "core-model.rkt"))

(define-extended-language Lph L
  [ph integer]
  [ctx desc-ctx]
  [desc-ctx (Map [ph scps] ...)])

;; The redefinition of `ctx` changes the definition of `val`, so most
;; metafunctions need to be reinterpreted with respect to `Lph`.

;; ----------------------------------------
;; Non-capturing substitution for AST:

(define-metafunction/extension core:subst Lph
  subst : ast var ast -> ast)

;; ----------------------------------------
;; Implementation of primitives:

(define-metafunction/extension core:δ Lph
  δ : prim (val ...) -> val)

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction Lph
  eval : ast -> val
  [(eval (App ast_fun ast_arg))
   (eval (subst ast_body var (eval ast_arg)))
   (where (Fun var ast_body) (eval ast_fun))]
  [(eval (App prim ast_arg ...))
   (δ prim ((eval ast_arg) ...))]
  [(eval val) val])

;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction Lph
  add : ph stx scp -> stx
  ;; Similar to one-phase `add`, but must update context
  ;; at a given phase
  [(add ph (Stx atom ctx) scp) 
   (Stx atom (update-ctx ctx ph (union (Set scp) (at-phase ctx ph))))]
  [(add ph (Stx (List stx ...) ctx) scp)
   (Stx (List (add ph stx scp) ...)  (update-ctx ctx ph (union (Set scp) (at-phase ctx ph))))])

(define-metafunction Lph
  flip : ph stx scp -> stx
  ;; Similar to one-phase `flip`, but must update context
  ;; at a given phase
  [(flip ph (Stx atom ctx) scp) 
   (Stx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
  [(flip ph (Stx (List stx ...) ctx) scp)
   (Stx (List (flip ph stx scp) ...)  (update-ctx ctx ph (addremove scp (at-phase ctx ph))))])

(define-metafunction/extension core:strip Lph
  strip : stx -> val)

(define-metafunction Lph
  prune : ph stx scps -> stx
  ;; Recursively removes a set of scopes from a syntax object
  ;; at a given phase
  [(prune ph (Stx atom ctx) scps_p)
   (Stx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
  [(prune ph (Stx (List stx ...) ctx) scps_p)
   (Stx (List stx_pruned ...) (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))
   (where (Seq stx_pruned ...) (Seq (prune ph stx scps_p) ...))])

(define-metafunction Lph
  update-ctx : ctx ph scps -> ctx
  ;; Updates the mapping of a `ctx` at a particular phase
  [(update-ctx (Map any_1 ... [ph scps_2] any_2 ...) ph scps_1)
   (Map any_1 ... [ph scps_1] any_2 ...)]
  [(update-ctx (Map any_1 ...) ph scps_1)
   (Map any_1 ... [ph scps_1])])

(define-metafunction Lph
  bind : ph Σ id nam -> Σ
  ;; Like one-phase `bind`, but extracts scopes at a given phase of
  ;; the identifier
  [(bind ph
         (Sto number
              (binds_1 ... [nam_1 (StoBind scps_2 nam_2) ...] binds_2 ...)
              boxes
              def-envs)
         (Stx (Sym nam_1) (Map _ ... [ph scps_1] _ ...))
         nam_3)
   (Sto number
        (binds_1 ... [nam_1 (StoBind scps_1 nam_3) (StoBind scps_2 nam_2) ...] binds_2 ...)
        boxes
        def-envs)]
  [(bind ph
         (Sto number (binds ...) boxes def-envs)
         (Stx (Sym nam_1) (Map _ ... [ph scps_1] _ ...))
         nam_3)
   (Sto number
        ([nam_1 (StoBind scps_1 nam_3)] binds ...)
        boxes
        def-envs)])

(define-metafunction Lph
  at-phase : ctx ph -> scps
  [(at-phase (Map any_1 ... [ph scps] any_2 ...) ph)
   scps]
  [(at-phase ctx ph)
   (Set)])

(define-metafunction Lph
  resolve : ph id Σ -> nam
  ;; Like the one-phase `resolve`, but at a particular phase
  [(resolve ph (Stx (Sym nam) ctx) Σ)
   nam_biggest
   (where (Set (StoBind scps_bind nam_bind) ...) (store-lookup Σ nam))
   (where scps_biggest (biggest-subset (at-phase ctx ph) (Set scps_bind ...)))
   (where nam_biggest (binding-lookup (Set (StoBind scps_bind nam_bind) ...) scps_biggest))]
  [(resolve ph (Stx (Sym nam) ctx) Σ)
   nam])

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

(define-metafunction Lph
  parse : ph stx Σ -> ast
  ;; This parse is the same as the single-phase one, but with `ph`
  ;; threaded through to `resolve`
  [(parse ph (Stx (List id_lambda id_arg stx_body) ctx) Σ)
   (Fun (Var (resolve ph id_arg Σ)) (parse ph stx_body Σ))
   (where lambda (resolve ph id_lambda Σ))]
  [(parse ph (Stx (List id_quote stx) ctx) Σ)
   (strip stx)
   (where quote (resolve ph id_quote Σ))]
  [(parse ph (Stx (List id_syntax stx) ctx) Σ)
   stx
   (where syntax (resolve ph id_syntax Σ))]
  [(parse ph (Stx (List stx_rator stx_rand ...) ctx) Σ)
   (App (parse ph stx_rator Σ) (parse ph stx_rand Σ) ...)]
  [(parse ph id Σ)
   (Var (resolve ph id Σ))])

;; ----------------------------------------
;; Expand-time environment operations:

(define-metafunction/extension core:lookup Lph
  lookup : env nam -> all-transform)

(define-metafunction/extension core:extend Lph
  extend : env nam all-transform -> env)

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

(define-metafunction/extension core:alloc-name Lph
  alloc-name : id Σ -> (values nam Σ))

(define-metafunction/extension core:alloc-scope Lph
  alloc-scope : id Σ -> (values nam Σ))

;; ----------------------------------------
;; The expander:

(define-metafunction Lph
  expand : ph stx env scps Σ -> (values stx Σ)

  ;; lambda
  [(expand ph (Stx (List id_lam id_arg stx_body) ctx) env scps_p Σ)
   (values (Stx (List id_lam id_new stx_body2) ctx) Σ_4)
   (where lambda (resolve ph id_lam Σ))
   (where (values nam_new Σ_1) (alloc-name id_arg Σ))
   (where (values scp_new Σ_2) (alloc-scope id_arg Σ_1))
   (where id_new (add ph id_arg scp_new))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where env_new (extend env nam_new (TVar id_new)))
   (where (values stx_body2 Σ_4)
          (expand ph (add ph stx_body scp_new) env_new (union (Set scp_new) scps_p) Σ_3))]

  ;; quote
  [(expand ph (Stx (List id_quote stx) ctx) env scps_p Σ)
   (values (Stx (List id_quote stx) ctx) Σ)
   (where quote (resolve ph id_quote Σ))]
  
  ;; syntax
  [(expand ph (Stx (List id_syntax stx) ctx) env scps_p Σ)
   (values (Stx (List id_syntax stx_pruned) ctx) Σ)
   (where syntax (resolve ph id_syntax Σ))
   (where stx_pruned (prune ph stx scps_p))]
  
  ;; macro creation
  [(expand ph (Stx (List id_ls id stx_rhs stx_body) ctx) env scps_p Σ)
   (expand ph stx_body2 env_2 scps_p2 Σ_4)
   (where let-syntax (resolve ph id_ls Σ))
   (where (values nam_new Σ_1) (alloc-name id Σ))
   (where (values scp_new Σ_2) (alloc-scope id Σ_1))
   (where id_new (add ph id scp_new))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where (values stx_exp Σ_4)
          (expand (plus ph 1) stx_rhs (primitives-env) (Set) Σ_3))
   (where env_2 (extend env nam_new (eval (parse (plus ph 1) stx_exp Σ_4))))
   (where stx_body2 (add ph stx_body scp_new))
   (where scps_p2 (union (Set scp_new) scps_p))]

  ;; macro invocation
  [(expand ph stx_macapp env scps_p Σ)
   (expand ph (flip ph stx_exp scp_i) env (union (Set scp_u) scps_p) Σ_3)
   (where (Stx (List id_mac stx_arg ...) ctx) stx_macapp)
   (where val (lookup env (resolve ph id_mac Σ)))
   (where (values scp_u Σ_2) (alloc-scope (Stx (Sym a) (Map)) Σ))
   (where (values scp_i Σ_3) (alloc-scope (Stx (Sym a) (Map)) Σ_2))
   (where stx_exp (eval (App val (flip ph (add ph stx_macapp scp_u) scp_i))))]
  
  ;; application
  [(expand ph (Stx (List stx_rtor stx_rnd ...) ctx) env scps_p Σ)
   (values (Stx (List stx_exprtor stx_exprnd ...) ctx) Σ_1)
   (where (values (stx_exprtor stx_exprnd ...) Σ_1)
          (expand* ph () (stx_rtor stx_rnd ...) env scps_p Σ))]
  
  ;; reference
  [(expand ph id env scps_p Σ)
   (values id_new Σ)
   (where (TVar id_new) (lookup env (resolve ph id Σ)))])

(define-metafunction Lph
  expand* : ph (stx ...) (stx ...) env scps Σ -> (values (stx ...) Σ)
  [(expand* ph (stx_done ...) () env scps_p Σ) (values (stx_done ...) Σ)]
  [(expand* ph (stx_done ...) (stx_0 stx_1 ...) env scps_p Σ)
   (expand* ph (stx_done ... stx_done0) (stx_1 ...) env scps_p Σ_1)
   (where (values stx_done0 Σ_1) (expand ph stx_0 env scps_p Σ))])

;; ----------------------------------------
;; Examples:

(define-metafunction Lph
  parse/values : (values stx Σ) -> ast
  [(parse/values (values stx Σ)) (parse 0 stx Σ)])

(define-example-definer define-example
  Lph
  (lambda (t) (term (expand 0 ,t (primitives-env) (Set) (init-store))))
  parse/values
  (Map))

(define-core-examples
  define-example
  simple-macro-example
  reftrans-macro-example z:6
  hyg-macro-example      z:8
  thunk-example          a:4 a:8
  get-identity-example   a:6 a:8)

(define-syntax-rule (define-phases-examples
                      define-example
                      prune-example
                      gen-example)
  (begin
    (define-example prune-example
      ;; This example fails if we make `prune` a no-op
      (let-syntax x (lambda e
                      ((lambda id1
                         ((lambda id2
                            ('MKS
                             ('LIST (syntax let-syntax)
                                    (syntax f)
                                    ('MKS
                                     ('LIST
                                      (syntax lambda)
                                      id2
                                      ('MKS
                                       ('LIST (syntax 'CAR)
                                              ('MKS ('LIST (syntax 'CDR)
                                                           ('MKS ('LIST (syntax 'SE)
                                                                        id1)
                                                                 e))
                                                    e))
                                       e))
                                     e)
                                    (syntax (f '3)))
                             e))
                          (syntax y)))
                       (syntax y)))
                     (x))
      3)

    (define-example gen-example
      ;; This example works even without pruning, since
      ;; the extra scopes on `id1` and `id2` are at phase 1,
      ;; and the identifiers are resolved at phase 0
      (let-syntax x (lambda e
                      ((lambda id1
                         ((lambda id2
                            ('MKS
                             ('LIST (syntax lambda)
                                    id2
                                    id1)
                             e))
                          (syntax y)))
                       (syntax y)))
                  (x))
      (Fun (Var y:10) (Var y:10)))))

(define-phases-examples
  define-example
  prune-example
  gen-example)

;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           pict
           "config.rkt")
  (provide (all-defined-out))
  
  (define (make-expand-pict pos [contract? #f])
    (parameterize ([metafunction-cases (list pos)]
                   [linebreaks (and narrow-mode?
                                    (if contract?
                                        '(#f #t)
                                        '(#t)))])
      (WR (metafunction->pict expand #:contract? contract?))))
  (define expand-syntax-pict (make-expand-pict 2))
  (define expand-let-syntax-pict (make-expand-pict 3 #t))
  (define prune-pict
    (parameterize ([linebreaks (and narrow-mode?
                                    '(#f #f #t))])
      (WR (metafunction->pict prune #:contract? #t))))
  (define resolve-pict
    (WR (metafunction->pict resolve #:contract? #t)))
  
  (define new-nts '(ph))
  (define changed-nts '(ctx))
  (define language-delta-pict
    (WR (language->pict Lph
                        #:nts (append '(stx)
                                      changed-nts
                                      new-nts))))
  
  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Lph lw))))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view language-delta-pict
        expand-let-syntax-pict
        prune-pict
        resolve-pict))

(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-model.rkt" pict) all-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Multi-Phase"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lph #:nts (append all-nts new-nts))))
     (WR (metafunction->pict eval #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict core:strip #:contract? #t)))
     (WR (metafunction->pict expand #:contract? #t))
     (WR (metafunction->pict expand* #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict prune #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
