#lang racket
(require redex/reduction-semantics
         "define-example.rkt"
         slideshow/pict
         (rename-in (except-in "phases-model.rkt"
                               eval
                               expand
                               expand*)))

(provide Lloc
         eval eval* extend* unstop
         expand expand*
         define-local-examples
         (all-from-out "phases-model.rkt"))

(define-extended-language Lloc Lph
  [maybe-scp scp no-scope]
  [Σ* (Tup Σ scps scps)])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction Lloc
  eval : ph ast maybe-scp env Σ* -> (values val Σ*)
  
  ;; local value
  [(eval ph (App LOCAL-VALUE ast_id) scp_i env Σ*)
   (values (lookup env (resolve ph id_result Σ_2)) Σ*_2)
   (where (values id_result Σ*_2) (eval ph ast_id scp_i env Σ*))
   (where (Tup Σ_2 _ _) Σ*_2)]

  ;; local expand
  [(eval ph (App LOCAL-EXPAND ast ast_stops) scp_i env Σ*)
   (values (flip ph stx_exp scp_i) Σ*_4)
   (where (values stx Σ*_2) (eval ph ast scp_i env Σ*))
   (where (values (List id_stop ...) Σ*_3) (eval ph ast_stops scp_i env Σ*_2))
   (where env_unstops
          ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p))))) (term env)))
   (where (Tup Σ_3 _ _) Σ*_3)
   (where (Seq nam_stop ...) (Seq (resolve ph id_stop Σ_3) ...))
   (where env_stops
          (extend* env_unstops ((nam_stop (TStop (lookup env_unstops nam_stop))) ...)))
   (where (values stx_exp Σ*_4) (expand ph (flip ph stx scp_i) env_stops Σ*_3))]
  
  ;; local binder
  [(eval ph (App LOCAL-BINDER ast_id) scp_i env Σ*)
   (values (prune ph id_result scps_u2) Σ*_2)
   (where (values id_result Σ*_2) (eval ph ast_id scp_i env Σ*))
   (where (Tup _ _ scps_u2) Σ*_2)]

  ;; core cases
  [(eval ph (App ast_fun ast_arg) maybe-scp env Σ*)
   (eval ph (subst ast_body var val_arg) maybe-scp env Σ*_3)
   (where (values (Fun var ast_body) Σ*_2) (eval ph ast_fun maybe-scp env Σ*))
   (where (values val_arg Σ*_3) (eval ph ast_arg maybe-scp env Σ*_2))]
  [(eval ph (App prim ast_arg ...) maybe-scp env Σ*)
   (values (δ prim (val_arg ...)) Σ*_2)
   (where (values (val_arg ...) Σ*_2) (eval* ph () (ast_arg ...) maybe-scp env Σ*))]
  [(eval ph val maybe-scp env Σ*)
   (values val Σ*)])

(define-metafunction Lloc
  eval* : ph (val ...) (ast ...) scp env Σ* -> (values (val ...) Σ*)
  [(eval* ph (val ...) () scp env Σ*)
   (values (val ...) Σ*)]
  [(eval* ph (val ...) (ast_0 ast_1 ...) scp env Σ*)
   (eval* ph (val ... val_0) (ast_1 ...) scp env Σ*_2)
   (where (values val_0 Σ*_2) (eval ph ast_0 scp env Σ*))])

(define-metafunction Lloc
  extend* : env ((nam all-transform) ...) -> env
  [(extend* env ((nam all-transform) ...)) ((nam all-transform) ... . env)])

(define-metafunction Lloc
  unstop : all-transform -> all-transform
  [(unstop (TStop all-transform)) all-transform]
  [(unstop all-transform) all-transform])

;; ----------------------------------------
;; The expander:

(define-metafunction Lloc
  expand : ph stx env Σ* -> (values stx Σ*)
  
  ;; stops
  [(expand ph (Stx (List id stx ...) ctx) env Σ*)
   (values (Stx (List id stx ...) ctx) Σ*)
   (where (Tup Σ _ _) Σ*)
   (where (TStop _) (lookup env (resolve ph id Σ)))]
  
  ;; lambda (unchanged)
  [(expand ph (Stx (List id_lam id_arg stx_bdy) ctx) env (Tup Σ scps_p scps_u))
   (values (Stx (List id_lam id_new stx_bdy2) ctx) (Tup Σ_4 scps_p scps_u))
   (where lambda (resolve ph id_lam Σ))
   (where (values nam_new Σ_1) (alloc-name id_arg Σ))
   (where (values scp_new Σ_2) (alloc-scope id_arg Σ_1))
   (where id_new (add ph id_arg scp_new))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where env_new (extend env nam_new (TVar id_new)))
   (where Σ*_3 (Tup Σ_3 (union (Set scp_new) scps_p) (Set)))
   (where (values stx_bdy2 (Tup Σ_4 _ _))
          (expand ph (add ph stx_bdy scp_new) env_new Σ*_3))]

  ;; quote (unchanged)
  [(expand ph (Stx (List id_quote stx) ctx) env Σ*)
   (values (Stx (List id_quote stx) ctx) Σ*)
   (where (Tup Σ _ _) Σ*)
   (where quote (resolve ph id_quote Σ))]
  
  ;; syntax (unchanged)
  [(expand ph (Stx (List id_syntax stx) ctx) env Σ*)
   (values (Stx (List id_syntax stx_pruned) ctx) Σ*)
   (where (Tup Σ scps_p scps_u) Σ*)
   (where syntax (resolve ph id_syntax Σ))
   (where stx_pruned (prune ph stx scps_p))]

  ;; macro creation (eval gets more and updates store)
  [(expand ph (Stx (List id_ls id stx_rhs stx_body) ctx) env (Tup Σ scps_p scps_u))
   (values stx_result (Tup Σ_6 scps_p scps_u))
   (where let-syntax (resolve ph id_ls Σ))
   (where (values nam_new Σ_1) (alloc-name id Σ))
   (where (values scp_new Σ_2) (alloc-scope id Σ_1))
   (where id_new (add ph id scp_new))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where (values stx_exp (Tup Σ_4 _ _))
          (expand (plus ph 1) stx_rhs (primitives-env) (Tup Σ_3 (Set) (Set))))
   (where (values val_exp (Tup Σ_5 _ _))
          (eval ph (parse (plus ph 1) stx_exp Σ_4) no-scope env (Tup Σ_4 scps_p (Set))))
   (where env_new (extend env nam_new val_exp))
   (where stx_body2 (add ph stx_body scp_new))
   (where (values stx_result (Tup Σ_6 _ _))
          (expand ph stx_body2 env_new (Tup Σ_5 (union (Set scp_new) scps_p) (Set))))]

  ;; macro invocation (eval gets more and updates store)
  [(expand ph stx_macapp env (Tup Σ scps_p scps_u))
   (values stx_result Σ*_5)
   (where (Stx (List id_mac stx_arg ...) ctx) stx_macapp)
   (where val (lookup env (resolve ph id_mac Σ)))
   (where (values scp_u Σ_2) (alloc-scope (Stx (Sym a) (Map)) Σ))
   (where (values scp_i Σ_3) (alloc-scope (Stx (Sym a) (Map)) Σ_2))
   (where Σ*_3 (Tup Σ_3 (union (Set scp_u) scps_p) (union (Set scp_u) scps_u)))
   (where stx_macapp2 (flip ph (add ph stx_macapp scp_u) scp_i))
   (where (values stx_exp Σ*_4)
          (eval ph (App val stx_macapp2) scp_i env Σ*_3))
   (where (values stx_result Σ*_5)
          (expand ph (flip ph stx_exp scp_i) env Σ*_4))]
  
  ;; application (unchanged)
  [(expand ph (Stx (List stx_rtor stx_rnd ...) ctx) env (Tup Σ scps_p scps_u))
   (values (Stx (List stx_exprtor stx_exprnd ...) ctx) (Tup Σ_1 scps_p scps_u))
   (where (values (stx_exprtor stx_exprnd ...) Σ_1)
          (expand* ph () (stx_rtor stx_rnd ...) env (Tup Σ scps_p (Set))))]
    
  ;; reference (unchanged)
  [(expand ph id env Σ*)
   (values id_new Σ*)
   (where (Tup Σ _ _) Σ*)
   (where (TVar id_new) (lookup env (resolve ph id Σ)))])

(define-metafunction Lloc
  expand* : ph (stx ...) (stx ...) env Σ* -> (values (stx ...) Σ)
  [(expand* ph (stx_done ...) () env (Tup Σ _ _)) (values (stx_done ...) Σ)]
  [(expand* ph (stx_done ...) (stx_0 stx_1 ...) env (Tup Σ scps_p (Set)))
   (expand* ph (stx_done ... stx_done0) (stx_1 ...) env (Tup Σ_2 scps_p (Set)))
   (where (values stx_done0 (Tup Σ_2 _ _)) (expand ph stx_0 env (Tup Σ scps_p (Set))))])

;; ----------------------------------------
;; Examples:

(define-metafunction Lloc
  parse/values : (values stx Σ*) -> ast
  [(parse/values (values stx (Tup Σ _ _))) (parse 0 stx Σ)])

(define-example-definer define-example
  Lloc
  (lambda (t) (term (expand 0 ,t (primitives-env) (Tup (init-store) (Set) (Set)))))
  parse/values
  (Map))

(define-core-examples
  define-example
  simple-macro-example
  reftrans-macro-example z:6
  hyg-macro-example      z:8
  thunk-example          a:4 a:8
  get-identity-example   a:6 a:8)

(define-phases-examples
  define-example
  prune-example
  gen-example)

(define-syntax-rule (define-local-examples
                      define-example
                      local-value-example
                      local-expand-example
                      local-expand-stop-example
                      nested-local-expand-example
                      local-binder-example)
  (begin
    (define-example local-value-example
      (let-syntax a '8
        (let-syntax b '9
          (let-syntax x (lambda s 
                          ('MKS
                           ('LIST (syntax quote)
                                  ('MKS ('LOCAL-VALUE ('CAR ('CDR ('SE s))))
                                        (syntax here)))
                           (syntax here)))
            (x a))))
      8)
    
    (define-example local-expand-example
      (let-syntax q (lambda s (syntax ('CAR '8)))
        (let-syntax x (lambda s 
                        ;; Used as (x (q)) => extracts '8 from ('CAR '8)
                        ('CAR ('CDR ('SE ('LOCAL-EXPAND ('CAR ('CDR ('SE s))) 
                                                        ('LIST))))))
          (x (q))))
      8)
    
    (define-example local-expand-stop-example
      (let-syntax p (lambda s (quote 0))
        (let-syntax q (lambda s (syntax ('CAR '8)))
          (let-syntax x (lambda s 
                          ;; Used as (x (q)) => extracts '8 from ('CAR '8)
                          ('CAR ('CDR ('SE ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                                          ('LIST (syntax p)))))))
            (x (q)))))
      8)
    
    (define-example nested-local-expand-example
      (let-syntax z (lambda s (syntax '0))
        (let-syntax a (lambda s
                        ;; When `b' forces `a', then `a'
                        ;; drops `z' form the stop list, so it
                        ;; should expand to 0
                        ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                       ('LIST)))
          (let-syntax b (lambda s
                          ('MKS
                           ('LIST
                            (syntax quote)
                            ('LOCAL-EXPAND ('CAR ('CDR ('SE s)))
                                           ('LIST (syntax z))))
                           s))
            ('LIST (b (z)) (b (a (z)))))))
      (App LIST (List (Sym z)) (List (Sym quote) 0)))
    
    (define-example local-binder-example
      (let-syntax q (lambda e
                         ;; quotes its argument
                         ('MKS
                          ('LIST (syntax quote) ('CAR ('CDR ('SE e))))
                          e))
                     (let-syntax a (lambda e
                                     ;; expands first argument, expected quoted name
                                     ;; to use as binder with second arguments body
                                     ('MKS
                                      ('LIST
                                       (syntax lambda)
                                       ('LOCAL-BINDER
                                        ('CAR ('CDR ('SE ('LOCAL-EXPAND ('CAR ('CDR ('SE e)))
                                                                        ('LIST))))))
                                       ('CAR ('CDR ('CDR ('SE e)))))
                                      e))
                                 ;; removing the LOCAL-BINDER call above
                                 ;; leaves the second `x` as unbound:
                                 (a (q x) x)))
      (Fun (Var x:12) (Var x:12)))))

(define-local-examples
  define-example
  local-value-example
  local-expand-example
  local-expand-stop-example
  nested-local-expand-example
  local-binder-example)

;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           pict
           "config.rkt")
  (provide (all-defined-out))
  
  (define eval-pict
    (if narrow-mode?
        ;; Independent form:
        (vl-append
         (metafunction-rule-gap-space)
         (parameterize ([metafunction-cases '(0)])
           (WR (metafunction->pict eval #:contract? #t)))
         (parameterize ([metafunction-cases '(1)])
           (WR (metafunction->pict eval)))
         (parameterize ([metafunction-cases '(2)])
           (WR (metafunction->pict eval))))
        ;; Table form:
        (parameterize ([metafunction-cases '(0 1 2)])
          (WR (metafunction->pict eval #:contract? #t)))))
  
  (define (make-expand-pict pos [contract? #f])
    (parameterize ([metafunction-cases (list pos)])
      (WR (metafunction->pict expand #:contract? contract?))))
  
  (define expand-stop-pict (make-expand-pict 0 #t))
  (define expand-macro-app-pict (make-expand-pict 5))
  (define expand-lambda-pict
    (parameterize ([linebreaks (list #t)])
      (make-expand-pict 1)))

  (define unstop-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict unstop #:contract? #t))))
  
  (define newer-nts '(maybe-scp Σ*))
  (define language-delta-pict
    (WR (language->pict Lloc #:nts newer-nts)))
  
  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Lloc lw))))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view language-delta-pict
        eval-pict
        expand-stop-pict
        expand-lambda-pict
        unstop-pict))

(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-model.rkt" pict) all-nts)
           (only-in (submod "phases-model.rkt" pict)
                    changed-nts
                    new-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Local-Expansion"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lloc #:nts (append newer-nts
                                              new-nts
                                              all-nts))))
     (WR (metafunction->pict eval #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict unstop #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict core:strip #:contract? #t)))
     (parameterize ([linebreaks '(#f #f #t #f #t #f #f #t #f)])
       (WR (metafunction->pict expand #:contract? #t)))
     (parameterize ([linebreaks '(#f #f #t)])
       (WR (metafunction->pict expand* #:contract? #t)))
     (WR (metafunction->pict prune #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
