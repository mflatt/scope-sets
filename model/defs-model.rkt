#lang racket
(require redex/reduction-semantics
         "define-example.rkt"
         slideshow/pict
         (rename-in (except-in "local-model.rkt"
                               expand expand*
                               eval eval*)
                    [parse local:parse]))

(define-extended-language Ldef Lloc
  ;; Refines the third component of Σ:
  [boxes ([addr val] ...)]
  ;; Refines the fourth component of Σ:
  [def-envs ([addr env] ...)])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction Ldef
  eval : ph ast maybe-scp env Σ* -> (values val Σ*)

  ;; create definition context
  [(eval ph (App NEW-DEFS) scp_i env (Tup Σ scps_p scps_u))
   (values (Defs scp_defs addr) Σ*_3)
   (where (values scp_defs Σ_2) (alloc-scope (Stx (Sym defs) (Map)) Σ))
   (where (values addr Σ_3) (alloc-def-env Σ_2))
   (where Σ*_3 (Tup (def-env-update Σ_2 addr env) (union (Set scp_defs) scps_p) scps_u))]
  
  ;; create definition binding (for a variable)
  [(eval ph (App DEF-BIND ast_defs ast_id) scp_i env Σ*)
   (values 0 (Tup Σ_6 scps_p3 scps_u3))
   (where (values (Defs scp_defs addr) Σ*_2) (eval ph ast_defs scp_i env Σ*))
   (where (values id_arg Σ*_3) (eval ph ast_id scp_i env Σ*_2))
   (where (Tup Σ_3 scps_p3 scps_u3) Σ*_3)
   (where id_defs (add ph (prune ph (flip ph id_arg scp_i) scps_u3) scp_defs))
   (where (values nam_new Σ_4) (alloc-name id_defs Σ_3))
   (where Σ_5 (bind ph Σ_4 id_defs nam_new))
   (where env_defs (def-env-lookup Σ_5 addr))
   (where Σ_6 (def-env-update Σ_5 addr (extend env_defs nam_new (TVar id_defs))))]

  ;; create macro definition binding
  [(eval ph (App DEF-BIND ast_defs ast_id ast_stx) scp_i env Σ*)
   (values 0 Σ*_9)
   (where (values (Defs scp_defs addr) Σ*_2) (eval ph ast_defs scp_i env Σ*))
   (where (values id_arg Σ*_3) (eval ph ast_id scp_i env Σ*_2))
   (where (values stx_arg Σ*_4) (eval ph ast_stx scp_i env Σ*_3))
   (where (Tup Σ_4 scps_p4 scps_u4) Σ*_4)
   (where stx_arg2 (add ph (flip ph stx_arg scp_i) scp_defs))
   (where (values stx_exp (Tup Σ_5 _ _))
          (expand (plus ph 1) stx_arg2 (primitives-env) (Tup Σ_4 (Set) (Set))))
   (where (values val_exp Σ*_6)
          (eval ph (parse (plus ph 1) stx_exp Σ_5) no-scope env (Tup Σ_5 scps_p4 (Set))))
   (where (Tup Σ_6 _ _) Σ*_6)
   (where env_defs (def-env-lookup Σ_6 addr))
   (where id_defs (add ph (prune ph (flip ph id_arg scp_i) scps_u4) scp_defs))
   (where (values nam_new Σ_7) (alloc-name id_defs Σ_6))
   (where Σ_8 (bind ph Σ_7 id_defs nam_new))
   (where Σ*_9 (Tup (def-env-update Σ_8 addr (extend env_defs nam_new val_exp)) scps_p4 scps_u4))]

  ;; local expand with definition context
  ;; - similar to the basic local expand case, but adding the
  ;;   definition context's scope and using its environment
  [(eval ph (App LOCAL-EXPAND ast_expr ast_stops ast_defs) scp_i env Σ*)
   (values stx_exp2 Σ*_5)
   (where (values stx Σ*_2) (eval ph ast_expr scp_i env Σ*))
   (where (values (List id_stop ...) Σ*_3) (eval ph ast_stops scp_i env Σ*_2))
   (where (values (Defs scp_defs addr) Σ*_4) (eval ph ast_defs scp_i env Σ*_3))
   (where (Tup Σ_4 _ _) Σ*_4)
   (where env_defs (def-env-lookup Σ_4 addr))
   (where env_unstops
          ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p))))) (term env_defs)))
   (where (Seq nam_stop ...) (Seq (resolve ph id_stop Σ_4) ...))
   (where env_stops
          (extend* env_unstops ((nam_stop (TStop (lookup env_unstops nam_stop))) ...)))
   (where (values stx_exp Σ*_5) (expand ph (add ph (flip ph stx scp_i) scp_defs) env_stops Σ*_4))
   (where stx_exp2 (flip ph stx_exp scp_i))]
  
  ;; ----------------------------------------
  ;; Including boxes lets us implement recursive definitions as a
  ;; macro, including variable definitions that are in a recursive
  ;; binding scope with macros.

  ;; box
  [(eval ph (App BOX ast_val) maybe-scp env Σ*)
   (values addr (Tup (box-update Σ_3 addr val) scps_p2 scps_u2))
   (where (values val (Tup Σ_2 scps_p2 scps_u2)) (eval ph ast_val maybe-scp env Σ*))
   (where (values addr Σ_3) (alloc-box Σ_2))]
  ;; unbox
  [(eval ph (App UNBOX ast_box) maybe-scp env Σ*)
   (values (box-lookup Σ_2 addr) Σ*_2)
   (where (values addr Σ*_2) (eval ph ast_box maybe-scp env Σ*))
   (where (Tup Σ_2 scps_p2 scps_u2) Σ*_2)]
  ;; set-box!
  [(eval ph (App SET-BOX! ast_box ast_val) maybe-scp env Σ*)
   (values val (Tup (box-update Σ_3 addr val) scps_p3 scps_u3))
   (where (values addr Σ*_2) (eval ph ast_box maybe-scp env Σ*))
   (where (values val Σ*_3) (eval ph ast_val maybe-scp env Σ*_2))
   (where (Tup Σ_3 scps_p3 scps_u3) Σ*_3)]
  
  ;; ----------------------------------------
  ;; The remaining caes are the same as for local-model
  
  ;; local value
  [(eval ph (App LOCAL-VALUE ast_id) scp_i env Σ*)
   (values (lookup env (resolve ph id_result Σ_2)) Σ*_2)
   (where (values id_result Σ*_2) (eval ph ast_id scp_i env Σ*))
   (where (Tup Σ_2 _ _) Σ*_2)]

  ;; local expand
  [(eval ph (App LOCAL-EXPAND ast_expr ast_stops) scp_i env Σ*)
   (values (flip ph stx_exp scp_i) Σ*_4)
   (where (values stx Σ*_2) (eval ph ast_expr scp_i env Σ*))
   (where (values (List id_stop ...) Σ*_3) (eval ph ast_stops scp_i env Σ*_2))
   (where env_unstops
          ,(map (lambda (p) (list (car p) (term (unstop ,(cadr p))))) (term env)))
   (where (Tup Σ_3 _ _) Σ*_3)
   (where env_stops
          (extend* env_unstops (((resolve ph id_stop Σ_3) (TStop (lookup env (resolve ph id_stop Σ_3)))) ...)))
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

(define-metafunction Ldef
  eval* : ph (val ...) (ast ...) scp env Σ* -> (values (val ...) Σ*)
  [(eval* ph (val ...) () scp env Σ*)
   (values (val ...) Σ*)]
  [(eval* ph (val ...) (ast_0 ast_1 ...) scp env Σ*)
   (eval* ph (val ... val_0) (ast_1 ...) scp env Σ*_2)
   (where (values val_0 Σ*_2) (eval ph ast_0 scp env Σ*))])

;; ----------------------------------------
;; Box allocations and updates:

(define-metafunction Ldef
  alloc-box : Σ -> (values addr Σ)
  [(alloc-box (Sto number (binds ...) boxes def-envs))
   (values ,(string->symbol (format "bx:~a" (term number)))
           (Sto ,(add1 (term number )) (binds ...) boxes def-envs))])

(define-metafunction Ldef
  box-lookup : Σ addr -> val
  [(box-lookup (Sto _ _ (_ ... [addr val] _ ...) _) addr) val])

(define-metafunction Ldef
  box-update : Σ addr val -> Σ
  [(box-update (Sto number (binds ...) (any_1 ... [addr _] any_2 ...) def-envs) addr val)
   (Sto number (binds ...) (any_1 ... [addr val] any_2 ...) def-envs)]
  [(box-update (Sto number (binds ...) (any_1 ...) def-envs) addr val)
   (Sto number (binds ...) ([addr val] any_1 ...) def-envs)])

;; ----------------------------------------
;; Definition-context environment allocations and updates:

(define-metafunction Ldef
  alloc-def-env : Σ -> (values addr Σ)
  [(alloc-def-env (Sto number (binds ...) boxes def-envs))
   (values ,(string->symbol (format "env:~a" (term number)))
           (Sto ,(add1 (term number )) (binds ...) boxes def-envs))])

(define-metafunction Ldef
  def-env-lookup : Σ addr -> env
  [(def-env-lookup (Sto _ _ _ (_ ... [addr env] _ ...)) addr) env])

(define-metafunction Ldef
  def-env-update : Σ addr env -> Σ
  [(def-env-update (Sto number (binds ...) boxes (any_1 ... [addr _] any_2 ...)) addr env)
   (Sto number (binds ...) boxes (any_1 ... [addr env] any_2 ...))]
  [(def-env-update (Sto number (binds ...) boxes (any_1 ...)) addr env)
   (Sto number (binds ...) boxes ([addr env] any_1 ...))])

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

(define-metafunction Ldef
  parse : ph stx Σ -> ast
  ;; This parse is the same as the multi-phase one, just
  ;; repeated here to access the right `resolve`
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
;; The expander:

;; The expander is the same as in local-model, just copied
;; here to use the updated `eval`.

(define-metafunction Ldef
  expand : ph stx env Σ* -> (values stx Σ*)
  
  ;; stops
  [(expand ph (Stx (List id_stop stx ...) ctx) env Σ*)
   (values (Stx (List id_stop stx ...) ctx) Σ*)
   (where (Tup Σ _ _) Σ*)
   #;
   (side-condition (begin
                     (printf "lookup ~s\n resolves ~a\n" 
                             (term id_stop)
                             (term (resolve ph id_stop Σ)))
                     (pretty-print (term env))
                     (pretty-print (term Σ))))
   (where (TStop _) (lookup env (resolve ph id_stop Σ)))]
  
  ;; lambda (unchanged)
  [(expand ph (Stx (List id_lam id_arg stx_body) ctx) env (Tup Σ scps_p scps_u))
   (values (Stx (List id_lam id_new stx_body2) ctx) (Tup Σ_4 scps_p scps_u))
   (where lambda (resolve ph id_lam Σ))
   (where (values nam_new Σ_1) (alloc-name id_arg Σ))
   (where (values scp_new Σ_2) (alloc-scope id_arg Σ_1))
   (where id_new (add ph id_arg scp_new))
   (where Σ_3 (bind ph Σ_2 id_new nam_new))
   (where env_new (extend env nam_new (TVar id_new)))
   (where (values stx_body2 (Tup Σ_4 _ _))
          (expand ph (add ph stx_body scp_new) env_new (Tup Σ_3 (union (Set scp_new) scps_p) (Set))))]

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
   (where (values scp_u Σ_2) (alloc-scope (Stx (Sym u) (Map)) Σ))
   (where (values scp_i Σ_3) (alloc-scope (Stx (Sym i) (Map)) Σ_2))
   (where Σ*_3 (Tup Σ_3 (union (Set scp_u) scps_p) (union (Set scp_u) scps_u)))
   (where (values stx_exp Σ*_4)
          (eval ph (App val (flip ph (add ph stx_macapp scp_u) scp_i)) scp_i env Σ*_3))
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

(define-metafunction Ldef
  expand* : ph (stx ...) (stx ...) env Σ* -> (values (stx ...) Σ)
  [(expand* ph (stx_done ...) () env (Tup Σ _ _)) (values (stx_done ...) Σ)]
  [(expand* ph (stx_done ...) (stx_0 stx_1 ...) env (Tup Σ scps_p (Set)))
   (expand* ph (stx_done ... stx_done0) (stx_1 ...) env (Tup Σ_2 scps_p (Set)))
   (where (values stx_done0 (Tup Σ_2 _ _)) (expand ph stx_0 env (Tup Σ scps_p (Set))))])

;; ----------------------------------------
;; Examples:

(define-metafunction Ldef
  parse/values : (values stx Σ*) -> ast
  [(parse/values (values stx (Tup Σ _ _))) (parse 0 stx Σ)])

(define-example-definer define-example
  Ldef
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

(define-local-examples
  define-example
  local-value-example
  local-expand-example
  local-expand-stop-example
  nested-local-expand-example
  local-binder-example)

(define-example box-example
  (let-syntax m (lambda e
                  ('MKS
                   ('LIST
                    (syntax quote)
                    ('MKS
                     ((lambda b ('UNBOX b))
                      ('BOX '0))
                     e))
                   e))
              (m))
  0)

(define-example set-box-example
  (let-syntax m (lambda e
                  ('MKS
                   ('LIST
                    (syntax quote)
                    ('MKS
                     ((lambda b
                        ((lambda x
                           ('UNBOX b))
                         ('SET-BOX! b '1)))
                      ('BOX '0))
                     e))
                   e))
              (m))
  1)

(define-example defs-shadow-example
  (let-syntax call (lambda s ('MKS ('LIST ('CAR ('CDR ('SE s))))
                              (syntax here)))
    (let-syntax p (lambda s (syntax '0))
      (let-syntax q (lambda s
                      ((lambda defs
                         ((lambda ignored
                            ('MKS
                             ('LIST
                              (syntax lambda)
                              ('LOCAL-BINDER ; not necessary in this case, but sensible
                               ('CAR ('CDR
                                      ('SE
                                       ('LOCAL-EXPAND ('MKS
                                                       ('LIST (syntax quote)
                                                              ('CAR ('CDR ('SE s))))
                                                       (syntax here))
                                                      ('LIST)
                                                      defs)))))
                              ('LOCAL-EXPAND ('CAR ('CDR ('CDR ('SE s))))
                                             ('LIST (syntax call))
                                             defs))
                             (syntax here)))
                          ('DEF-BIND defs ('CAR ('CDR ('SE s))))))
                       ('NEW-DEFS)))
        (q p (call p)))))
  (Fun (Var p:20) (App (Var p:20))))

;; Like the previous example, but using a macro that expands to `quote`:
(define-example defs-shadow2-example
  (let-syntax call (lambda s ('MKS ('LIST ('CAR ('CDR ('SE s))))
                              (syntax here)))
   (let-syntax qt (lambda s ('MKS ('LIST (syntax quote) ('CAR ('CDR ('SE s))))
                             (syntax here)))
    (let-syntax p (lambda s (syntax '0))
      (let-syntax q (lambda s
                      ((lambda defs
                         ((lambda ignored
                            ('MKS
                             ('LIST
                              (syntax lambda)
                              ('LOCAL-BINDER ; necessary in this case
                               ('CAR ('CDR
                                      ('SE
                                       ('LOCAL-EXPAND ('MKS
                                                       ('LIST (syntax qt)
                                                              ('CAR ('CDR ('SE s))))
                                                       (syntax here))
                                                      ('LIST)
                                                      defs)))))
                              ('LOCAL-EXPAND ('CAR ('CDR ('CDR ('SE s))))
                                             ('LIST (syntax call))
                                             defs))
                             (syntax here)))
                          ('DEF-BIND defs ('CAR ('CDR ('SE s))))))
                       ('NEW-DEFS)))
        (q p (call p))))))
  (Fun (Var p:26) (App (Var p:26))))

(define-example defs-local-macro-example
  (let-syntax call (lambda s ('MKS ('LIST ('CAR ('CDR ('SE s)))) 
                              (syntax here)))
    (let-syntax p (lambda s (syntax '0))
      (let-syntax q (lambda s
                      ((lambda defs
                         ((lambda ignored
                            ('MKS
                             ('LIST
                              (syntax lambda)
                              ('CAR ('CDR
                                     ('SE
                                      ('LOCAL-EXPAND ('MKS 
                                                      ('LIST (syntax quote)
                                                             ('CAR ('CDR ('SE s))))
                                                      (syntax here))
                                                     ('LIST)
                                                     defs))))
                              ('LOCAL-EXPAND ('CAR ('CDR ('CDR ('SE s))))
                                             ('LIST)
                                             defs))
                             (syntax here)))
                          ('DEF-BIND defs ('CAR ('CDR ('SE s)))
                            ('MKS
                             ('LIST (syntax lambda)
                                    (syntax s)
                                    ('CAR ('CDR ('CDR ('CDR ('SE s))))))
                             (syntax here)))))
                       ('NEW-DEFS)))
        (q p (call p) (syntax '13)))))
  (Fun (Var p:26) 13))

(define-example defs-begin-with-defn-example
  (let-syntax bwd (lambda s
                    ((lambda ctx ; the int-def context
                       ((lambda id1 ; the x in (define x '10)
                          ((lambda e1 ; the '10 in (define x '10)
                             ((lambda id2 ; the q n (define-syntax q (lambda  v ...))
                                ((lambda e2 ; the (lambda v ...) in (define-syntax q (lambda  v ...))
                                   ((lambda e3 ; the last body expression, expands to (lambda i x)
                                      ((lambda ignored ; for side-effect of binding x in ctx
                                         ((lambda ignored ; for side-effect of binding q in ctx
                                            ((lambda ee3 ; local-expand e3
                                               ((lambda qid1 ; local-expand id1 (in a syntax form)
                                                  ((lambda eid1 ; extract expanded id1 from qid1
                                                     ;; generate ((lambda eid1 ee3) '10):
                                                     ('MKS ('LIST
                                                            ('MKS ('LIST (syntax lambda)
                                                                         eid1
                                                                         ee3)
                                                                  (syntax here))
                                                            e1)
                                                           (syntax here)))
                                                   ('CAR ('CDR ('SE qid1)))))
                                                ;; local-expand of id1 (to give it context from ctx):
                                                ('LOCAL-EXPAND ('MKS ('LIST (syntax quote)
                                                                            id1)
                                                                     (syntax here))
                                                               ('LIST (syntax quote))
                                                               ctx)))
                                             ;; local-expand e3 (i.e., the body expression):
                                             ('LOCAL-EXPAND e3 ('LIST (syntax lambda)) ctx)))
                                          ;; bind id2 (i.e., q)
                                          ('DEF-BIND ctx id2 e2)))
                                       ;; bind id1 (i.e., x)
                                       ('DEF-BIND ctx id1)))
                                    ;; extract e3
                                    ('CAR ('CDR ('CDR ('CDR ('SE s)))))))
                                 ;; extract e2
                                 ('CAR ('CDR ('CDR ('SE ('CAR ('CDR ('CDR ('SE s))))))))))
                              ;; extract id2
                              ('CAR ('CDR ('SE ('CAR ('CDR ('CDR ('SE s)))))))))
                           ;; extract e1
                           ('CAR ('CDR ('CDR ('SE ('CAR ('CDR ('SE s)))))))))
                        ;; extract id1
                        ('CAR ('CDR ('SE ('CAR ('CDR ('SE s))))))))
                     ;; create ctx
                     ('NEW-DEFS)))
    ;; `bwd' is short for `begin-with-definitions', which
    ;; assumes a `define' followed by a `define-syntax' followed
    ;; by a body form
    (bwd (define x '10)
         (define-syntax q (lambda v (syntax (lambda i x))))
         #;(lambda i x)
         (q)))
  (App (Fun (Var x:35) (Fun (Var i:37) (Var x:35))) 10))

;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           pict
           "config.rkt")
  (provide (all-defined-out))
  
  (define (make-eval-pict pos)
    (parameterize ([metafunction-cases (list pos)])
      (WR (metafunction->pict eval))))
  
  (define eval-new-defs-pict (make-eval-pict 0))
  (define eval-def-bind-var-pict (make-eval-pict 1))
  (define eval-def-bind-syntax-pict (make-eval-pict 2))
  (define eval-local-expand-pict (make-eval-pict 3))

  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict Ldef lw))))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view eval-new-defs-pict
        eval-def-bind-var-pict
        eval-def-bind-syntax-pict
        eval-local-expand-pict))


(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict)
           (only-in (submod "core-model.rkt" pict) all-nts)
           (only-in (submod "phases-model.rkt" pict)
                    changed-nts
                    new-nts)
           (only-in (submod "local-model.rkt" pict)
                    newer-nts))
  (provide doc)
  (define doc
    (make-model-doc
     "Definition-Contexts"
     (parameterize ([extend-language-show-union #t])
       (WR (language->pict Lloc #:nts (append newer-nts
                                              new-nts
                                              all-nts))))
     (parameterize ([metafunction-cases (for/list ([n (in-range 0 10)]
                                                   #:unless (< 3 n 7))
                                          n)])
       (WR (metafunction->pict eval #:contract? #t)))
     (parameterize ([metafunction-cases (for/list ([n (in-range 10 13)])
                                          n)])
       (WR (metafunction->pict eval)))
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
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict prune #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
