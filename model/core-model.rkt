#lang racket
(require redex/reduction-semantics
         "define-example.rkt"
         slideshow/pict)

(provide L
         subst
         plus minus
         δ/stx δ
         addremove
         flip add strip
         subtract union
         bind resolve
         store-lookup binding-lookup biggest-subset
         parse lookup extend
         alloc-name alloc-scope
         primitives-env init-store
         define-core-examples)

(define-language L
  
  ;; Executable AST and values:
  [ast var (App ast ast ...) val]
  [var (Var nam)]
  [val (Fun var ast) atom (List val ...) stx]

  ;; Syntax objects (a subset of values):
  [stx (Stx atom ctx) (Stx (List stx ...) ctx)]
  [id (Stx sym ctx)]
  [ctx scps]
  [scps (Set scp ...)]
  
  ;; Literal values:
  [atom sym prim desc-other-atom] ; `desc-other-atom' typesets as "...."
  [desc-other-atom number
                   ;; Not used until definition-context model:
                   addr (Defs scp addr)]
  [sym (Sym nam)]
  [prim SE MKS desc-other-prim] ; `desc-other-prim' typesets as "...."
  [desc-other-prim + - CONS CAR CDR LIST
                   ;; Not implemented at first, but it's simplest
                   ;; to include these in the grammar from the start:
                   LOCAL-VALUE LOCAL-EXPAND LOCAL-BINDER
                   BOX UNBOX SET-BOX!
                   NEW-DEFS DEF-BIND]

  ;; Expand-time environment:
  [env desc-env] ; `desc-env' typesets as prose
  [desc-env ((nam all-transform) ...)]
  [transform lambda let-syntax quote syntax (TVar id) val]
  ;; The `TStop' transform type is not used at first:
  [all-transform transform (TStop all-transform)]

  ;; Expand-time store:
  [Σ desc-store] ; `desc-store' typesets as prose
  [desc-store (Sto number      ; for alloc
                   (binds ...) ; binding store
                   boxes       ; for later model
                   def-envs)]  ; for later model
  [binds [nam (StoBind scps nam) ...]]
  
  ;; Use names for vars, addrs, and scopes
  [nam desc-name] ; `desc-name' typesets as prose
  [desc-name variable-not-otherwise-mentioned
             lambda let-syntax quote syntax]
  [scp desc-scope] ; `desc-scope' typesets as prose
  [desc-scope nam]
  
  [boxes any] ; refined in a later model
  [def-envs any]  ; refined in a later model
  [addr nam]  ; used in a later model
  
  [nam-or-false nam #f])


;; ----------------------------------------
;; Non-capturing substitution for AST:

(define-metafunction L
  subst : ast var ast -> ast
  [(subst var var ast_v) ast_v]
  [(subst var_2 var ast_v) var_2]
  [(subst (App ast ...) var ast_v)
   (App (subst ast var ast_v) ...)]
  [(subst (Fun var ast) var ast_v)
   (Fun var ast)]
  [(subst (Fun var_2 ast) var ast_v)
   (Fun var_3 (subst (subst ast var_2 var_3) var ast_v))
   (where (Var nam_2) var_2)
   (where var_3 (Var ,(variable-not-in (term ast_v) (term nam_2))))]
  [(subst atom var ast_v) atom]
  [(subst (List val ...) var ast_v) 
   (List (subst val var ast_v) ...)]
  [(subst stx var ast_v) stx])

;; ----------------------------------------
;; Implementation of primitives:

(define-metafunction L
  [(plus number_1 number_2) ,(+ (term number_1) (term number_2))])
(define-metafunction L
  [(minus number_1 number_2) ,(+ (term number_1) (term number_2))])

(define-metafunction L
  δ/stx : prim (val ...) -> val
  [(δ/stx SE ((Stx val ctx))) val]
  [(δ/stx MKS (atom (Stx val ctx))) (Stx atom ctx)]
  [(δ/stx MKS ((List stx ...) (Stx val ctx))) (Stx (List stx ...) ctx)])

(define-metafunction/extension δ/stx L
  δ : prim (val ...) -> val
  [(δ + (number_1 number_2)) (plus number_1 number_2)]
  [(δ - (number_1 number_2)) (minus number_1 number_2)]
  [(δ CONS (val_1 (List val_2 ...))) (List val_1 val_2 ...)]
  [(δ CAR ((List val_1 val_2 ...))) val_1]
  [(δ CDR ((List val_1 val_2 ...))) (List val_2 ...)]
  [(δ LIST (val ...)) (List val ...)])

;; ----------------------------------------
;; Evaluating AST:

(define-metafunction L
  eval : ast -> val
  [(eval (App ast_fun ast_arg))
   (eval (subst ast_body var (eval ast_arg)))
   (where (Fun var ast_body) (eval ast_fun))]
  [(eval (App prim ast_arg ...))
   (δ prim ((eval ast_arg) ...))]
  [(eval val) val])

;; ----------------------------------------
;; Syntax-object operations:

(define-metafunction L
  add : stx scp -> stx
  ;; Simply pushes scopes down through a syntax object
  [(add (Stx atom ctx) scp) 
   (Stx atom (union (Set scp) ctx))]
  [(add (Stx (List stx ...) ctx) scp) 
   (Stx (List (add stx scp) ...) (union (Set scp) ctx))])

(define-metafunction L
  ;; Adds or cancels a scope
  addremove : scp scps -> scps
  [(addremove scp_2 (Set scp_1 ... scp_2 scp_3 ...)) (Set scp_1 ... scp_3 ...)]
  [(addremove scp_1 (Set scp_2 ...)) (Set scp_1 scp_2 ...)])

(define-metafunction L
  flip : stx scp -> stx
  ;; Pushes flipping a scope down through a syntax object
  [(flip (Stx atom ctx) scp) 
   (Stx atom (addremove scp ctx))]
  [(flip (Stx (List stx ...) ctx) scp) 
   (Stx (List (flip stx scp) ...) (addremove scp ctx))])

(define-metafunction L
  strip : stx -> val
  ;; Recursively strips lexical context from a syntax object
  [(strip (Stx atom ctx))
   atom]
  [(strip (Stx (List stx ...) ctx)) 
   (List (strip stx) ...)])

(define-metafunction L
  subtract : scps scps -> scps
  [(subtract scps (Set)) scps]
  [(subtract (Set scp_1 ... scp scp_2 ...) (Set scp scp_3 ...))
   (subtract (Set scp_1 ... scp_2 ...) (Set scp_3 ...))]
  [(subtract scps (Set scp scp_1 ...))
   (subtract scps (Set scp_1 ...))])

(define-metafunction L
  union : scps scps -> scps
  [(union (Set scp_1 ...) (Set scp_2 ...)) (Set scp_1 ... scp_2 ...)])

(define-metafunction L
  bind : Σ id nam -> Σ
  ;; Add a binding using the name and scopes of an identifier, mapping
  ;; them in the store to a given name
  [(bind (Sto number
              (binds_1 ... [nam_1 (StoBind ctx_2 nam_2) ...] binds_2 ...)
              boxes
              def-envs)
         (Stx (Sym nam_1) ctx_1)
         nam_3)
   (Sto number
        (binds_1 ... [nam_1 (StoBind ctx_1 nam_3) (StoBind ctx_2 nam_2) ...] binds_2 ...)
        boxes
        def-envs)]
  [(bind (Sto number (binds ...) boxes def-envs)
         (Stx (Sym nam_1) ctx_1)
         nam_3)
   (Sto number
        ([nam_1 (StoBind ctx_1 nam_3)] binds ...)
        boxes
        def-envs)])

(define-metafunction L
  resolve : id Σ -> nam
  [(resolve (Stx (Sym nam) ctx) Σ)
   nam_biggest
   (where (Set (StoBind scps_bind nam_bind) ...) (store-lookup Σ nam))
   (where scps_biggest (biggest-subset ctx (Set scps_bind ...)))
   (where nam_biggest (binding-lookup (Set (StoBind scps_bind nam_bind) ...) scps_biggest))]
  [(resolve (Stx (Sym nam) ctx) Σ)
   nam])

(define-metafunction L
  store-lookup : Σ nam -> (Set (StoBind scps nam) ...)
  [(store-lookup (Sto number (_ ... [nam (StoBind scps_bind nam_bind) ...] _ ...) _ _) nam)
   (Set (StoBind scps_bind nam_bind) ...)]
  [(store-lookup Σ nam) (Set)])

(define-metafunction L
  binding-lookup : (Set (StoBind scps nam) ...) scps -> nam-or-false
  [(binding-lookup (Set _ ... (StoBind scps nam) _ ...) scps) nam]
  [(binding-lookup _ scps) #f])

(define-metafunction L
  biggest-subset : scps (Set scps ...) -> scps
  [(biggest-subset scps_ref (Set scps_bind ...))
   scps_biggest
   (where scps_biggest
          ;; The biggest-subset search seems easiest to write in Racket:
          ,(let* ([matching
                   (filter (lambda (scps_bind)
                             (subset? scps_bind (term scps_ref)))
                           (term (scps_bind ...)))]
                  [sorted
                   (sort matching
                         (lambda (a b)
                           (> (length a) (length b))))])
             ;; The binding is ambigious if the first scps in
             ;; `sorted` is not bigger than the others, or if
             ;; some scps in `sorted` is not a subset of the
             ;; first one.
             (if (or (empty? sorted)
                     (and (pair? (rest sorted))
                          (= (length (first sorted))
                             (length (second sorted))))
                     (ormap (lambda (b)
                              (not (subset? b (first sorted))))
                            (rest sorted)))
                 #f
                 (first sorted))))]
  [(biggest-subset _ _) (Set)])

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

(define-metafunction L
  parse : stx Σ -> ast
  [(parse (Stx (List id_lam id stx_body) ctx) Σ)
   (Fun (Var (resolve id Σ))
        (parse stx_body Σ))
   (where lambda (resolve id_lam Σ))]
  [(parse (Stx (List id_quote stx) ctx) Σ)
   (strip stx)
   (where quote (resolve id_quote Σ))]
  [(parse (Stx (List id_syntax stx) ctx) Σ)
   stx
   (where syntax (resolve id_syntax Σ))]
  [(parse (Stx (List stx_fun stx_arg ...) ctx) Σ)
   (App (parse stx_fun Σ)
        (parse stx_arg Σ) ...)]
  [(parse id Σ)
   (Var (resolve id Σ))])

;; ----------------------------------------
;; Expand-time environment operations:

(define-metafunction L
  lookup : env nam -> all-transform
  [(lookup ((nam all-transform) any_2 ...) nam) all-transform]
  [(lookup (any_1 any_2 ...) nam) (lookup (any_2 ...) nam)]
  [(lookup () nam) nam])

(define-metafunction L
  extend : env nam all-transform -> env
  [(extend env nam all-transform) ((nam all-transform) . env)])

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

(define-metafunction L
  alloc-name : id Σ -> (values nam Σ)
  [(alloc-name (Stx (Sym nam) ctx) (Sto number (binds ...) boxes def-envs))
   (values ,(string->symbol (format "~a:~a"
                                    (term nam)
                                    (term number)))
           (Sto ,(add1 (term number )) (binds ...) boxes def-envs))])

(define-metafunction L
  alloc-scope : id Σ -> (values nam Σ)
  [(alloc-scope (Stx (Sym nam) ctx) (Sto number any boxes def-envs))
   (values ,(string->symbol (format "~a:~a"
                                    (term nam)
                                    (term number)))
           (Sto ,(add1 (term number)) any boxes def-envs))])

;; ----------------------------------------
;; The expander:

(define-metafunction L
  expand : stx env Σ -> (values stx Σ)

  ;; lambda
  [(expand (Stx (List id_lam id_arg stx_body) ctx) env Σ)
   (values (Stx (List id_lam id_new stx_body2) ctx) Σ_4)
   (where lambda (resolve id_lam Σ))
   (where (values nam_new Σ_1) (alloc-name id_arg Σ))
   (where (values scp_new Σ_2) (alloc-scope id_arg Σ_1))
   (where id_new (add id_arg scp_new))
   (where Σ_3 (bind Σ_2 id_new nam_new))
   (where env_new (extend env nam_new (TVar id_new)))
   (where (values stx_body2 Σ_4) (expand (add stx_body scp_new) env_new Σ_3))]

  ;; quote
  [(expand (Stx (List id_quote stx) ctx) env Σ)
   (values (Stx (List id_quote stx) ctx) Σ)
   (where quote (resolve id_quote Σ))]
  
  ;; syntax
  [(expand (Stx (List id_syntax stx) ctx) env Σ)
   (values (Stx (List id_syntax stx) ctx) Σ)
   (where syntax (resolve id_syntax Σ))]
  
  ;; macro creation
  [(expand (Stx (List id_ls id stx_rhs stx_b) ctx) env Σ)
   (expand stx_b2 env_2 Σ_3)
   (where let-syntax (resolve id_ls Σ))
   (where (values nam_new Σ_1) (alloc-name id Σ))
   (where (values scp_new Σ_2) (alloc-scope id Σ_1))
   (where id_new (add id scp_new))
   (where Σ_3 (bind Σ_2 id_new nam_new))
   (where env_2 (extend env nam_new (eval (parse stx_rhs Σ_3))))
   (where stx_b2 (add stx_b scp_new))]

  ;; macro invocation
  [(expand stx_macapp env Σ)
   (expand (flip stx_exp scp_i) env Σ_2)
   (where (Stx (List id_mac stx_arg ...) ctx) stx_macapp)
   (where val (lookup env (resolve id_mac Σ)))
   (where (values scp_u Σ_1) (alloc-scope (Stx (Sym a) (Set)) Σ))
   (where (values scp_i Σ_2) (alloc-scope (Stx (Sym a) (Set)) Σ_1))
   (where stx_exp (eval (App val (flip (add stx_macapp scp_u) scp_i))))]
  
  ;; application
  [(expand (Stx (List stx_fun stx_arg ...) ctx) env Σ)
   (values (Stx (List stx_fun2 stx_arg2 ...) ctx) Σ_1)
   (where (values (stx_fun2 stx_arg2 ...) Σ_1)
          (expall () (stx_fun stx_arg ...) env Σ))]
  
  ;; reference
  [(expand id env Σ)
   (values id_new Σ)
   (where (TVar id_new) (lookup env (resolve id Σ)))])

(define-metafunction L
  expall : (stx ...) (stx ...) env Σ -> (values (stx ...) Σ)
  [(expall (stx_e ...) () env Σ) (values (stx_e ...) Σ)]
  [(expall (stx_e ...) (stx_0 stx_1 ...) env Σ)
   (expall (stx_e ... stx_e0) (stx_1 ...) env Σ_1)
   (where (values stx_e0 Σ_1) (expand stx_0 env Σ))])

;; ----------------------------------------
;; Helpers for writing examples:

(define-metafunction L
  primitives-env : -> env
  [(primitives-env) ()])

(define-metafunction L
  init-store : -> Σ
  [(init-store) (Sto 0 () () ())])

;; ----------------------------------------
;; Examples:

(define-metafunction L
  parse/values : (values stx Σ) -> ast
  [(parse/values (values stx Σ)) (parse stx Σ)])

(define-example-definer define-example
  L 
  (lambda (t) (term (expand ,t (primitives-env) (init-store))))
  parse/values
  (Set))

(define-syntax-rule (define-core-examples
                      define-example
                      simple-macro-example
                      reftrans-macro-example z:rt
                      hyg-macro-example      z:h
                      thunk-example          a:t a:t2
                      get-identity-example   a:gi a:gi2)
  (begin
    (define-example simple-macro-example
      (let-syntax x (lambda z (syntax (quote 2))) (x 1))
      2)

    (define-example reftrans-macro-example
      (lambda z (let-syntax x (lambda s (syntax z)) (lambda z (x))))
      (Fun (Var z:0) (Fun (Var z:rt) (Var z:0))))

    (define-example hyg-macro-example
      (lambda z (let-syntax x (lambda s 
                           ('MKS
                            ('LIST (syntax lambda)
                                   (syntax z)
                                   ('CAR ('CDR ('SE s))))
                            (syntax here)))
                       (x z)))
      (Fun (Var z:0) (Fun (Var z:h) (Var z:0))))
    
    (define-example thunk-example
      (let-syntax thunk (lambda e
                          ('MKS
                           ('LIST (syntax lambda) 
                                  (syntax a) 
                                  ('CAR ('CDR ('SE e))))
                           e))
                  (((lambda a (thunk ('+ a '1))) '5) '0))
      (App (App (Fun (Var a:t) (Fun (Var a:t2) (App + (Var a:t) 1))) 5) 0))

    (define-example get-identity-example
      (let-syntax get-identity (lambda e
                                 ('MKS
                                  ('LIST (syntax lambda) 
                                         (syntax a)
                                         ('MKS
                                          ('LIST (syntax lambda) 
                                                 ('CAR ('CDR ('SE e)))
                                                 (syntax a))
                                          e))
                                  e))
                  (get-identity a))
      (Fun (Var a:gi) (Fun (Var a:gi2) (Var a:gi))))))

(define-core-examples
  define-example
  simple-macro-example
  reftrans-macro-example z:4
  hyg-macro-example      z:6
  thunk-example          a:2 a:6
  get-identity-example   a:4 a:6)

;; ----------------------------------------

(module+ pict
  (require "rewrites.rkt"
           redex/pict
           pict
           "config.rkt")
  (provide (all-defined-out))
  
  (define base-nts '(ast var val
                     stx id
                     atom
                     sym
                     nam))
  (define eval-language-pict
    (WR (language->pict L #:nts base-nts)))
  (define eval-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict eval #:contract? #t))))

  (define prim-nts '(prim))
  (define prim-language-pict
    (WR (language->pict L #:nts prim-nts)))
  (define δ-pict
    (parameterize ([compact-metafunction #t])
      (WR (metafunction->pict δ/stx))))

  (define parse-pict
    (WR (metafunction->pict parse #:contract? #t)))
  (define resolve-nts '(scps ctx
                        Σ
                        scp))
  (define resolve-language-pict
    (WR (language->pict L #:nts resolve-nts)))
  (define resolve-pict
    (vl-append
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (blank 0 (metafunction-gap-space)))
     (WR
      (parameterize ([where-combine (lambda (l r) r)]
                     [metafunction-cases '(0)])
        (metafunction->pict biggest-subset #:contract? #t)))))

  (define (make-expand-pict pos [contract? #f] #:narrower? [narrower? #f])
    (parameterize ([metafunction-cases (list pos)]
                   [linebreaks (append
                                (if contract? '(#f) '())
                                (list (and narrow-mode? narrower?)))])
      (WR (metafunction->pict expand #:contract? contract?))))
  
  (define expand-quote-pict (make-expand-pict 1 #t))
  (define expand-syntax-pict (make-expand-pict 2))
  (define expand-lambda-pict (make-expand-pict 0 #:narrower? #t))
  (define expand-var-pict (make-expand-pict 6))
  (define expand-let-syntax-pict (make-expand-pict 3))
  (define expand-macro-app-pict (make-expand-pict 4))
  (define expand-app-pict
    (vl-append
     (make-expand-pict 5 #:narrower? #t)
     (WR (blank 0 (metafunction-gap-space)))
     (WR (metafunction->pict expall #:contract? #t))))
  
  (define expand-nts '(env transform))
  (define expand-language-pict
    (WR (language->pict L #:nts expand-nts)))

  (define add+flip-pict
    (vl-append
     12
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t)))))

  (define-syntax-rule (tm e)
    (to-pict (to-lw e)))

  (define (to-pict lw)
    (WR/inline (lw->pict L lw)))
  
  (define all-nts (append base-nts
                          prim-nts
                          resolve-nts
                          expand-nts)))

#;
(module+ main
  (require "viewer.rkt"
           (submod ".." pict))
  (view eval-language-pict
        (hc-append
         40
         (vl-append
          expand-language-pict
          expand-app-pict)
         parse-pict)
        resolve-pict
        add+flip-pict))

;; Providing this file to `scribble` will render the model.
;; Set the `SCOPE_SETS_TO_PDF` environment variable to get
;; the right scale for PDF output.
(module+ doc
  (require "doc.rkt"
           "rewrites.rkt"
           redex/pict
           (submod ".." pict))
  (provide doc)
  (define doc
    (make-model-doc
     "Single-Phase"
     (WR (language->pict L #:nts all-nts))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict eval #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict δ/stx)))
     (WR (metafunction->pict parse #:contract? #t))
     (WR (metafunction->pict resolve #:contract? #t))
     (WR (parameterize ([where-combine (lambda (l r) r)]
                        [metafunction-cases '(0)])
           (metafunction->pict biggest-subset #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict strip #:contract? #t)))
     (WR (metafunction->pict expand #:contract? #t))
     (WR (metafunction->pict expall #:contract? #t))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict add #:contract? #t)))
     (parameterize ([compact-metafunction #t])
       (WR (metafunction->pict flip #:contract? #t))))))
