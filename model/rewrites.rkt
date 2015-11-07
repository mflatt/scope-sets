#lang scheme
(require redex/reduction-semantics
         redex/pict
         slideshow/pict
         scheme/list
         scheme/gui/base
         scheme/class
         "config.rkt")

(provide WR WR/inline
         with-rewrites
         ->ps 
         grammar+reductions
         append-steps
         step-...
         explain
         together
         prim ast trans langle rangle
         compact-metafunction)

(define-syntax-rule (WR e)
  (with-rewrites (lambda () (scale e s))))
(define-syntax-rule (WR/inline e)
  (with-rewrites (lambda () (scale e s/inline))))

(define (underline p)
  (refocus (vc-append -2 
                      p 
                      (linewidth 0.5 (hline (pict-width p) 0)))
           p))

(define (prim s) (text s '(bold . modern) (default-font-size)))
(define (ast s) (text s '(bold caps . roman) (default-font-size)))
(define (trans s) (text s '(caps . roman) (default-font-size)))
(define (lit s) (text s (literal-style) (default-font-size)))
(define (mf s) (text s (metafunction-style) (metafunction-font-size)))
(define (atag s) (trans s))

(define transform (text "transform" (non-terminal-style) (default-font-size)))

(define (between s a b)
  (build-lw s
            (+ (lw-line a) (lw-line-span a))
            0
            (+ (lw-column a) (lw-column-span a))
            (max 0 (- (lw-column b)
                      (+ (lw-column a) (lw-column-span a))))))

(define (re-lw new-e lw)
  (build-lw new-e
            (lw-line lw) (lw-line-span lw)
            (lw-column lw) (lw-column-span lw)))

(define (refit orig new)
  (append (list (between "" (just-before "" (car orig)) (car new)))
          new
          (list (between "" (just-after "" (last new)) (just-after "" (last orig))))))

(define (<-) (let-values ([(h w d a) (send (dc-for-text-size) get-text-extent "m")])
               (dc (lambda (dc x y)
                     (let ([p (send dc get-pen)])
                       (send dc set-pen (send p get-color) 0.5 'solid)
                       (let ([m (+ y (/ h 2))]
                             [x (+ 2 x)]
                             [w (- w 4)])
                         (send dc draw-line x m (+ x w) m)
                         (send dc draw-line x m (+ x (/ w 4)) (- m (/ w 4)))
                         (send dc draw-line x m (+ x (/ w 4)) (+ m (/ w 4))))
                       (send dc set-pen p)))
                   w h (- h d) d)))
(define (=>) (let-values ([(h w d a) (send (dc-for-text-size) get-text-extent "m")])
               (dc (lambda (dc x y)
                     (let ([p (send dc get-pen)])
                       (send dc set-pen (send p get-color) 0.5 'solid)
                       (let ([m (+ y (/ h 2))]
                             [x (- (+ x w) 2)]
                             [w (- w 4)])
                         (send dc draw-line x m (- x w) m)
                         (send dc draw-line x m (- x (/ w 4)) (- m (/ w 4)))
                         (send dc draw-line x m (- x (/ w 4)) (+ m (/ w 4))))
                       (send dc set-pen p)))
                   w h (- h d) d)))

(define (hat p #:delta [delta 0])
  (refocus (ct-superimpose (inset p 0 2 0 0)
                           (inset
                            (dc (lambda (dc x y)
                                  (define p (send dc get-pen))
                                  (define s (send dc get-smoothing))
                                  (send dc set-smoothing 'smoothed)
                                  (send dc set-pen (make-pen #:width 0.75
                                                             #:join 'miter
                                                             #:cap 'projecting))
                                  (send dc draw-lines (list (cons 0 1.5)
                                                            (cons 2.5 0.5)
                                                            (cons 5 1.5))
                                        x y)
                                  (send dc set-pen p)
                                  (send dc set-smoothing s))
                                5 1.5)
                            0 (* delta (pict-height p)) 0 0))
           p))

(define (overline p #:delta [delta 0])
  (refocus (ct-superimpose (inset p 0 2 0 0)
                           (inset (linewidth 0.75 (hline (pict-width p) 1))
                                  0 (* delta (pict-height p)) 0 0))
           p))

(define (fader w h)
  (define FADE-W 40)
  (dc (lambda (dc x y)
        (define p (send dc get-pen))
        (define b (send dc get-brush))
        (send dc set-pen (make-pen #:style 'transparent))
        (send dc set-brush (make-brush #:gradient (make-object linear-gradient%
                                                               (+ x w) y
                                                               (+ x w FADE-W) y
                                                               (list (list 0 (make-color 210 210 210))
                                                                     (list 1 (make-color 255 255 255))))))
        (send dc draw-rectangle x y (+ w FADE-W) h)
        (send dc set-pen p)
        (send dc set-brush b))
      (+ w FADE-W) h))

(define (make-subst-rewrite pre <- post)
  (lambda (lws)
    (refit
     lws
     (list (list-ref lws 2)
           (between pre
                    (list-ref lws 2)
                    (list-ref lws 3))
           (list-ref lws 3)
           (between (<-)
                    (list-ref lws 3)
                    (list-ref lws 4))
           (list-ref lws 4)
           (just-after post (list-ref lws 4))))))

(define subst-rewrite (make-subst-rewrite "[" <- "]"))
(define extend-rewrite (make-subst-rewrite "+{" => "}"))

(define (make-subst*-rewrite pre <- post1 post2)
  (lambda (lws)
    (let* ([subs (lw-e (list-ref lws 3))]
           [subs2 (lw-e (list-ref subs 1))])
      (refit
       lws
       (list (list-ref lws 2)
             (between pre
                      (list-ref lws 2)
                      (list-ref lws 3))
             (re-lw "" (list-ref subs 0))
             (re-lw "" (list-ref subs2 0))
             (list-ref subs2 1)
             (between (=>)
                      (list-ref subs2 1)
                      (list-ref subs2 2))
             (list-ref subs2 2)
             (re-lw post1 (list-ref subs2 3))
             (list-ref subs 2)
             (re-lw "" (list-ref subs 3))
             (just-after post2 (list-ref lws 3)))))))

(define subst*-rewrite (make-subst*-rewrite "[" <- "" "]"))
(define extend*-rewrite (make-subst*-rewrite "+{" <- "}" ""))
(define store*-rewrite (make-subst*-rewrite "{" <- "" "}"))

(define (seq-at lw . cs)
  (build-lw (map (lambda (c) (just-before c lw)) cs)
            (lw-line lw) 0
            (lw-column lw) 0))

(define (fix-subscript p)
  (let ([p (symbol->string p)])
    (hbl-append (text (substring p 0 1) (non-terminal-style) (default-font-size))
                (text (substring p 2) (non-terminal-subscript-style) (default-font-size)))))

(define (store-rewrite lws)
  (let* ([re-sig (seq-at (list-ref lws 4)
                         (fix-subscript (lw-e (list-ref lws 2)))
                         "("
                         (lw-e (list-ref lws 3))
                         ")")]
         [sub (extend-rewrite (list re-sig ; fake "("
                                    re-sig ; fake "subst"
                                    re-sig
                                    (list-ref lws 4)
                                    (list-ref lws 5)))])
    (extend-rewrite (list (list-ref lws 0)
                          (list-ref lws 1)
                          (list-ref lws 2)
                          (list-ref lws 3)
                          (build-lw sub
                                    (lw-line (car sub)) 0
                                    (lw-column (car sub))
                                    (- (+ (lw-column (last sub))
                                          (lw-column-span (last sub)))
                                       (lw-column (car sub))))))))

(define (alloc-rewrite lws)
  (extend-rewrite (list (list-ref lws 0)
                        (list-ref lws 1)
                        (list-ref lws 2)
                        (list-ref lws 3)
                        (just-after "∅" (list-ref lws 3))
                        (list-ref lws 4))))

(define (taller s)
  (define p (text s (default-style) (default-font-size)))
  (define h (pict-height p))
  ;; Ad hoc spacing adjustments (to match original build):
  (inset (drop-below-ascent (scale (launder p) 1 1.25) (* 0.05 h))
         0 (* -0.25 h) 0 0))

(define (langle) (taller "\u2329"))
(define (rangle) (taller "\u232A"))

(define (angle-brackets who lws)
  (if (= (length lws) 3)
      (list (re-lw (langle) (car lws))
            (re-lw (atag who) (cadr lws))
            (re-lw (rangle) (caddr lws)))
      (refit
       lws
       (let* ([open (re-lw (langle) (car lws))]
              [prev-tag (cadr lws)]
              [lws (drop (drop-right lws 1) 2)])
         (append (if who
                     (list open
                           (between "" open prev-tag)
                           (re-lw (atag who) prev-tag))
                     (list open (between "" open (car lws))))
                 (if who
                     lws
                     (append
                      (list (car lws)
                            (just-after "," (car lws))
                            (cadr lws))
                      (if (null? (cddr lws))
                          null
                          (list (just-after "," (cadr lws))
                                (caddr lws)))))
                 (list (just-after (rangle) (last lws))))))))

#;
(define (symbol lws)
  (refit
   lws
   (let ([e (caddr lws)]
         [/ (text "/" 'roman (default-font-size))])
     (list (just-before / e)
           e
           (just-after / e)))))

(define (symbol lws)
  (refit
   lws
   (let ([e (caddr lws)]
         [quot (text "\u2019" 'roman (default-font-size))])
     (list (just-before quot e)
           e))))

(define (binary sep lws)
  (refit 
   lws
   (list
    (list-ref lws 2)
    (just-after sep (list-ref lws 2))
    (between "" (list-ref lws 2) (list-ref lws 3))
    (list-ref lws 3))))

(define (delta name lws)
  (refit lws
         (list* (re-lw name (list-ref lws 1))
                (between "(" (list-ref lws 1) (list-ref lws 2))
                (list-ref lws 2)
                (between ", " (list-ref lws 2) (cadr (lw-e (list-ref lws 3))))
                (comma-ize (cdr (lw-e (list-ref lws 3)))))))

(define (constructor name)
  (lambda (lws)
    (refit lws
           (list* (re-lw name (list-ref lws 1))
                  (between "(" (list-ref lws 1) (list-ref lws 2))
                  (comma-ize (cddr lws))))))

(define (trans-constructor name)
  (constructor name))

(define (comma-ize lws [has-paren? #t])
  (let loop ([lws lws])
    (cond
     [(or (null? (cdr lws))
          (and has-paren? (null? (cddr lws))))
      lws]
     [else
      (list*
       (car lws)
       (between ", " (car lws) (cadr lws))
       (loop (cdr lws)))])))
  
(define bm-dc #f)

(define (name)
  (text "name" (non-terminal-style) (default-font-size)))
(define (var)
  (text "var" (non-terminal-style) (default-font-size)))

(define (rng lws)
  (refit lws
         (list (re-lw "rng" (list-ref lws 1))
               (between "(" (list-ref lws 1) (list-ref lws 2))
               (list-ref lws 2)
               (just-after ")" (list-ref lws 2)))))

(define (set-rewrite lws)
  (cond
   [(= (length lws) 3)
    (refit
     lws
     (list (re-lw "∅" (list-ref lws 0))))]
   [else
    (refit
     lws
     (let ([lws (refit lws (let loop ([lws (drop-right (cddr lws) 1)])
                             (cond
                              [(null? (cdr lws)) lws]
                              [else (list* (car lws)
                                           (between ", " (car lws) (cadr lws))
                                           (loop (cdr lws)))])))])
       (append (list (just-before "{" (car lws)))
               lws
               (list (just-after "}" (last lws))))))]))

(define (seq-rewrite lws)
  (refit lws (let loop ([lws (drop-right (cddr lws) 1)])
               (cond
                [(null? (cdr lws)) lws]
                [else (list* (car lws)
                             (between ", " (car lws) (cadr lws))
                             (loop (cdr lws)))]))))


(define (bind-rewrite lws)
  (refit lws
         (let ([lws (refit lws (drop-right (cddr lws) 1))])
           (list (cadr lws)
                 (<-)
                 (caddr lws)))))

(define (lookup-rewrite lws)
  (refit
   lws
   (list
    (list-ref lws 2)
    (between "(" (list-ref lws 2) (list-ref lws 3))
    (list-ref lws 3)
    (just-after ")" (list-ref lws 3)))))

(define (hide-first-argument lws)
  (refit lws
         (append
          (list (re-lw (text (symbol->string (lw-e (list-ref lws 1)))
                             (metafunction-style)
                             (metafunction-font-size))
                       (list-ref lws 1))
                (between ((white-square-bracket) #t) (list-ref lws 1) (list-ref lws 3)))
          (comma-ize (drop-right (drop lws 3) 1) #f)
          (list (just-after ((white-square-bracket) #f) (list-ref lws (- (length lws) 2)))))))

(define ((make-phase-indexed #:name [name #f]) lws)
  (define len (length lws))
  (define ph? (let ([e (lw-e (list-ref lws 2))])
                ;; Special-case "ph" or "ph+1" as a first argument
                (or (eq? 'ph e)
                    (and (list? e)
                         (= 5 (length e))
                         (eq? 'plus (lw-e (list-ref e 1)))
                         (eq? 'ph (lw-e (list-ref e 2)))
                         (equal? "1" (lw-e (list-ref e 3)))))))
  (refit lws
         (append
          (list (re-lw (let ([f (text (symbol->string (or name (lw-e (list-ref lws 1))))
                                      (metafunction-style)
                                      (metafunction-font-size))])
                         (if ph?
                             (let ([g
                                    (hbl-append
                                     f
                                     (text "ph" (non-terminal-subscript-style) (default-font-size)))])
                               (if (eq? 'ph (lw-e (list-ref lws 2)))
                                   g
                                   (hbl-append
                                    g
                                    (text "+1" `(large-script subscript . ,(default-style)) (default-font-size)))))
                             f))
                       (list-ref lws 1))
                (between ((white-square-bracket) #t) (list-ref lws 1) (list-ref lws (if ph? 3 2))))
          (comma-ize (drop-right (drop lws (if ph? 3 2)) 1) #f)
          (list (just-after ((white-square-bracket) #f) (list-ref lws (- len 2)))))))

(define phase-indexed (make-phase-indexed))

(define (scps) (overline (text "scp" (non-terminal-style) (default-font-size))
                         #:delta 0.2))

(define (bullet) (text "•" 'default (default-font-size)))

(define compact-metafunction (make-parameter #f))

(define (with-rewrites thunk)
  (parameterize ([literal-style 'modern]
                 [metafunction-font-size 12]
                 [label-font-size 10]
                 [metafunction-pict-style 'left-right/compact-side-conditions]
                 [non-terminal-subscript-style (cons 'large-script
                                                     (non-terminal-subscript-style))]
                 [metafunction-gap-space 12]
                 [metafunction-rule-gap-space (if (compact-metafunction) 0 6)]
                 [metafunction-line-gap-space 0]
                 [metafunction-fill-acceptable-width (if narrow-mode?
                                                         400
                                                         0)]
                 [current-render-pict-adjust (lambda (p mode)
                                               (case mode
                                                 [(metafunction-line side-condition-line language-line)
                                                  ;; Angle brackets are the tallest; make sure every line
                                                  ;; matches an angle-brackets line for consistent spacing:
                                                  (define adj (langle))
                                                  (define min-ascent 15)
                                                  (define min-descent (pict-descent adj))
                                                  (define top-delta (max 0 (- min-ascent (pict-ascent p))))
                                                  (define bottom-delta (max 0 (- min-descent (pict-descent p))))
                                                  (if (and (zero? top-delta)
                                                           (zero? bottom-delta))
                                                      p
                                                      (inset p 0 top-delta 0 bottom-delta))]
                                                 [else p]))]
                 [metafunction-combine-contract-and-rules (lambda (c mf)
                                                            (vl-append
                                                             6
                                                             (vl-append
                                                              c
                                                              (linewidth 0.5 (hline (pict-width c) 0 #:segment 1)))
                                                             mf))]
                 [where-make-prefix-pict (lambda ()
                                           ((current-text) " subject to " (default-style) (default-font-size)))]
                 [where-combine (lambda (l r)
                                  (define =-pict ((current-text) " = " (default-style) (default-font-size)))
                                  (hbl-append r =-pict l))])
    (with-atomic-rewriter*
     (lambda ()
       (with-compound-rewriter*
        (lambda ()
          (with-unquote-rewriter
           (lambda (lw)
             (cond
              [(and (string? (lw-e lw))
                    (string->number (lw-e lw)))
               (re-lw (lw-e lw) lw)]
              [(and (pair? (lw-e lw))
                    (pair? (cdr (lw-e lw)))
                    (lw? (cadr (lw-e lw)))
                    (eq? (lw-e (cadr (lw-e lw))) '+))
               (define a (list-ref (lw-e lw) 2))
               (define b (list-ref (lw-e lw) 3))
               (re-lw (refit (lw-e lw)
                             (list a
                                   (between "+" a b)
                                   b))
                      lw)]
              [(and (pair? (lw-e lw))
                    (pair? (cdr (lw-e lw)))
                    (lw? (cadr (lw-e lw)))
                    (eq? (lw-e (cadr  (lw-e lw))) 'map))
               (re-lw
                (let ([env (let ([arg (lw-e (last (lw-e (list-ref (lw-e lw) (- (length (lw-e lw)) 2)))))]
                                 [env (text "\u03BE" (default-style) (default-font-size))])
                             (case arg
                               [(env) env]
                               [(env_defs)
                                (hbl-append env
                                            (text "defs" (non-terminal-subscript-style) (default-font-size)))]
                               [else (error 'rewrite "unknown map target: ~s" arg)]))])
                  (htl-append (text "{" 'roman (default-font-size))
                              (var)
                              (=>)
                              (text "unstop" (metafunction-style) (metafunction-font-size))
                              ((white-square-bracket) #t)
                              env
                              (text "(" 'roman (default-font-size))
                              (var)
                              (text ")" 'roman (default-font-size))
                              ((white-square-bracket) #f)
                              (text " | " 'roman (default-font-size))
                              (var)
                              (text " ∈ dom(" 'roman (default-font-size))
                              env
                              (text ")}" 'roman (default-font-size))))
                lw)]
              [(and (pair? (lw-e lw))
                    (pair? (cdr (lw-e lw)))
                    (lw? (cadr (lw-e lw)))
                    (eq? (lw-e (cadr  (lw-e lw))) 'filter))
               (re-lw
                (htl-append (text "{" 'roman (default-font-size))
                            (var)
                            (=>)
                            transform
                            (text " | " 'roman (default-font-size))
                            (vl-append
                             (hbl-append
                              (text "\u03BE" (default-style) (default-font-size))
                              (text "(" 'roman (default-font-size))
                              (var)
                              (text ") = " 'roman (default-font-size))
                              transform)
                             (hb-append
                              (text " and " 'roman (default-font-size))
                              transform
                              (text " ≠ " 'roman (default-font-size))
                              (trans "Stop")
                              (text "}" 'roman (default-font-size)))))
                lw)]
              [(and (pair? (lw-e lw))
                    (pair? (cdr (lw-e lw)))
                    (lw? (cadr (lw-e lw)))
                    (eq? (lw-e (cadr  (lw-e lw))) 'let*))
               ;; biggest-subset
               (define (s sub)
                 (hbl-append (scps)
                             (text sub (non-terminal-subscript-style) (default-font-size))))
               (define subset (text " ⊆ " 'roman (default-font-size)))
               (re-lw (vl-append
                       ((current-render-pict-adjust)
                        (htl-append (s "biggest") subset (s "ref")
                                    (text ", " 'roman (default-font-size))
                                    (s "biggest")
                                    (text " \u2208 {" 'roman (default-font-size))
                                    (s "bind")
                                    (text ", ...}," 'roman (default-font-size)))
                        'side-condition-line)
                       ((current-render-pict-adjust)
                        (htl-append (s "bind") subset (s "ref") (text "  ⇒  " 'roman (default-font-size))
                                    (s "bind") subset (s "biggest"))
                        'side-condition-line))
                      lw)]
              [else lw]))
           (thunk)))
        (list
         'mf (lambda (lws)
               (refit lws
                      (list (re-lw (text (symbol->string (lw-e (caddr lws)))
                                         (metafunction-style)
                                         (metafunction-font-size))
                                   (caddr lws)))))
         'plus (lambda (lws) (binary "+" lws))
         'minus (lambda (lws) (binary "\u2212" lws))
         'subtract (lambda (lws) (binary " ∖ " lws))
         'union (lambda (lws) (binary " ∪ " lws))
         'and? (lambda (lws) (binary " and " lws))
         'same? (lambda (lws) (binary " = " lws))
         'is-in? (lambda (lws) (binary
                                ;; Ad hoc spacing adjustment (to match original build):
                                (inset (text " \u2208 " 'roman (default-font-size)) 0 -3 0 0)
                                lws))
         'binds? (lambda (lws) (list (list-ref lws 2) 
                                (just-after "  binds" (list-ref lws 2))
                                (list-ref lws 3)))
         'elem (lambda (lws) (refit lws (drop-right (drop lws 2) 1)))
         'emptyset (lambda (lws) (refit lws (list (re-lw "∅" (list-ref lws 0)))))
         'add-elem (lambda (lws)
                     (refit lws
                            (list (just-before "{" (list-ref lws 2))
                                  (list-ref lws 2)
                                  (just-after "}∪" (list-ref lws 2))
                                  (between "" (list-ref lws 2) (list-ref lws 3))
                                  (list-ref lws 3))))                                  
         'lookup (lambda (lws) 
                   (refit
                    lws
                    (append
                     (list (list-ref lws 2) 
                           (between "(" (list-ref lws 2) (list-ref lws 3))
                           (list-ref lws 3)
                           (just-after ")" (list-ref lws 3)))
                     (if ((length lws) . > . 5)
                         ;; Has store
                         (list (just-after (text "Σ" `(large-script subscript . roman) (default-font-size)) (list-ref lws 3)))
                         null))))
         'Fst (lambda (lws) 
                (refit lws (list (caddr lws))))
         
         'expand phase-indexed
         'expand* phase-indexed
         'resolve phase-indexed
         'flip phase-indexed
         'add phase-indexed
         'prune phase-indexed
         'parse phase-indexed
         'eval phase-indexed
         'subtract-at (make-phase-indexed #:name 'subtract)
         
         'addremove (lambda (lws) (binary " ⊕ " lws))
         'extend extend-rewrite
         'extend* extend*-rewrite
         'update-ctx extend-rewrite
         'def-env-update extend-rewrite
         'bind (lambda (lws)
                 (if (eq? 'ph (lw-e (list-ref lws 2)))
                     (extend-rewrite (list*
                                      (car lws)
                                      (cadr lws)
                                      (cdddr lws)))
                     (extend-rewrite lws)))
         'subst subst-rewrite
         'store store-rewrite
         'alloc alloc-rewrite
         'store-lookup lookup-rewrite
         'binding-lookup lookup-rewrite
         'def-env-lookup lookup-rewrite
         'at-phase lookup-rewrite
         
         'store-val (lambda (lws)
                      (store*-rewrite (list* (car lws)
                                             (cadr lws)
                                             (just-after "" (cadr lws))
                                             (cddr lws))))
         'rng rng
         'primitives-env (lambda (lws)
                           (refit lws 
                                  (list (re-lw 'env_primitives (car lws)))))
         'Xalloc-name (lambda (lws)
                       (refit lws 
                              (list (re-lw "fresh" (car lws)))))
         'alloc-scope hide-first-argument
         'alloc-name hide-first-argument
         'Tup (lambda (lws) (angle-brackets #f lws))
         'St (lambda (lws) (angle-brackets #f lws))
         'StE (lambda (lws) (angle-brackets #f lws))
         'values (lambda (lws) (angle-brackets #f lws))
         'Sym (lambda (lws) (symbol lws))
         'Exp (lambda (lws) (angle-brackets "Exp" lws))
         'Eval (lambda (lws) (angle-brackets "Eval" lws))
         'Local-Exp (lambda (lws) (angle-brackets "Local" lws))
         'Def-Exp (lambda (lws) (angle-brackets "Defs-Exp" lws))
         'InExp (lambda (lws) (angle-brackets "InExp" lws))
         'InBind (lambda (lws) (angle-brackets "InBind" lws))
         'InIntDef (lambda (lws) (angle-brackets "InDef" lws))
         'Plain (lambda (lws) (refit lws (drop-right (cddr lws) 1)))
         'Seq seq-rewrite
         'Set set-rewrite
         'Map set-rewrite
         'Sto (lambda (lws)
                ;; Hide number that's use for generating names,
                ;; and remove a layer of parens around the rest
                (set-rewrite (list*
                              (car lws)
                              (cadr lws)
                              (append
                               (drop-right (drop (lw-e (cadddr lws)) 1) 1)
                               (list (last lws))))))
         'Bind bind-rewrite
         'StoBind bind-rewrite
         'Comma (lambda (lws) (refit 
                               lws
                               (let ([lws (drop-right (cddr lws) 1)])
                                 (append lws
                                         (list (just-after "," (last lws)))))))
         'δ (lambda (lws) (delta "δ" lws))
         'δ/stx (lambda (lws) (delta "δ" lws))
         'App (constructor (ast "App"))
         'Ref (constructor (ast "Ref"))
         'Fun (constructor (ast "Fun"))
         'Var (constructor (ast "Var"))
         'Let (constructor (ast "Let"))
         'Atom (constructor (ast "Atom"))
         'List (constructor (ast "List"))
         'Stx (constructor (ast "Stx"))
         'Mark (constructor (trans "Mark"))
         'Rename (constructor (trans "Rename"))
         'Defs (constructor (ast "Defs"))
         'TStop (constructor (trans "Stop"))
         'TVar (constructor (trans "Var")))))
      (list
       'nam name
       'scps scps
       'no-scope bullet
       '• bullet ;; tt bullet is too small
       'Σ (lambda () (text "Σ" (default-style) (default-font-size)))
       'Σ* (lambda () (hat (text "Σ" (default-style) (default-font-size))))
       'maybe-scp (lambda () (hat #:delta 0.2 (text "scp" (non-terminal-style) (default-font-size))))
       'δ (lambda () (text "δ" (default-style) (default-font-size)))
       'kont (lambda () (text "κ" 'default (default-font-size)))
       'env (lambda () (text "\u03BE" (default-style) (default-font-size)))
       '.... (lambda () (text "...." 'default (default-font-size)))
       'SPC (lambda () (text " " 'default (default-font-size)))
       'all-tprim (lambda () (text "tprim" (non-terminal-style) (default-font-size)))
       'all-transform (lambda () (text "transform" (non-terminal-style) (default-font-size)))
       'pre-ctx (lambda () (text "ctx" (non-terminal-style) (default-font-size)))
       'pre-val (lambda () (text "val" (non-terminal-style) (default-font-size)))
       'desc-other-prim (lambda () (text "...." 'roman (default-font-size)))
       'desc-other-val (lambda () (text "...." 'roman (default-font-size)))
       'desc-other-atom (lambda () (text "...." 'roman (default-font-size)))
       'desc-other-trans (lambda () (text "...." 'roman (default-font-size)))
       'desc-old-mixture (lambda () (text "...." 'roman (default-font-size)))
       'desc-old-F (lambda () (text "...." 'roman (default-font-size)))
       'desc-old-tprim (lambda () (text "...." 'roman (default-font-size)))
       'desc-name (lambda () (hbl-append (text "a token such as " 'roman (default-font-size))
                                         (lit "x")
                                         (text ", " 'roman (default-font-size))
                                         (lit "egg")
                                         (text ", or " 'roman (default-font-size))
                                         (lit "lambda")))
       'desc-env (lambda () (hbl-append (text "a mapping from " 'roman (default-font-size))
                                        (name)
                                        (text " to " 'roman (default-font-size))
                                        transform))
       'desc-ctx (lambda () (hbl-append (text "a mapping from " 'roman (default-font-size))
                                   (text "ph" (non-terminal-style) (default-font-size))
                                   (text " to " 'roman (default-font-size))
                                   (scps)))
       'desc-scope (lambda () (text "a token that represents a scope" 'roman (default-font-size)))
       'desc-store (lambda () (hbl-append (text "binding store, " 'roman (default-font-size))
                                     (name)
                                     (text " " 'roman (default-font-size))
                                     (=>)
                                     (text " (" 'roman (default-font-size))
                                     (scps)
                                     (text " " 'roman (default-font-size))
                                     (=>)
                                     (text " " 'roman (default-font-size))
                                     (name)
                                     (text ")" 'roman (default-font-size))))
       'desc-S (lambda () (hbl-append (text "set of " 'roman (default-font-size))
                                      (text "σ" (non-terminal-style) (default-font-size))))
       'ctx-defined-later (lambda () (text ".... (defined below)" 'roman (default-font-size)))
       'CONS (lambda () (prim "cons"))
       'CAR (lambda () (prim "car"))
       'CDR (lambda () (prim "cdr"))
       'SEL (lambda () (prim "sel"))
       'LIST (lambda () (prim "list"))
       'SE (lambda () (prim "stx-e"))
       'MKS (lambda () (prim "mk-stx"))
       'LOCAL-VALUE (lambda () (prim "lvalue"))
       'LOCAL-EXPAND (lambda () (prim "lexpand"))
       'LOCAL-BINDER (lambda () (prim "lbinder"))
       'NEW-DEFS (lambda () (prim "new-defs"))
       'DEF-BIND (lambda () (prim "def-bind"))
       '+ (lambda () (prim "+"))
       '- (lambda () (prim "-"))
       'NULL (lambda () (trans "null"))
       'TFun (lambda () (trans "Fun"))
       'TLet (lambda () (trans "Let"))
       'TQuote (lambda () (trans "Quote"))
       'TSyntax (lambda () (trans "Syntax"))
       'TLet-Syntax (lambda () (trans "Let-Syntax"))
       'TStop (lambda () (trans "Stop"))

       'Defs (lambda () (ast "Defs"))
       
       ;; Metafunctions for reference in prose:
       'resolve (lambda () (mf "resolve"))
       'biggest-subset (lambda () (mf "biggest-subset"))
       'expand (lambda () (mf "expand"))
       'expand* (lambda () (mf "expand*"))
       'parse (lambda () (mf "parse"))
       'eval (lambda () (mf "eval"))
       'add (lambda () (mf "add"))
       'flip (lambda () (mf "flip"))
       'prune (lambda () (mf "prune"))
       'unstop (lambda () (mf "unstop"))
       
       ))))
  
(define (with-atomic-rewriter* thunk l)
  (if (null? l)
      (thunk)
      (with-atomic-rewriter
       (car l) (cadr l)
       (with-atomic-rewriter* thunk (cddr l)))))

(define (with-compound-rewriter* thunk l)
  (if (null? l)
      (thunk)
      (with-compound-rewriter
       (car l) (cadr l)
       (with-compound-rewriter* thunk (cddr l)))))


(define (->ps file mk)
    (let ([pss (current-ps-setup)])
      (send pss set-mode 'file)
      (send pss set-file file)
      (let ([dc (make-object post-script-dc% #f)])
        (let ([p (parameterize ([dc-for-text-size dc]) (mk))])
          (send dc start-doc "pict")
          (send dc start-page)
          (draw-pict p dc 0 0)
          (send dc end-page)
          (send dc end-doc)))))

(define (grammar+reductions g r)
  (vc-append 10 g r))

(define (append-steps #:init [init ghost] . l)
  (let ([-> (text "=" 'roman (default-font-size))])
    (apply
     vl-append
     (map (lambda (a b)
            (htl-append 4 
                        (if (pair? b)
                            (lbl-superimpose (ghost a) (car b))
                            a)
                        (if (pair? b)
                            (cdr b)
                            b)))
          (cons (init ->) (map (lambda (x) ->) (cdr l))) 
          l))))

(define-syntax-rule (step-... e ...)
  (explain "..." e ... "..."))

(define (explain . es)
  (apply htl-append 
         (add-between
          (for/list ([e (in-list es)])
            (if (pict? e)
                e
                (text e '(italic . roman) (default-font-size))))
          (text " " '(italic . roman) (default-font-size)))))

(define (together . l)
  ;; Make all pictures the same width to help typesetting
  (let ([w (apply max (map pict-width l))])
    (apply
     values
     (map (lambda (p)
            (lt-superimpose 
             ;; A white frame ensures that the PS bounding
             ;; box is the size we want:
             (colorize (frame (blank w 0)) "white")
             p))
          l))))
