;; PL Project - Fall 2019
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool (e) #:transparent) ;; a boolean constants, e,g., (bool #t)

(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus (e1 e2) #:transparent) ;; minus two expressions
(struct mult (e1 e2) #:transparent) ;; multipy two expressions
(struct div (e1 e2) #:transparent) ;; divide two expressions
(struct neg (e) #:transparent) ;; negative a expression

(struct andalso (e1 e2) #:transparent) ;; logical andalso conjunction two expressions
(struct orelse (e1 e2) #:transparent) ;; logical or conjunction expressions

(struct cnd (e1 e2 e3) #:transparent) ;; condition if e1 evaluate e2 else e3
(struct iseq (e1 e2) #:transparent) ;; comparison two expressions
(struct ifnzero (e1 e2 e3) #:transparent) ;; condition if e1 is not zero evaluate e2 else e3
(struct ifleq (e1 e2 e3 e4) #:transparent) ;; condition if e1 < e2 evaluate e3 else e4

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application

(struct with (s e1 e2) #:transparent) ;; e1 is bound to s in e2

(struct apair (e1 e2) #:transparent) ;; pair constructor
(struct 1st (e) #:transparent) ;; the first part of a pair
(struct 2nd (e) #:transparent) ;; the second part of a pair

(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent)

(struct letrec (s1 e1 s2 e2 e3) #:transparent) ;; a letrec expression for recursive definitions

(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

;; Problem 1

(define (racketlist->numexlist racketList)
  (cond [(null? racketList) (munit)]
        [(list? racketList) (apair (car racketList) (racketlist->numexlist (cdr racketList)))]
        [#t (error ("it's not a racket list"))]
  )
)

(define (numexlist->racketlist numexList)
  (cond [(munit? numexList) null]
        [(apair? numexList) (cons (apair-e1 numexList) (numexlist->racketlist (apair-e2 numexList)))]
        [#t (error ("it's not a numex list"))]
  )
)

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)])
  (cond [(equal? str (car (car env))) (cdr (car env))]
        [else (envlookup (cdr env) str)]
  )
)

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
          (envlookup env (var-string e))]
      
        ;; arithmetic operations
        [(num? e)
          (cond [(integer? (num-int e)) e]
                [else (error "NUMEX num applied to non racket integer")])]
      
        [(plus? e)
          (let ([v1 (eval-under-env (plus-e1 e) env)]
                [v2 (eval-under-env (plus-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (num (+ (num-int v1) 
                      (num-int v2)))
              (error "NUMEX addition applied to non-number")))]
      
        [(minus? e)
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (num (- (num-int v1)
                      (num-int v2)))
              (error "NUMEX minus applied to non-numbers")))]
     
        [(mult? e)
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (num (* (num-int v1)
                      (num-int v2)))
              (error "NUMEX multiply applied to non-numbers")))]
      
        [(div? e)
          (let ([v1 (eval-under-env (div-e1 e) env)]
                [v2 (eval-under-env (div-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (if (equal? (num? v2)
                          0)
                (error "NUMEX divide applied to zero")
                (num (quotient (num-int v1)
                               (num-int v2))))
              (error "NUMEX divide applied to non-numbers")))]
     
        ;; logical operations
        [(bool? e)
          (cond [(boolean? (bool-e e)) e]
                [else (error "NUMEX bool applied to non racket boolean")])]
       
        [(andalso? e)
          (let ([v1 (eval-under-env (andalso-e1 e) env)])
            (if (bool? v1)
              (if (equal? (bool-e v1) #f) 
                  (bool #f) 
                  (let ([v2 (eval-under-env (andalso-e2 e) env)])
                    (if (bool? v2)
                      (if (equal? (bool-e v2) #f)
                        (bool #f)
                        (bool #t))
                      (error "NUMEX orelse applied to non-booleans"))))
              (error "NUMEX orelse applied to non-booleans")))]
      
        [(orelse? e)
          (let ([v1 (eval-under-env (orelse-e1 e) env)])
            (if (bool? v1)
              (if (equal? (bool-e v1) #t)
                (bool #t) 
                (let ([v2 (eval-under-env (orelse-e2 e) env)])
                  (if (bool? v2)
                    (if (equal? (bool-e v2) #t)
                      (bool #t)
                      (bool #f))
                    (error "NUMEX orelse applied to non-booleans"))))
              (error "NUMEX orelse applied to non-booleans")))]
      
        ;; negation
        [(neg? e)
          (let ([v (eval-under-env (neg-e e) env)])
            (if (or (num? v)
                    (bool? v))
              (if (num? v)
                (num (- 0 (num-int v)))
                (if (equal? (bool-e v) #t)
                  (bool #f)
                  (bool #t)))
              (error "NUMEX negation applied to non-number or non-boolean")))]
      
        ;; condition
        [(cnd? e)
          (let ([v (eval-under-env (cnd-e1 e) env)])
            (if (bool? v)
              (if (equal? (bool-e v) #t)
                (eval-under-env (cnd-e2 e) env)
                (eval-under-env (cnd-e3 e) env))
              (error "NUMEX cnd applied to non-boolean")))]
     
        ;; is equal
        [(iseq? e)
          (let ([v1 (eval-under-env (iseq-e1 e) env)]
                [v2 (eval-under-env (iseq-e2 e) env)])
            (if (and (or (num? v1) (bool? v1))
                     (or (num? v2) (bool? v2)))
              (if (equal? v1 v2)
                (bool #t)
                (bool #f))
              (error "NUMEX iseq applied to non-number or non-boolean")))]
      
        ;; not zero
        [(ifnzero? e)
          (let ([v (eval-under-env (ifnzero-e1 e) env)])
            (if (num? v)
              (if (equal? (num-int v) 0)
                (eval-under-env (ifnzero-e3 e) env)
                (eval-under-env (ifnzero-e2 e) env))
              (error "NUMEX isnzero applied to non-number")))]
      
        ;; less or equal
        [(ifleq? e)
          (let ([v1 (eval-under-env (ifleq-e1 e) env)]
                [v2 (eval-under-env (ifleq-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (if (> (num-int v1) 
                      (num-int v2))
                (eval-under-env (ifleq-e4 e) env)
                (eval-under-env (ifleq-e3 e) env))
              (error "NUMEX ifleq applied to non-number")))]
       
        ;; lam
        [(lam? e)
          (if (and (or (string? (lam-nameopt e))
                       (null? (lam-nameopt e)))
                   (string? (lam-formal e)))
            (closure env e)
            (error "NUMEX lam name and parameter name must be string"))]
      
        ;; apply
        [(apply? e)
          (let ([v (eval-under-env (apply-actual e) env)]
                [funexp (eval-under-env (apply-funexp e) env)])
            (if (closure? funexp)
              (let ([clsrFun (closure-f funexp)])
                (if (null? (lam-nameopt clsrFun))
                  (eval-under-env (lam-body clsrFun) (cons (cons (lam-formal clsrFun) v) (closure-env funexp)))
                  (eval-under-env (lam-body clsrFun) (cons (cons (lam-nameopt clsrFun) funexp) (cons (cons (lam-formal clsrFun) v) (closure-env funexp))))))
              (if (lam? funexp)
                (eval-under-env (apply funexp (apply-actual e)) env)
                (error "NUMEX apply applied to non-lam"))))]
       
        ;; with
        [(with? e)
          (let ([v (eval-under-env (with-e1 e) env)])
            (if (string? (with-s e))
              (eval-under-env (with-e2 e) (cons (cons (with-s e) v) env))
              (error "NUMEX key applied to not-string variable")))]
       
        ;; pair
        [(apair? e)
          (let ([v1 (eval-under-env (apair-e1 e) env)]
                [v2 (eval-under-env (apair-e2 e) env)])
            (apair v1 v2))]
       
        ;; 1st of pair
        [(1st? e)
          (let ([v (eval-under-env (1st-e e) env)])
            (if (apair? v)
              (apair-e1 v)
              (error "NUMEX 1st applied to non-apair")))]
        
        ;; 2nd of pair
        [(2nd? e)
          (let ([v (eval-under-env (2nd-e e) env)])
            (if (apair? v)
              (apair-e2 v)
              (error "NUMEX 2nd applied to non-apair")))]
       
        ;; munit
        [(munit? e) e]
      
        ;; is munit
        [(ismunit? e)
          (let ([v (eval-under-env (ismunit-e e) env)])
            (if (munit? v)
              (bool #t)
              (bool #f)))]
       
        ;; closure
        [(closure? e) e]
      
        ;; let recursive
        [(letrec? e)
            (if (and (string? (letrec-s1 e))
                     (string? (letrec-s2 e)))
              (eval-under-env (letrec-e3 e) (append (list (cons (letrec-s1 e) (letrec-e1 e)) (cons (letrec-s2 e) (letrec-e2 e))) env))
              (error "NUMEX letrec names must be string"))]
       
        ;; key
        [(key? e)
          (let ([v (eval-under-env (key-e e) env)])
            (if (string? (key-s e))
              (key (key-s e) v)
              (error "NUMEX key name must be string")))]
        
        ;; record
        [(record? e)
          (let ([v1 (eval-under-env (record-k e) env)]
                [v2 (eval-under-env (record-r e) env)])
            (if (and (key? v1)
                     (munit? v2))
              (record v1 v2)
              (if (and (key? v1)
                       (record? v2))
                (record v1 v2)
                (error "NUMEX record name must be key and applied to munit or record"))))]
       
        ;; value
        [(value? e)
          (let ([v (eval-under-env (value-r e) env)])
            (if (and (string? (value-s e))
                     (record? v))
              (if (munit? (record-r v))
                  (if (equal? (key-s (record-k v))
                              (value-s e))
                    (key-e (record-k v))
                    (munit))
                  (if (equal? (key-s (record-k v))
                              (value-s e))
                        (key-e (record-k v))
                        (eval-under-env (value (value-s e) (record-r v)) env)))
              (error "NUMEX value name must be string and applied to record")))]

        [#t (error (format "bad NUMEX expression: ~v" e))]
  )
)

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null)
)
        
;; Problem 3

;; a
(define (ifmunit e1 e2 e3)
  (cnd (ismunit  e1) e2 e3)
)

;; b
(define (with* lst e2)
  (if (equal? lst null) e2
    (with (car (car lst)) (cdr (car lst)) (with* (cdr lst) e2))
  )
)

;; c
(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4 e3)
)

;; Problem 4

;; a
(define numex-filter
  (lam null "filterFunction"
    (lam "numexFilter" "list"
      (cnd (ismunit (var "list"))
        (munit)
        (ifnzero (apply (var "filterFunction") (1st (var "list")))
          (apair (apply (var "filterFunction") (1st (var "list")))
                 (apply (var "numexFilter") (2nd (var "list"))))
          (apply (var "numexFilter") (2nd (var "list"))))
      )
    )
  )
)

;; b
(define numex-all-gt
  (lam null "i"
    (lam "filterGt" "list"
      (apply
        (apply numex-filter(lam null "x" (ifleq (var "x") (var "i") (num 0) (var "x"))))
        (var "list")
      )
    )
  )
)

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (car (compute-free-vars-handler e))
)

;; return a cons of computed 'e and free vars of 'e
(define (compute-free-vars-handler e)
  (cond [(var? e) 
          (cons e (set (var-string e)))]
        
        ;; arithmetic operations
        [(num? e)
          (cons e (set))]
      
        [(plus? e)
         (let ([v1 (compute-free-vars-handler (plus-e1 e))]
               [v2 (compute-free-vars-handler (plus-e2 e))])
           (cons (plus (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
      
        [(minus? e)
         (let ([v1 (compute-free-vars-handler (minus-e1 e))]
               [v2 (compute-free-vars-handler (minus-e2 e))])
           (cons (minus (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
      
        [(mult? e)
         (let ([v1 (compute-free-vars-handler (mult-e1 e))]
               [v2 (compute-free-vars-handler (mult-e2 e))])
           (cons (mult (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
       
        [(div? e)
         (let ([v1 (compute-free-vars-handler (div-e1 e))]
               [v2 (compute-free-vars-handler (div-e2 e))])
           (cons (div (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
    
        ;; logical operations
        [(bool? e)
          (cons e (set))]
       
        [(andalso? e)
        (let ([v1 (compute-free-vars-handler (andalso-e1 e))]
              [v2 (compute-free-vars-handler (andalso-e2 e))])
           (cons (andalso (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
       
        [(orelse? e)
        (let ([v1 (compute-free-vars-handler (orelse-e1 e))]
              [v2 (compute-free-vars-handler (orelse-e2 e))])
           (cons (orelse (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        
        ;; negation
        [(neg? e)
         (let ([v (compute-free-vars-handler (neg-e e))])
           (cons (neg (car v)) (cdr v)))]

        ;; condition
        [(cnd? e)
        (let ([v1 (compute-free-vars-handler (cnd-e1 e))]
              [v2 (compute-free-vars-handler (cnd-e2 e))]
              [v3 (compute-free-vars-handler (cnd-e3 e))])
           (cons (cnd (car v1) (car v2) (car v3)) (set-union (cdr v1) (set-union (cdr v2) (cdr v3)))))]
      
        ;; is equal
        [(iseq? e)
        (let ([v1 (compute-free-vars-handler (iseq-e1 e))]
              [v2 (compute-free-vars-handler (iseq-e2 e))])
           (cons (iseq (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        
        ;; not zero
        [(ifnzero? e)
         (let ([v1 (compute-free-vars-handler (ifnzero-e1 e))]
               [v2 (compute-free-vars-handler (ifnzero-e2 e))]
               [v3 (compute-free-vars-handler (ifnzero-e3 e))])
           (cons (ifnzero (car v1) (car v2) (car v3)) (set-union (cdr v1) (cdr v2) (cdr v3))))]
        
        ;; less or equal
        [(ifleq? e)
          (let ([v1 (compute-free-vars-handler (ifleq-e1 e))]
                [v2 (compute-free-vars-handler (ifleq-e2 e))]
                [v3 (compute-free-vars-handler (ifleq-e3 e))]
                [v4 (compute-free-vars-handler (ifleq-e4 e))])
           (cons (ifleq (car v1) (car v2) (car v3) (car v4)) (set-union (cdr v1) (cdr v2) (cdr v3) (cdr v4))))]
        
        ;; lam
        [(lam? e)
          (let ([cfvf (compute-free-vars-handler (lam-body e))])
            (let ([free-var-set (set-remove (set-remove (cdr cfvf) (lam-formal e)) (lam-nameopt e))])
               (cons (fun-challenge (lam-nameopt e) (lam-formal e) (car cfvf) free-var-set) free-var-set)))]
      
        ;; apply
        [(apply? e)
         (let ([va (compute-free-vars-handler (apply-actual e))]
               [vf (compute-free-vars-handler (apply-funexp e))])
           (cons (apply (car vf) (car va)) (set-union (cdr vf) (cdr va))))]

        ;; with
        [(with? e)
         (let ([v1 (compute-free-vars-handler (with-e1 e))]
               [v2 (compute-free-vars-handler (with-e2 e))])
            (cons (with (with-s e) (car v1) (car v2)) (set-union (set-remove (cdr v2) (with-s e)) (cdr v1))))]
        
        ;; pair
        [(apair? e)
         (let ([v1 (compute-free-vars-handler (apair-e1 e))]
               [v2 (compute-free-vars-handler (apair-e2 e))])
           (cons (apair (car v1) (car v2)) (set-union (cdr v1) (cdr v2))))]
        
        ;; 1st of pair
        [(1st? e)
         (let ([v (compute-free-vars-handler (1st-e e))])
           (cons (1st (car v)) (cdr v)))]

        ;; 2nd of pair
        [(2nd? e)
          (let ([v (compute-free-vars-handler (2nd-e e))])
           (cons (2nd (car v)) (cdr v)))]
       
        ;; munit
        [(munit? e)
          (cons e (set))]

        ;; is munit
        [(ismunit? e)
          (let ([v (compute-free-vars-handler (ismunit-e e))])
           (cons (ismunit (car v)) (cdr v)))]

        ;; closure
        [(closure? e)
          (cons e (set))]
        
        ;; let recursive
        [(letrec? e)
         (let ([v1 (compute-free-vars-handler (letrec-e1 e))]
               [v2 (compute-free-vars-handler (letrec-e2 e))]
               [v3 (compute-free-vars-handler (letrec-e3 e))])
           (cons (letrec (car v1) (car v2) (car v3)) (set-union (cdr v1) (set-union (cdr v2) (cdr v3)))))]
       
        ;; key
        [(key? e)
          (let ([v (compute-free-vars-handler (key-e e))])
           (cons (key (car v)) (cdr v)))]
        
        ;; record
        [(record? e)
          (let ([v (compute-free-vars-handler (record-r e))])
           (cons (record (car v)) (cdr v)))]

        ;; value
        [(value? e)
          (let ([v (compute-free-vars-handler (value-r e))])
           (cons (value (car v)) (cdr v)))]
        
        [#t (error (format "bad NUMEX expression: ~v" e))]
))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) 
  (cond [(var? e)
          (envlookup env (var-string e))]
      
        ;; arithmetic operations
        [(num? e)
          (cond [(integer? (num-int e)) e]
                [else (error "NUMEX num applied to non racket integer")])]
      
        [(plus? e)
          (let ([v1 (eval-under-env-c (plus-e1 e) env)]
                [v2 (eval-under-env-c (plus-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (num (+ (num-int v1) 
                      (num-int v2)))
              (error "NUMEX addition applied to non-number")))]
      
        [(minus? e)
         (let ([v1 (eval-under-env-c (minus-e1 e) env)]
               [v2 (eval-under-env-c (minus-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (num (- (num-int v1)
                      (num-int v2)))
              (error "NUMEX minus applied to non-numbers")))]
     
        [(mult? e)
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (num (* (num-int v1)
                      (num-int v2)))
              (error "NUMEX multiply applied to non-numbers")))]
      
        [(div? e)
          (let ([v1 (eval-under-env-c (div-e1 e) env)]
                [v2 (eval-under-env-c (div-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (if (equal? (num? v2)
                          0)
                (error "NUMEX divide applied to zero")
                (num (quotient (num-int v1)
                               (num-int v2))))
              (error "NUMEX divide applied to non-numbers")))]
     
        ;; logical operations
        [(bool? e)
          (cond [(boolean? (bool-e e)) e]
                [else (error "NUMEX bool applied to non racket boolean")])]
       
        [(andalso? e)
          (let ([v1 (eval-under-env-c (andalso-e1 e) env)])
            (if (bool? v1)
              (if (equal? (bool-e v1) #f) 
                  (bool #f) 
                  (let ([v2 (eval-under-env-c (andalso-e2 e) env)])
                    (if (bool? v2)
                      (if (equal? (bool-e v2) #f)
                        (bool #f)
                        (bool #t))
                      (error "NUMEX orelse applied to non-booleans"))))
              (error "NUMEX orelse applied to non-booleans")))]
      
        [(orelse? e)
          (let ([v1 (eval-under-env-c (orelse-e1 e) env)])
            (if (bool? v1)
              (if (equal? (bool-e v1) #t)
                (bool #t) 
                (let ([v2 (eval-under-env-c (orelse-e2 e) env)])
                  (if (bool? v2)
                    (if (equal? (bool-e v2) #t)
                      (bool #t)
                      (bool #f))
                    (error "NUMEX orelse applied to non-booleans"))))
              (error "NUMEX orelse applied to non-booleans")))]
      
        ;; negation
        [(neg? e)
          (let ([v (eval-under-env-c (neg-e e) env)])
            (if (or (num? v)
                    (bool? v))
              (if (num? v)
                (num (- 0 (num-int v)))
                (if (equal? (bool-e v) #t)
                  (bool #f)
                  (bool #t)))
              (error "NUMEX negation applied to non-number or non-boolean")))]
      
        ;; condition
        [(cnd? e)
          (let ([v (eval-under-env-c (cnd-e1 e) env)])
            (if (bool? v)
              (if (equal? (bool-e v) #t)
                (eval-under-env-c (cnd-e2 e) env)
                (eval-under-env-c (cnd-e3 e) env))
              (error "NUMEX cnd applied to non-boolean")))]
     
        ;; is equal
        [(iseq? e)
          (let ([v1 (eval-under-env-c (iseq-e1 e) env)]
                [v2 (eval-under-env-c (iseq-e2 e) env)])
            (if (and (or (num? v1) (bool? v1))
                     (or (num? v2) (bool? v2)))
              (if (equal? v1 v2)
                (bool #t)
                (bool #f))
              (error "NUMEX iseq applied to non-number or non-boolean")))]
      
        ;; not zero
        [(ifnzero? e)
          (let ([v (eval-under-env-c (ifnzero-e1 e) env)])
            (if (num? v)
              (if (equal? (num-int v) 0)
                (eval-under-env-c (ifnzero-e3 e) env)
                (eval-under-env-c (ifnzero-e2 e) env))
              (error "NUMEX isnzero applied to non-number")))]
      
        ;; less or equal
        [(ifleq? e)
          (let ([v1 (eval-under-env-c (ifleq-e1 e) env)]
                [v2 (eval-under-env-c (ifleq-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
              (if (> (num-int v1) 
                      (num-int v2))
                (eval-under-env-c (ifleq-e4 e) env)
                (eval-under-env-c (ifleq-e3 e) env))
              (error "NUMEX ifleq applied to non-number")))]
       
        ;; lam
        [(lam? e)
          (if (and (or (string? (lam-nameopt e))
                       (null? (lam-nameopt e)))
                   (string? (lam-formal e)))
            (closure env e)
            (error "NUMEX lam name and parameter name must be string"))]
      
        ;; apply
        [(apply? e)
          (let ([v (eval-under-env-c (apply-actual e) env)]
                [funexp (eval-under-env-c (apply-funexp e) env)])
            (if (closure? funexp)
              (let ([clsrFun (closure-f funexp)])
                (if (null? (lam-nameopt clsrFun))
                  (eval-under-env-c (lam-body clsrFun) (cons (cons (lam-formal clsrFun) v) (closure-env funexp)))
                  (eval-under-env-c (lam-body clsrFun) (cons (cons (lam-nameopt clsrFun) funexp) (cons (cons (lam-formal clsrFun) v) (closure-env funexp))))))
              (if (lam? funexp)
                (eval-under-env-c (apply funexp (apply-actual e)) env)
                (error "NUMEX apply applied to non-lam"))))]
       
        ;; with
        [(with? e)
          (let ([v (eval-under-env-c (with-e1 e) env)])
            (if (string? (with-s e))
              (eval-under-env-c (with-e2 e) (cons (cons (with-s e) v) env))
              (error "NUMEX key applied to not-string variable")))]
       
        ;; pair
        [(apair? e)
          (let ([v1 (eval-under-env-c (apair-e1 e) env)]
                [v2 (eval-under-env-c (apair-e2 e) env)])
            (apair v1 v2))]
       
        ;; 1st and 2nd
        [(1st? e)
          (let ([v (eval-under-env-c (1st-e e) env)])
            (if (apair? v)
              (apair-e1 v)
              (error "NUMEX 1st applied to non-apair")))]
        
        [(2nd? e)
          (let ([v (eval-under-env-c (2nd-e e) env)])
            (if (apair? v)
              (apair-e2 v)
              (error "NUMEX 2nd applied to non-apair")))]
       
        ;; munit
        [(munit? e) e]
      
        ;; is munit
        [(ismunit? e)
          (let ([v (eval-under-env-c (ismunit-e e) env)])
            (if (munit? v)
              (bool #t)
              (bool #f)))]
       
        ;; closure
        [(closure? e) e]
      
        ;; let recursive
        [(letrec? e)
            (if (and (string? (letrec-s1 e))
                     (string? (letrec-s2 e)))
              (eval-under-env-c (letrec-e3 e) (append (list (cons (letrec-s1 e) (letrec-e1 e)) (cons (letrec-s2 e) (letrec-e2 e))) env))
              (error "NUMEX letrec names must be string"))]
       
        ;; key
        [(key? e)
          (let ([v (eval-under-env-c (key-e e) env)])
            (if (string? (key-s e))
              (key (key-s e) v)
              (error "NUMEX key name must be string")))]
        
        ;; record
        [(record? e)
          (let ([v1 (eval-under-env-c (record-k e) env)]
                [v2 (eval-under-env-c (record-r e) env)])
            (if (and (key? v1)
                     (munit? v2))
              (record v1 v2)
              (if (and (key? v1)
                       (record? v2))
                (record v1 v2)
                (error "NUMEX record name must be key and applied to munit or record"))))]
       
        ;; value
        [(value? e)
          (let ([v (eval-under-env-c (value-r e) env)])
            (if (and (string? (value-s e))
                     (record? v))
              (if (munit? (record-r v))
                  (if (equal? (key-s (record-k v))
                              (value-s e))
                    (key-e (record-k v))
                    (munit))
                  (if (equal? (key-s (record-k v))
                              (value-s e))
                        (key-e (record-k v))
                        (eval-under-env-c (value (value-s e) (record-r v)) env)))
              (error "NUMEX value name must be string and applied to record")))]
        
        ;; fun-challenge
        [(fun-challenge? e)
         (let ([nameopt (fun-challenge-nameopt e)]
               [formal (fun-challenge-formal e)]
               [freevars (fun-challenge-freevars e)])
         (if (and (or (string? nameopt) (null? nameopt)) (string? formal))
             (closure (commons env freevars)  e)
             (error "NUMEX function name and parameter name must be string")))]

        [#t (error (format "bad NUMEX expression: ~v" e))]
))

(define (commons env set)
  (if (equal? env null) null
      (if (set-member? set (car (car env)))
          (cons (car env) (commons (cdr env) set))
          (commons (cdr env) set)))
)

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null)
)
