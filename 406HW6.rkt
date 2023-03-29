#lang plai

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                     HW6 written by Jake Hahne (Spring 2023)              |
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



;/--------------------------------------------------------------------------\
;                       FAE type definitions/syntax
;\--------------------------------------------------------------------------/

(define-type FAE
  [ num (n number?)]
  [ binop (op procedure?) (lhs FAE?) (rhs FAE?)]
  [ id (name symbol?)]
  [ if0 (condition FAE?) (then FAE?) (else FAE?)] 
  [ fun (args (listof symbol?)) (body FAE?)]
  [ app (fun-expr FAE?) (args (listof FAE?))])

(define-type FAE-Value
  [ numV (n number?)]
  [ closureV (params (listof symbol?))
             (body FAE?)
             (ds DefrdSub?)])

(define-type DefrdSub
  [ mtSub ]
  [ aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;;Lookup
(define (lookup name ds)
  (type-case DefrdSub ds
    [ mtSub () (error 'lookup "no binding for identifier" )]
    [ aSub (bound-name bound-value rest-ds)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-ds))]))


;/--------------------------------------------------------------------------\
;                 my definitions for binop/lookup table
;\--------------------------------------------------------------------------/

;;num-binop (replaced num+)
(define (num-binop op n1 n2)
  (cond
    [(and (numV? n1) (numV? n2))
     (numV (op (numV-n n1) (numV-n n2)))]
    [else (error "arguments must be numbers")]))

; Defining table that maps operator symbols to their definitions
(define op-table
  (list (list '+ +) (list '- -) (list '* *) (list '/ /)))

; Defining a look-up function that takes in a symbol and returns it's definition from previously defined table
(define (lookup-op symbol)
  (let ((match (assoc symbol op-table)))
    (if match
        (cadr match)
        (error "no such operator"))))


;/--------------------------------------------------------------------------\
;                                interpreter
;\--------------------------------------------------------------------------/
(define (interp expr ds)
  (type-case FAE expr
    [ num (n) ( numV n)]
    [ binop (op lhs rhs) (num-binop op (interp lhs ds) (interp rhs ds)) ]
    [ id (v) (lookup v ds)]
    [ if0 (condition then else)
          (let ([conditionV (interp condition ds)])
            (cond [(and (numV? conditionV) (= (numV-n conditionV) 0))
                   (interp then ds)]
                  [else (interp else ds)]))]
    [ fun (bound-id bound-body)
          ( closureV bound-id bound-body ds)]
    [ app (fun-expr args)
          (local ([define fun-val (interp fun-expr ds)])
            (interp (closureV-body fun-val)
                    (build-env (closureV-params fun-val)
                               (map (lambda (arg) (interp arg ds)) args)
                               (closureV-ds fun-val))))]))


;helper procedure that builds substitution list for multi-param/arg functions
(define (build-env params args ds)
  (if (null? params)
      ds
      (build-env (rest params) (rest args) (aSub (first params) (first args) ds))))
                           


;/----------------------------------------------------------------------------\
;                                   parser
;\----------------------------------------------------------------------------/
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       
       ; allows parsing of binary operations (add, subtract, etc.)
       [(+ - * /)
        (binop (lookup-op (first sexp)) (parse (second sexp)) (parse (third sexp)))]
       
       ; function parser (handles multi-parameter function calls)
       [(fun)
        (fun (second sexp) (parse (third sexp)))]

       ;'with' parsed to a function app
       [(with)
        (let* ((bindings (second sexp))
               (args (map (lambda (b) (parse (second b))) bindings))
               (params (map first bindings))
               (body (parse (third sexp))))
          (app (fun params body) args))]
       
       ; if0 parser
       [(if0)
        (if0 (parse (second sexp))
             (parse (third sexp))
             (parse (fourth sexp)))]

       ; else app
       [else
        (app (parse (first sexp)) (map parse (rest sexp)))]
       )]
    [else (error "invalid expression: " sexp)]))




;/----------------------------------------------------------------------------\
;                           functionality tests
;\----------------------------------------------------------------------------/

;---------------Test for binary operations implementation----------------;

(newline)(display "---------------------tests for binop---------------------")(newline)(newline)

(test
 (parse '{* 1 2})
 (binop * (num 1) (num 2)))

(test
 (interp (parse '{* 1 2}) (mtSub))
 (numV 2))

(test
 (parse '{* 2 {/ 3 4}})
 (binop * (num 2) (binop / (num 3) (num 4))))

(test
 (interp (parse '{* 2 {/ 3 4}}) (mtSub))
 (numV (/ 3 2)))

;-------------------Tests for multi-parameter fun update----------------;

(newline)(display "-----------------------tests for fun (multi-param)---------------------")(newline)(newline)

(test
 (parse '{fun {x y z} {+ x x}})
 (fun '(x y z) (binop + (id 'x) (id 'x))))

(test
 (interp (parse '{fun {x y z} {+ x x}}) (mtSub))
 (closureV '(x y z) (binop + (id 'x) (id 'x)) (mtSub)))


;------------------------Tests for part 3 - app-------------------------;

(newline)(display "--------------------tests for app---------------------")(newline)(newline)

(test
 (parse '{{fun {x y z} {* {+ x y} z}} 5 6 7})
 (app (fun '(x y z)
           (binop * (binop + (id 'x) (id 'y)) (id 'z)))
      (list (num 5) (num 6) (num 7))))

(test
 (interp (parse '{{fun {x y z} {* {+ x y} z}} 5 6 7}) (mtSub))
 (numV 77))

;-----------------------Tests for part 4 - with-------------------------;

(newline)(display "--------------------tests for with-----------------------")(newline)(newline)

(test
 (parse '{with {{x 5} {y 6} {z 7} } {* {+ x y} z} })
 (app (fun '(x y z) (binop * (binop + (id 'x) (id 'y)) (id 'z))) (list (num 5) (num 6) (num 7))))

(test
 (interp (parse '{with {{x 5} {y 6} {z 7} } {* {+ x y} z} }) (mtSub))
 (numV 77))

;-----------------------Tests for part 5 - if0-------------------------;

(newline)(display "--------------------tests for if0-----------------------")(newline)(newline)

(test
 (parse '(if0 0 1 2))
 (if0 (num 0) (num 1) (num 2)))

(test
 (interp (parse '(if0 0 1 2)) (mtSub))
 (numV 1))

(test
 (parse '(if0 3 1 2))
 (if0 (num 3) (num 1) (num 2)))

(test
 (interp (parse '(if0 3 1 2)) (mtSub))
 (numV 2))

(test
 (parse '{with {{n 0}}
               {if0 n
                    1
                    2}})
 (app (fun '(n) (if0 (id 'n) (num 1) (num 2))) (list (num 0))))

(test
 (interp
  (parse '{with {{n 0}}
                {if0 n
                     1
                     2}})
  [mtSub])
 (numV 1))

(test
 (parse '{{fun {w x}
               {with {{y 5} {z 6}}
                     (if0
                      {- (* w y) (* x z)}
                      {/ w y}
                      {/ x z})}}
          5 6})
 (app (fun '(w x)
           (app (fun '(y z)
                     (if0
                      (binop -
                             (binop * (id 'w) (id 'y))
                             (binop * (id 'x) (id 'z)))
                      (binop / (id 'w) (id 'y))
                      (binop / (id 'x) (id 'z))))
                (list (num 5) (num 6))))
      (list (num 5) (num 6))))

(test
 (interp (parse '{{fun {w x}
                       {with {{y 5} {z 6}}
                             (if0
                              {- (* w y) (* x z)}
                              {/ w y}
                              {/ x z})}}
                  5 6})
         (mtSub))
 (numV 1))

;------------------------other test cases------------------------------;

(newline)(display "--------------------other tests---------------------")(newline)(newline)

;; testing the interpreter on a function application (single param)
(interp (parse '{{fun {x} {+ x x}} 5}) (mtSub) )

;; sample test cases
(test
 (interp (parse '5 ) [mtSub])
 (numV 5) )

(test
 (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub) )
 (numV 10))