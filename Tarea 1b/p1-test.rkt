#lang play
(require "p1.rkt")
(require "env.rkt")

(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;
;; test interprete ;;
;;;;;;;;;;;;;;;;;;;;;

;; test numero
(test (interp (parse-expr 1) empty-env '()) (numV 1))
(test (interp (parse-expr -341) empty-env '()) (numV -341))
(test (interp (parse-expr 0) empty-env '()) (numV 0))


;; test boolean
(test (interp (parse-expr #f) empty-env '()) (boolV #f))
(test (interp (parse-expr #t) empty-env '()) (boolV #t))

;; test pair
(test
 (interp (parse-expr '(cons #t 2)) empty-env '())
 (pairV (boolV #t) (numV 2)))

(test
 (interp (parse-expr '(cons 5 (cons #t 2))) empty-env '())
 (pairV (numV 5) (pairV (boolV #t) (numV 2))))


;; test id
(test
 (interp (id 'x) (extend-env 'x (numV 10) empty-env) '())
 (numV 10))
(test
 (interp (id 'x) (extend-env 'x (boolV #t) empty-env) '())
 (boolV #t))



;; test add 1
(test
 (interp (parse-expr '(add1 2)) empty-env '())
 (numV 3))
(test/exn
 (interp (parse-expr '(add1 (cons #t 2))) empty-env '())
 "Runtime type error: expected Number found")

;; test suma
(test
 (interp (parse-expr '(+ 1 5)) empty-env '())
 (numV 6))
(test
 (interp (parse-expr '(+ 1 x)) (extend-env 'x (numV 10) empty-env) '())
 (numV 11))
(test/exn
 (interp (parse-expr '(+ #t 5)) empty-env '())
 "Runtime type error: expected Number found")


;; test resta
(test
 (interp (parse-expr '(- 10 5)) empty-env '())
 (numV 5))

(test
 (interp (parse-expr '(- x 6)) (extend-env 'x (numV 10) empty-env) '())
 (numV 4))

(test/exn
 (interp (parse-expr '(- 10 (cons 1 2))) empty-env '())
 "Runtime type error: expected Number found")

;; test menor
(test
 (interp (parse-expr '(< 10 5)) empty-env '())
 (boolV #f))

(test
 (interp (parse-expr '(< 10 10)) empty-env '())
 (boolV #f))

(test
 (interp (parse-expr '(< 10 15)) empty-env '())
 (boolV #t))


;; test igual
(test
 (interp (parse-expr '(= 10 5)) empty-env '())
 (boolV #f))

(test
 (interp (parse-expr '(= 10 10)) empty-env '())
 (boolV #t))

(test
 (interp (parse-expr '(= 10 15)) empty-env '())
 (boolV #f))


;; test not equal
(test
 (interp (parse-expr '(!(= 10 15))) empty-env '())
 (boolV #t))

(test
 (interp (parse-expr '(!(< 10 15))) empty-env '())
 (boolV #f))


;; test or
(test
 (interp (parse-expr '(|| #f (= 10 15))) empty-env '())
 (boolV #f))
(test
 (interp (parse-expr '(|| #t (= 10 15))) empty-env '())
 (boolV #t))

;; test and
(test
 (interp (parse-expr '(&& #f (< 10 15))) empty-env '())
 (boolV #f))
(test
 (interp (parse-expr '(&& #t (< 10 15))) empty-env '())
 (boolV #t))

;; test first
(test
 (interp (parse-expr '(fst (cons #t 2))) empty-env '())
 (boolV #t))

(test
 (interp (parse-expr '(fst (cons 5 (cons #t 2)))) empty-env '())
 (numV 5) )

;; test second
(test
 (interp (parse-expr '(snd (cons #t 2))) empty-env '())
 (numV 2))

(test
 (interp (parse-expr '(snd (cons 5 (cons #t 2)))) empty-env '())
 (pairV (boolV #t) (numV 2)))

;; test if
(test
 (interp (parse-expr '(if #t (+ 1 1) 3)) empty-env '())
 (numV 2))

(test
 (interp (parse-expr '(if #f 2 (- 5 2))) empty-env '())
 (numV 3))

;; test with
(test
 (interp (parse-expr '(with ((x (add1 1))) (+ x 2))) empty-env '())
 (numV 4))

(test
 (interp (parse-expr '(with ((x 2) (y 5)) (+ x y))) empty-env '())
 (numV 7))
         
;; para testear integraciones más complejas del interprete se realizara directamente en run


;;;;;;;;;;;;;;
;; test run ;;
;;;;;;;;;;;;;;



;; test numero
(test (run '{5}) (numV 5))
(test (run '{-5}) (numV -5))
(test (run '{0}) (numV 0))

;; test boolean
(test (run '{#t}) (boolV #t))
(test (run '{#f}) (boolV #f))

;; test pair
(test (run '((cons 1 2))) (pairV (numV 1) (numV 2)))

;; test add1
(test (run '((add1 1))) (numV 2))
(test/exn (run '((add1 #t))) "Runtime type error: expected Number found")

;; test suma
(test (run '((+ 1 2))) (numV 3))
(test/exn (run '((+ 1 #t))) "Runtime type error: expected Number found")
(test/exn (run '((+ (cons 1 2) 2))) "Runtime type error: expected Number found")

;; test resta
(test (run '((- (+ 1 2) 2))) (numV 1))
(test/exn (run '((- 1 (cons 1 2)))) "Runtime type error: expected Number found")
(test/exn (run '((- #t 2))) "Runtime type error: expected Number found")

;; test menor
(test (run '((< 1 2))) (boolV #t))
(test (run '((< 2 2))) (boolV #f))
(test/exn (run '((< 1 #t))) "Runtime type error: expected Number found")
(test/exn (run '((< (cons 1 2) 2))) "Runtime type error: expected Number found")

;; test igual
(test (run '((= 1 2))) (boolV #f))
(test (run '((= 2 2))) (boolV #t))
(test/exn (run '((= 1 (cons 1 2)))) "Runtime type error: expected Number found")
(test/exn (run '((= #t 2))) "Runtime type error: expected Number found")

;; test not equal
(test (run '((! #f))) (boolV #t))
(test (run '((! #t))) (boolV #f))
(test/exn (run '((! 2))) "Runtime type error: expected Boolean found")

;; test or
(test (run '((|| #f #t))) (boolV #t))
(test (run '((|| (= 1 2) (< 1 2)))) (boolV #t))
(test/exn (run '((|| #f (cons 1 2)))) "Runtime type error: expected Boolean found")
(test/exn (run '((|| 2 #t))) "Runtime type error: expected Boolean found")

;; test and
(test (run '((&& #f #t))) (boolV #f))
(test (run '((&& (= 1 2) (< 1 2)))) (boolV #f))
(test/exn (run '((&& #f (cons 1 2)))) "Runtime type error: expected Boolean found")
(test/exn (run '((&& 2 #t))) "Runtime type error: expected Boolean found")

;; test first
(test (run '((fst(cons 1 2)))) (numV 1))
(test/exn (run '((fst #t))) "Runtime type error: expected Pair found")

;; test second
(test (run '((snd(cons 1 2)))) (numV 2))
(test/exn (run '((snd 1))) "Runtime type error: expected Pair found")

;; test if
(test (run '((if #t 1 2))) (numV 1))
(test (run '((if #f 1 2))) (numV 2))
(test/exn (run '((if 5 1 2))) "Runtime type error: expected Boolean found")

;; test fundef
(test (run '{
  {define {bar p} {! p}}
  {bar #t}
}) (boolV #f))
(test/exn (run '{{foo 2 3}}) "Undefined function: ")
(test/exn (run '{
  {define {bar p} {! p}}
  {bar #t #f}
}) "Arity mismatch: function")

;; Programa de Ejemplo 1
(test (run '{
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      (numV 13))

;; Programa de Ejemplo 2
(test (run '{
             {with {{x 5} {y 23} {z {cons 11 -3}}}
                   z}
             })
      (pairV (numV 11) (numV -3)))

;; Programa de Ejemplo 3
(test (run '{
             {define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             })
      (numV 8))

;; Programa de Ejemplo 4
(test (run '{
             {with {{x 3}
                    {y {+ 1 2}}}
                   {if {= x y} x y}}
             })
      (numV 3))



