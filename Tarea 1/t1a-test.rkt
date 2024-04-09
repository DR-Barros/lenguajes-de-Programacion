#lang play

(require "t1a.rkt")
(print-only-errors)

#|    TESTS    |#

;;;;;;;;;;;;;;;;
;; parse-expr ;;
;;;;;;;;;;;;;;;;

; Basic types
(test (parse-expr '5) (num 5))
(test (parse-expr '-9) (num -9))
(test (parse-expr 'x) (id 'x))
(test (parse-expr '#f) (bool #f))
(test (parse-expr '#t) (bool #t))

; unary number primitives
(test (parse-expr '{add1 3}) (add1 (num 3)))
(test (parse-expr '{add1 -15}) (add1 (num -15)))
(test (parse-expr '{add1 y}) (add1 (id 'y)))
(test (parse-expr '{add1 {add1 3}}) (add1 (add1 (num 3))))


; binary number primitives
(test (parse-expr '{+ x -2})
      (add (id 'x) (num -2)))
(test (parse-expr '{- 3 z})
      (sub (num 3) (id 'z)))
(test (parse-expr '{+ {add1 3} {- z 2}})
      (add (add1 (num 3)) (sub (id 'z) (num 2))))
(test (parse-expr '{add1 {- var {+ var 2}}})
      (add1 (sub (id 'var) (add (id 'var) (num 2)))))


; booleans simple operator
(test (parse-expr '{! #t}) (dist (bool #t)))
(test (parse-expr '{< 5 6}) (minor (num 5) (num 6)))
(test (parse-expr '{<= 3 6}) (minor-equal (num 3) (num 6)))
(test (parse-expr '{> 5 9}) (mayor (num 5) (num 9)))
(test (parse-expr '{>= 2 6}) (mayor-equal (num 2) (num 6)))
(test (parse-expr '{= 3 3}) (equal (num 3) (num 3)))

; conditional
(test (parse-expr '{if #t 3 4}) (if (bool #t) (num 3) (num 4)))
(test (parse-expr '{if #f 3 4}) (if (bool #f) (num 3) (num 4)))
(test (parse-expr '{if {<= 3 6} 3 4}) (if  (minor-equal (num 3) (num 6)) (num 3) (num 4)))


; with
(test (parse-expr '{with {{x 1}} {+ x 1}})
      (with (list (binding (id 'x) (num 1))) (add (id 'x) (num 1))))

(test (parse-expr '{with {{x 5} {y 42} {z #t}} {if z {add1 x} {- y 2}}})
      (with
       (list (binding (id 'x) (num 5)) (binding (id 'y) (num 42)) (binding (id 'z) (bool #t)))
       (if (id 'z) (add1 (id 'x)) (sub (id 'y) (num 2)))))
      

; Function applications with zero and more arguments.
(test (parse-expr '{f})
      (app 'f '()))
(test (parse-expr '{foo {bar 5}})
      (app 'foo (list (app 'bar (list (num 5))))))
(test (parse-expr '{add1 {zam {+ 1 x} (- 2 y)}})
      (add1 (app 'zam (list (add (num 1) (id 'x)) (sub (num 2) (id 'y))))))

; An expression that combines everything.
(test (parse-expr '{+ {- {fib 4} {add1 x}} {add1 {fact 5}}})
      (add (sub (app 'fib (list (num 4))) (add1 (id 'x))) (add1 (app 'fact (list (num 5))))))


;

;;;;;;;;;;;;;;;;;;
;; parse-fundef ;;
;;;;;;;;;;;;;;;;;;

; Function with no parameters.
(test (parse-fundef '{define {five} {+ 2 3}})
      (fundef 'five '() (add (num 2) (num 3))))

; Functions with one or more parameters.
(test (parse-fundef '{define {add2 n} {add1 {add1 n}}})
      (fundef 'add2 '(n) (add1 (add1 (id 'n)))))
(test (parse-fundef '{define {double x} {+ x x}})
      (fundef 'double '(x) (add (id 'x) (id 'x))))
(test (parse-fundef '{define {sum a b c d} {+ a {+ b {+ c  d}}}})
      (fundef 'sum '(a b c d) (add (id 'a) (add (id 'b) (add (id 'c) (id 'd))))))

;;;;;;;;;;;;;;;;
;; parse-prog ;;
;;;;;;;;;;;;;;;;

; Programs with no function definitions.
(test
 (parse-prog '{2}) (prog '() (num 2)))
(test (parse-prog '{y}) (prog '() (id 'y)))
(test
 (parse-prog '{
                {+ 3 {- 5 -4}}
              })
 (prog '() (add (num 3) (sub (num 5) (num -4)))))

; Programs with fundefs without parameters
(test
 (parse-prog '{
               {define {val} 4}
               {+ 3 {val}}
             }
             )
 (prog
  (list (fundef 'val '() (num 4)))
  (add (num 3) (app 'val '()))))

(test
 (parse-prog '{
               {define {x} 4}
               {define {y} 8}
               {define {z} 6}
               {- {+ {x} {y}} {z}}
             })
 (prog
  (list (fundef 'x '() (num 4))
        (fundef 'y '() (num 8))
        (fundef 'z '() (num 6)))
  (sub (add (app 'x '()) (app 'y '())) (app 'z '()))))

; Programs with fundef with parameters
(test
 (parse-prog '{
                 {define {sum x y z} {+ x {+ y z}}}
                 {define {add2 x} {+ {add1 x} 1}}
                 {- {sum 2 -4 1} {add2 3}}
              })
(prog
 (list (fundef 'sum '(x y z) (add (id 'x) (add (id 'y) (id 'z))))
       (fundef 'add2 '(x) (add (add1 (id 'x)) (num 1))))
 (sub (app 'sum (list (num 2) (num -4) (num 1))) (app 'add2 (list (num 3))))))




;;;;;;;;;;;;;;;;;;;
;; parse-binding ;;
;;;;;;;;;;;;;;;;;;;



;bindigs with simple expresions
(test (parse-binding '(x 9)) (binding (id 'x) (num 9)))
(test (parse-binding '(x (+ 1 3))) (binding (id 'x) (add (num 1) (num 3))))



