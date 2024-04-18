#lang play

(require "p2.rkt")

(print-only-errors #t)

;;;;;;;;;;;;;;;;;
;;  PARSE-ARG  ;;
;;;;;;;;;;;;;;;;;


;; num
(test (typecheck (parse-prog '{3})) (numT))
;; bool
(test (typecheck (parse-prog '{#t})) (boolT))
;; pair
(test (typecheck (parse-prog '{(cons 3 #t)})) (pairT (numT) (boolT)))
;; add1
(test (typecheck (parse-prog '{(add1 3)})) (numT))
(test/exn (typecheck (parse-prog '{(add1 #t)})) "")
;; add
(test (typecheck (parse-prog '{(+ 1 3)})) (numT))
(test/exn (typecheck (parse-prog '{(+ #t 3)})) "")
(test/exn (typecheck (parse-prog '{(+ 5 #t)})) "")
;; sub
(test (typecheck (parse-prog '{(- 1 3)})) (numT))
;; lt
(test (typecheck (parse-prog '{(< 1 3)})) (boolT))
;; eq
(test (typecheck (parse-prog '{(= 1 3)})) (boolT))
;; not
(test (typecheck (parse-prog '{(! #f)})) (boolT))
;; and
(test (typecheck (parse-prog '{(&& #t #f)})) (boolT))
;; or
(test (typecheck (parse-prog '{(|| #t #f)})) (boolT))
;; first
(test (typecheck (parse-prog '{(fst (cons 3 #t))})) (numT))
;; second
(test (typecheck (parse-prog '{(snd (cons 3 #t))})) (boolT))

;; with
(test (typecheck (parse-prog '{{with {{z : Num 42}}
                                         z}})) (numT))





(test (typecheck (prog '() (num 5))) (numT))
(test (typecheck (prog '() (bool #t))) (boolT))


; Programa de ejemplo 1
(test (typecheck (parse-prog '{
                               {with {{x : Num 5} {y : Num 10}}
                                     {+ x y}}
                               }))
      (numT))

; Programa de ejemplo 2
(test (typecheck (parse-prog '{
                               {with {{x 5}}
                                     {with {{y : Num {+ x 1}}}
                                           {+ x y}}
                                     }}))
      (numT))

; Programa de ejemplo 3
(test (typecheck (parse-prog '{
                               {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                                 {cons {+ {fst p} x} {+ {snd p} x}}}
                               {add-pair {cons 1 1} 1}
                               }))
      (pairT (numT) (numT)))

; Programa de ejemplo 4
(test (typecheck (parse-prog '{
                               {define {id {x : Num}} x}
                               {id 5}
                               }))
      (numT))

; Programa de ejemplo 5
(test (typecheck (parse-prog '{
                               {define {sum {x : Num} {y : Num} {z : Num}}
                                 {+ x {+ y z}}}
                               {define {cadr {x : {Pair Num {Pair Num Num}}}} : Num
                                 {fst {snd x}}}
                               {with {{x 9} {y {cons 1 {cons 3 4}}}}
                                     {sum x {fst y} {cadr y}} }
                               }))
      (numT))

(test (typecheck (parse-prog '{3})) (numT))

(test  (typecheck (parse-prog '{{define {f {p : Bool}} {if p 23 42}}
                                {f {< 3 4}}}))
       (numT))

(test/exn (typecheck (parse-prog '{{define {one {x : Num}} 1}
                                   {one #t}}))
          "Static type error: expected Num found Bool")

(test/exn (typecheck (parse-prog '{{< 10 #t}}))
          "Static type error: operator < expected Num found Bool")

(test/exn (typecheck (parse-prog '{{if 73 #t #t}}))
          "Static type error: expected Bool found Num")

(test/exn (typecheck (parse-prog '{{with {{x : Num 5} {y : Num #t} {z : Num 42}}
                                         z}}))
          "Static type error: expected Num found Bool")

