#lang play
(require "p1.rkt")

(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))

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

