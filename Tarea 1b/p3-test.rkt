
#lang play

(require "p3.rkt")
(require "p2-test.rkt")


;; si las funciones de contrato no devuelven un booleano falla estaticamente el typechecker

(test/exn (run '{{define {sum {x : Num}} : Num {+ 1 x}}
             {define {sub {x : Num @ sum} {y : Num}} : Num
               {- y x}}
             {sub 5 3}}) "Static contract error: invalid type for sum")

;; si las funciones de contrato no reciben un solo parametro falla estaticamente el typechecker

(test/exn (run '{{define {eq {x : Num} {y : Num}} : Num {+ y x}}
             {define {sub {x : Num @ eq} {y : Num}} : Num
               {- y x}}
             {sub 5 3}}) "Static contract error: invalid type for eq")

;; si las funciones de contrato devuelven se ejecuta la función
(test (run '{{define {positive {x : Num}} : Bool {< 0 x}}
             {define {sub {x : Num @ positive} {y : Num}} : Num
               {- y x}}
             {sub 5 3}})
      (numV -2))

(test (run '{{define {positive {x : Num}} : Bool {< 0 x}}
             {define {negate {x : Num @ positive}} : Num {- 0 x}}
             {negate 23}})
      (numV -23))

;; si las funciones de contrato devuelven false devuelve un error de runtime por no satisfacer contrato

(test/exn (run '{{define {pair-non-zero? {p : {Pair Num Num}}} : Bool {&& {!{= 0 {fst p}}} {!{= 0 {snd p}}}}}
                 {define {pair-sum {p : {Pair Num Num} @ pair-non-zero?}} : Num {+ {fst p} {snd p}}}
                 {+ {pair-sum {cons 30 5}} {pair-sum {cons 60 0}}}
                 })
          "Runtime contract error: {cons 60 0} does not satisfy pair-non-zero?")

(test/exn (run '{{define {add {x : Num} {y : Num}} : Num {+ x y}}
                 {define {oh-no {x : Num @ add}} : Bool x}
                 {oh-no 21 21}})
          "Static contract error: invalid type for add")

