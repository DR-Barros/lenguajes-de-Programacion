#lang play

(require "core-base.rkt")
(require "surface-base.rkt")
(require "env.rkt")
(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     CORE LANG     ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests
(test (run-cl '{with {addn {fun {n}
                          {fun {m}
                            {+ n m}}}}
                 {{addn 10} 4}})
      14)


;;;;;;;;;;;;;;;;;;;;;;;
;;;;;     LOG     ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;


;; test num val
(test (interp-p (parse-cl '5))
      (result 5 '()))

;; test clos val
(test (interp-p (parse-cl
                 '{fun {x} x}))
      (result 'procedure '()))

;; test with two logs
(test (interp-p (parse-cl
                 '{+ {printn 1} {printn 2}}))
      (result 3 '("1" "2")))

;; test with nested printn
(test (interp-p (parse-cl
                 '{printn {+ {printn 1} 1}}))
      (result 2 '("1" "2")))


;; test with if0, condition true
(test (interp-p (parse-cl
                 '{if0 0 {printn 10} 5}))
      (result 10 '("10")))


;; test with if0, condition false
(test (interp-p (parse-cl
                 '{if0 1 {printn 10} {printn 5}}))
      (result 5 '("5")))


;; inside function
(test (interp-p (parse-cl
                 '{{fun {x} {printn x}} 10}))
      (result 10 '("10")))

;; with addition nested
(test (interp-p (parse-cl
                 '{printn {+ 5 {printn 5}}}))
      (result 10 '("5" "10")))

;; 
(test (interp-p (parse-cl
                 '{with {f {fun {x}
                                {if0 x
                                     {printn 0}
                                     {printn 1}}}}
                        {printn {+ {f 0} {f {printn 10}}}} }))
      (result 1 '("0" "10" "1" "1")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   MEMOIZACIÓN   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; normal fun ==> 2 printn
(test (interp-p (parse-cl

                 '{with {f {fun {x} {printn x}}}
                        {+ {f 1} {f 1}}}))
      (result 2 '("1" "1")))

;; mfun ==> 1 printn
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {printn x}}}
                        {+ {f 1} {f 1}}}))
      (result 2 '("1")))


;; fun with same name but different body and env
(test (interp-p (parse-cl
                 '{with {f {mfun {h} {h {printn 1}}}}
                        {with {g {fun {x} x}}
                              {+ {f g}
                                 {with {g {fun {x} {+ x 1}}}
                                       {f g}}}}}))
      (result 3 '("1" "1")))

;; mfun with same name but different body and env
(test (interp-p (parse-cl
                 '{with {f {mfun {h} {h {printn 1}}}}
                        {with {g {mfun {x} x}}
                              {+ {f g}
                                 {with {g {mfun {x} {+ x 1}}}
                                       {f g}}}}}))
      (result 3 '("1" "1")))



;; same fun but different env
(test (interp-p (parse-cl
                 '{with {f {mfun {h} {h {printn 1}}}}
                        {with {g {fun {x} x}}
                              {+ {f g} {+ {f g} {f g}}}}}))
      (result 3 '("1")))

;; mfun used by another mfun
(test (interp-p (parse-cl
                 '{with {f {mfun {h} {h 1}}}
                        {with {g {mfun {x} {printn x}}}
                              {+ {f g} {f g}}}}))
      (result 2 '("1")))


;; multiple memoized functions applied sequentially with the same argument
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {printn x}}}
                        {+ {f 1} {+ {f 2} {+ {f 1} {f 2}}}}}))
      (result 6 '("1" "2")))


;; nested memoized functions with the same argument
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {mfun {y} {printn y}}}}
                        {+ {{f 3} 3} {{f 3} 3}}}))
      (result 6 '("3")))


;; memoized function applied inside another memoized function
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {printn x}}}
                        {with {g {mfun {y} {f y}}}
                              {+ {g 5} {g 5}}}}))
      (result 10 '("5")))


;; no dynamic scope
(test (interp-p (parse-cl
                 '{with {x 10}
                        {with {f {mfun {y} {printn (+ x y)}}}
                              {+ {f 1} {with {x 20} {f 1}}}}}))
      (result 22 '("11")))


;; re-using a memoized function with a different closure environment
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {+ {printn x} 1}}}
                        {with {g {fun {x} {+ {f x} {f x}}}}
                              {g 5}}}))
      (result 12 '("5")))


;; complex nesting and environments
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {printn {+ x 1}}}}
                        {with {g {mfun {x} {f {printn x}}}}
                              {with {h {fun {x} {g x}}}
                                    {+ {h 7} {h 7}}}}}))
      (result 16 '("7" "8")))


;; multiple different memoized functions on the same input
(test (interp-p (parse-cl
                 '{+ {with {f {mfun {x} {printn x}}}
                           {f 1}}
                      {with {g {mfun {x} {printn (+ x 1)}}}
                           {g 1}}}))
      (result 3 '("1" "2")))


;; using a memoized function inside a condition
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {printn x}}}
                        {if0 {f 0} {f 1} {f 2}}}))
      (result 1 '("0" "1")))


;; memoized function with side-effects in another memoized function
(test (interp-p (parse-cl
                 '{with {f {mfun {x} {printn x}}}
                        {with {g {mfun {x} {f (+ x 5)}}}
                              {+ {g 1} {g 1}}}}))
      (result 12 '("6")))

(test (interp-p (parse-cl
                '{with {doble {mfun {x} {+ x x}}}
                                {+ {doble {printn 3}} {doble {printn 3}}}}))
      (result 12 '("3" "3")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Chequeo de tipos   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; eager arg
(test (type-ast (parse-sl
       '{{fun {x : Num} -> Num : x} 10}) empty-env)
                (sapp (TNum) (sfun (TFun (TNum) (TNum)) 'x (sid (TNum) 'x)) (snum (TNum) 10)))
;; lazy arg
(test (type-ast (parse-sl
       '{{fun {x : {lazy Num}} -> Num : x} 10}) empty-env)
      (sapp (TNum) (sfun (TFun (TMod (lazy) (TNum)) (TNum)) 'x (sid (TMod (lazy) (TNum)) 'x)) (snum (TNum) 10)))

;; name arg
(test (type-ast (parse-sl
       '{{fun {x : {name Num}} -> Num : x} 10}) empty-env)
      (sapp (TNum) (sfun (TFun (TMod (name) (TNum)) (TNum)) 'x (sid (TMod (name) (TNum)) 'x)) (snum (TNum) 10)))


;; parse mtype
(test (parse-sl  '{fun {_ : {lazy Num}} -> {{lazy {{lazy Num} -> {Num -> Num}}} -> {name Num}} : _ })
  (sfun (TFun (TMod (lazy) (TNum)) (TFun (TMod (lazy) (TFun (TMod (lazy) (TNum)) (TFun (TNum) (TNum)))) (TMod (name) (TNum)))) '_ (sid #f '_)))

;; function with lazy Num that returns Num
(test 
 (type-ast (parse-sl
            '{{fun {x : {lazy Num}} -> Num : x} {+ 10 20}}
            ) empty-env)
 (sapp (TNum)
       (sfun (TFun (TMod (lazy) (TNum)) (TNum)) 'x
                    (sid (TMod (lazy) (TNum)) 'x))
       (sadd (TNum) (snum (TNum) 10) (snum (TNum) 20))))


(test 
 (type-ast (parse-sl
            '{{fun {x : {lazy Num}} -> {lazy Num} : {+ x 1}} {+ 10 20}}
            ) empty-env)
   (sapp (TNum)
         (sfun (TFun (TMod (lazy) (TNum)) (TMod (lazy) (TNum))) 'x ;; funcion que recibe lazy num
               (sadd (TNum)
                     (sid (TMod (lazy) (TNum)) 'x) ;; id con tipo == lazy num
                     (snum (TNum) 1)))
         (sadd (TNum) (snum (TNum) 10) (snum (TNum) 20))))

;; type error, body resuturn x with type Num
(test/exn
 (type-ast (parse-sl
            '{{fun {x : {lazy Num}} -> {name {Num -> {lazy Num}}} : x} 10}) empty-env)
 "type error: expected {Num -> {lazy Num}}, got Num")


(test 
 (type-ast (parse-sl
            '{{fun {x : Num} -> {lazy Num} : {+ x 1}} 5}
            ) empty-env)
   (sapp
    (TNum)
    (sfun
     (TFun (TNum) (TMod (lazy) (TNum)))
     'x
     (sadd (TNum) (sid (TNum) 'x) (snum (TNum) 1)))
    (snum (TNum) 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Transformación a CL   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; test entregados en la tarea

;; Una función de tipo Num -> Num significa que se aplicará con evaluación temprana.
(test (run-p-sl
       '{with {f {fun {x : Num} -> Num : {+ x x}}}   
                     {f {printn 10}}})
      (result 20 '("10")));; Se imprime una vez, al evaluar el argumento en la aplicación


;; Una función de tipo (lazy Num -> Num), usará evaluación lazy/call-by-need para su argumento.
(test (run-p-sl
       '{with {f {fun {x : {lazy Num}} -> Num : {+ x x}}}   
                     {f {printn 10}}})
      (result 20 '("10")))   ;; Se imprime una vez cuando se usa el argumento dentro del cuerpo


;; Una función de tipo (name Num -> Num), usará evaluación name/call-by-name para su argumento.
(test (run-p-sl
       '{with {f {fun {x : {name Num}} -> Num : {+ x x}}} 
                     {f {printn 10}}})
      (result 20 '("10" "10")))  ;; Se imprime dos veces, una por cada uso que se hace del argumento, dentro del cuerpo
 
;; Otro ejemplo de lazy. Note que efectivamente al evaluarla se comporta correctamente.
(test (run-p-sl
       '{with {f {fun {x : {lazy Num}} -> Num : 1}}   
                     {f {printn 10}}})
      (result 1 '()))   ;; No se imprime porque el argumento nunca se usa dentro del cuerpo

;;;;;


;; basic test
(test (run-p-sl
       '{printn {+ {printn 1} 1}}) (result 2 '("1" "2")))

; name
(test (run-p-sl
       '{with {f {fun {x : {name Num}} -> Num : 1}}   
                    {f {printn 10}}})
  (result 1 '()))

;; nested functions
;; eager
(test (run-p-sl
       '{with {f {fun {x : Num} -> {Num -> Num} : {fun {y : Num} -> Num : {+ x y}}}}
                    {{f {printn 5}} {printn 10}}})
  (result 15 '("5" "10")))

;; lazy
(test (run-p-sl
       '{with {f {fun {x : {lazy Num}} -> {Num -> Num} : {fun {y : Num} -> Num : {+ x y}}}}
                    {{f {printn 5}} {printn 10}}})
  (result 15 '("10" "5")))

;; name
(test (run-p-sl
       '{with {f {fun {x : {name Num}} -> {Num -> Num} : {fun {y : Num} -> Num : {+ x y}}}} 
                    {{f {printn 5}} {printn 10}}})
  (result 15 '("10" "5")))

(test (run-p-sl
       '{with {f {fun {x : {name Num}} -> {Num -> Num} : {fun {y : Num} -> Num : {+ x y}}}} 
                    {f {printn 5}}})
  (result 'procedure '()))

;; return lazy Num
(test (run-p-sl
       '{with {f {fun {x : Num} -> {lazy Num} : x}} 
                    {with {g {fun {y : {lazy Num}} -> Num : {+ y y}}}
                          {g {f {printn 5}}}}})
  (result 10 '("5")))

(test (run-p-sl
       '{with {f {fun {x : {lazy Num}} -> {{lazy Num} -> Num} : {fun {y : {lazy Num}} -> Num : 0}}}
              {{f {printn 1}} {printn 2}}})
      (result 0 '()))


;; function that returns a function that returns another function
;; all of them use a lazy Num as argument
(test (run-p-sl
       '{with {f
               {fun {x : {lazy Num}} -> {{lazy Num} -> {{lazy Num} -> Num}}
                    : {fun {y : {lazy Num}} -> {{lazy Num} -> Num}
                           : {fun {z : {lazy Num}} -> Num
                                  : x}}}};; the last function returns the first parameter
              {{{f {printn 1}} {printn 2}} {printn 3}}})
      (result 1 '("1")))

(test (run-p-sl
       '{with {f
               {fun {x : {lazy Num}} -> {{lazy Num} -> {{lazy Num} -> Num}}
                    : {fun {y : {lazy Num}} -> {{lazy Num} -> Num}
                           : {fun {z : {lazy Num}} -> Num
                                  : x}}}}
              {{{f {printn 1}} {printn 2}} {printn 3}}})
      (result 1 '("1")))

;; if0 with functions
(test (run-p-sl
       '{if0 {{fun {x : {lazy Num}} -> {name Num} : x} {printn 0}}
             {{fun {x : Num} -> {lazy Num} : x} {printn 2}}
             {printn 3}})
      (result 2 '("0" "2")))

(test (run-p-sl
       '{with {f {fun {x : Num} -> {Num -> {name Num}}
                      : {fun {y : Num} -> {name Num}
                             : {if0 y
                                    x
                                    {+ x y}}}}}
              {{f {printn 5}} {printn 0}}})
  (result 5 '("5" "0")))

(test (run-p-sl
       '{with {f {fun {x : {lazy Num}} -> {Num -> {name Num}}
                      : {fun {y : Num} -> {name Num}
                             : {if0 y
                                    {printn x}
                                    {+ x y}}}}}
              {{f {printn 5}} 1}})
  (result 6 '("5")))









