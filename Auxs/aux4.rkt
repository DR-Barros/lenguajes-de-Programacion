#lang play

(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Definiciones Generales   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment ADT

empty-env  :: Env
extend-env :: Symbol Val Env -> Env
lookup-env :: Symbol Env -> Val or error

Representation BNF:
<Env> ::= (mtEnv)
        | (aEnv <id> <val> <Env>)
|#

(deftype Env
  (mtEnv)
  (aEnv id val env))

; empty-env :: Env
(def empty-env  (mtEnv))

; extend-env :: Symbol Val Env -> Env
(def extend-env aEnv)

; lookup-env :: Symbol Env -> Val or error
; Looks for the value associated to the given identifier.
; Raises an error if the identifier does not exist in the environment.
(define (lookup-env x env)
  (match env
    [(mtEnv) (error 'lookup-env "Free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (lookup-env x rest))]))

;;;;;;;;;;;;;;;;
;;   Parser   ;;
;;;;;;;;;;;;;;;;

#|
Sintaxis concreta

<expr> ::= <num>
         | <sym>
         | {add <expr> <expr>}
         | {with {<sym> <expr>} <expr>}
|#


#|
Sintaxis abstracta

<Expr> ::= (num <num>)
         | (id <sym>)
         | (add <Expr> <Expr>)
         | (with <sym> <Expr> <Expr>)
|#

(deftype Expr
  (num n)
  (id x)
  (add l r)
  (with name named-expr body))

; parse :: S-expr -> Expr
; Retorna el AST correspondiente a la expresión en sintaxis concreta que
; recibe como argumento.
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'with (list (? symbol? id) named-expr) body)
     (with id (parse named-expr) (parse body))]))



;;;;;;;;;;;;
;;   P1   ;;
;;;;;;;;;;;;

; Si, el lenguaje visto en clases acepta funciones recursivas gracias a que maneja List(fundef)


;;;;;;;;;;;;
;;   P2   ;;
;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  usar fold left :) para los with  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;
;;   P3   ;;
;;;;;;;;;;;;

; no, cambia solamente la forma de ejecución
; usar ambiente es mas eficiente en tiempo, se paga con memoria 

;;;;;;;;;;;;
;;   P4   ;;
;;;;;;;;;;;;

; Tipo de datos que representa los valores del lenguaje.
(deftype Val
  (numV n)
  (boolV b)
  (pairV lv rv))

; pp-val :: Val -> String
; Dado un valor del lenguaje, retorna
; su representación como String.
(define (pp-val val)
  (match val
    [(numV n) (number->string n)]
    [(boolV b) (format "~a" b)]
    [(pairV f s) (format "{cons ~a ~a}" (pp-val f) (pp-val s))]
    ))


; liftNumV+ :: Val Val -> Val
; ...
(define (liftNumV+ v1 v2)
  (match* (v1 v2)
    [((numV n1) (numV n2)) (numV (+ n1 n2))]
    [((numV _) _) (error (format "runtime type error: expected number received ~a" (pp-val v2)))]
    [(_ (numV _)) (error (format "runtime type error: expected number received ~a" (pp-val v1)))]))


(test (liftNumV+ (numV 3) (numV 4)) (numV 7))
(test/exn (liftNumV+ (numV 3) (boolV #t)) "runtime")

;;;;;;;;;;;;
;;   P5   ;;
;;;;;;;;;;;;

; alpha-rename :: ...
; ...
(define (alpha-rename expr)
  (error "Not implemented"))