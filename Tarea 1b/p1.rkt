#lang play

(require "env.rkt")

;; Parte 1

#|

Gramática BNF de la sintaxis concreta del lenguaje.

<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {- <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr> <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}



|#


;;;;;;;;;;;;;
;;   AST   ;;
;;;;;;;;;;;;;


(deftype Fundef
  (fundef name args body))

(deftype Binding
  (binding id expr))

(deftype Expr
  (num n)
  (id s)
  (bool b)
  (pair l r)
  (add1 e)
  (add l r)
  (sub l r)
  (lt l r)
  (eq l r)
  (not-new b)
  (and-new l r)
  (or-new l r)
  (fst p)
  (snd p)
  (if-new c t f)
  (with bindings body)
  (app name args))

(deftype Prog
  (prog fundefs main))




;; Tipo inductivo para los valores del lenguaje.
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))


;;;;;;;;;;;;;;
;;  PARSER  ;;
;;;;;;;;;;;;;;

; parse-fundef :: s-expr -> Fundef
(define (parse-fundef src)
  (match src
    [(list 'define (list fname args ...) body) (fundef fname args (parse-expr body))]
   ))

; parse-binding :: s-expr -> Binding
(define (parse-binding src)
  (match src
    [(list id expr) (binding id (parse-expr expr))]))

; parse-expr :: s-expr -> Expr
(define (parse-expr src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?) (bool src)]
    [(list 'cons l r) (pair (parse-expr l) (parse-expr r))]
    [(list 'add1 e) (add1 (parse-expr e))]
    [(list '+ l r) (add (parse-expr l) (parse-expr r))]
    [(list '- l r) (sub (parse-expr l) (parse-expr r))]
    [(list '< l r) (lt (parse-expr l) (parse-expr r))]
    [(list '= l r) (eq (parse-expr l) (parse-expr r))]
    [(list '! b) (not-new (parse-expr b))]
    [(list '&& l r) (and-new (parse-expr l) (parse-expr r))]
    [(list '|| l r) (or-new (parse-expr l) (parse-expr r))]
    [(list 'fst p) (fst (parse-expr p))]
    [(list 'snd p) (snd (parse-expr p))]
    [(list 'if c t f) (if-new (parse-expr c)
                              (parse-expr t)
                              (parse-expr f))]
    [(list 'with (list bindings ...) body) (with (map parse-binding bindings) (parse-expr body))]
    [(list name args ...) (app name (map parse-expr args))]))

; parse-prog :: s-expr -> Prog
(define (parse-prog src)
  (match src
    [(list fundefs ... expr) (prog (map parse-fundef fundefs) (parse-expr expr))]))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;   PRETTY-PRINTING   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


; pp-val :: Val -> String
; Dado un valor del lenguaje, retorna
; su representación como String.
(define (pp-val val)
  (match val
    [(numV n) (number->string n)]
    [(boolV b) (format "~a" b)]
    [(pairV f s) (format "{cons ~a ~a}" (pp-val f) (pp-val s))]
    ))



;;;;;;;;;;;;;;;;;;;;
;;   INTÉRPRETE   ;;
;;;;;;;;;;;;;;;;;;;;


;; interp :: 
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    [(pair l r) (pairV (interp l env funs) (interp r env funs))]
    ; ...
    [_ (error "not yet implemented")]
    ))

;; run :: s-expr -> Val
(define (run src)
  (interp (parse-prog src)))



;; testeo de funciones
(parse-expr 2)
(interp (parse-expr 2) empty-env '())