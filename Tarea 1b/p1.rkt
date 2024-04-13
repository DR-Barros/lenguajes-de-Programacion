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


;; interp :: Expr Env listof(FunDef) -> Val
;; interpreta la expresion y obtiene su valor dado un ambiente y un listado de funciones
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    [(pair l r) (pairV (interp l env funs) (interp r env funs))]
    [(add1 e)
     (def expr (interp e env funs))
     (match expr
       [(numV n) (numV (+ n 1))]
       [_ (error (format "Runtime type error: expected Number found ~a" (pp-val expr)))])]
    [(add l r)
     (def left (interp l env funs))
     (def rigth (interp r env funs))
     (match* (left rigth)
       [((numV l)(numV r)) (numV (+ l r))]
       [(_ (numV r)) (error (format "Runtime type error: expected Number found ~a" (pp-val left)))]
       [((numV l) _) (error (format "Runtime type error: expected Number found ~a" (pp-val rigth)))])]
    [(sub l r)
     (def left (interp l env funs))
     (def rigth (interp r env funs))
     (match* (left rigth)
       [((numV l)(numV r)) (numV (- l r))]
       [(_ (numV r)) (error (format "Runtime type error: expected Number found ~a" (pp-val left)))]
       [((numV l) _) (error (format "Runtime type error: expected Number found ~a" (pp-val rigth)))])]
    [(lt l r)
     (def left (interp l env funs))
     (def rigth (interp r env funs))
     (match* (left rigth)
       [((numV l)(numV r)) (boolV (< l r))]
       [(_ (numV r)) (error (format "Runtime type error: expected Number found ~a" (pp-val left)))]
       [((numV l) _) (error (format "Runtime type error: expected Number found ~a" (pp-val rigth)))])]
    [(eq l r)
     (def left (interp l env funs))
     (def rigth (interp r env funs))
     (match* (left rigth)
       [((numV l)(numV r)) (boolV (equal? l r))]
       [(_ (numV r)) (error (format "Runtime type error: expected Number found ~a" (pp-val left)))]
       [((numV l) _) (error (format "Runtime type error: expected Number found ~a" (pp-val rigth)))])]
    [(not-new e)
     (def expr (interp e env funs))
     (match expr
       [(boolV n) (boolV (not n))]
       [_ (error (format "Runtime type error: expected Boolean found ~a" (pp-val expr)))])]
    [(and-new l r)
     (def left (interp l env funs))
     (def rigth (interp r env funs))
     (match* (left rigth)
       [((boolV l)(boolV r)) (boolV (and l r))]
       [(_ (boolV r)) (error (format "Runtime type error: expected Boolean found ~a" (pp-val left)))]
       [((boolV l) _) (error (format "Runtime type error: expected Boolean found ~a" (pp-val rigth)))])]
    [(or-new l r)
     (def left (interp l env funs))
     (def rigth (interp r env funs))
     (match* (left rigth)
       [((boolV l)(boolV r)) (boolV (or l r))]
       [(_ (boolV r)) (error (format "Runtime type error: expected Boolean found ~a" (pp-val left)))]
       [((boolV l) _) (error (format "Runtime type error: expected Boolean found ~a" (pp-val rigth)))])]
    [(fst e)
     (def expr (interp e env funs))
     (match expr
       [(pairV l r) l]
       [_ (error (format "Runtime type error: expected Pair found ~a" (pp-val expr)))])]
    [(snd e)
     (def expr (interp e env funs))
     (match expr
       [(pairV l r) r]
       [_ (error (format "Runtime type error: expected Pair found ~a" (pp-val expr)))])]
    [(if-new c t f)
     (def cond (interp c env funs))
     (match cond
       [(boolV c) (if c (interp t env funs) (interp f env funs))]
       [_ (error (format "Runtime type error: expected Boolean found ~a" (pp-val cond)))])]
    [(with bindings body)
     (def new-env (foldl (λ (b e) (match b
                                    [(binding id val) (extend-env id (interp val env funs) e)])) env bindings))
     (interp body new-env funs)]
    [(app f-name args-expr)
     (def (fundef _ the-args the-body) (look-up f-name funs))
     (if (equal? (length the-args) (length args-expr))
     (interp the-body (generate-new-env env the-args args-expr funs) funs)
     (error (format "Arity mismatch: function ~a expected ~a arguments, received ~a" f-name (length the-args) (length args-expr))))]
    ))

(define (generate-new-env env the-args args-expr funs)
  (foldl (λ (id val e) (extend-env id (interp val env funs) e)) env the-args args-expr))


;; run :: s-expr -> Val
;; ejecuta un programa (lo parsea y luego lo interpreta)
(define (run src)
  (def (prog funs expr) (parse-prog src))
  (interp expr empty-env funs))



;; look-up :: <sym> listof(FunDef) -> FunDef
;; searches a function definition within a list of definitions
(define (look-up f-name l)
  (match l
    [(list) (error 'look-up "Undefined function: ~a" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head)) head (look-up f-name tail))]))



