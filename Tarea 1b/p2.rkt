#lang play

;;;;
#|  COPIE Y PEGUE SU CODIGO DE LA PREGUNTA UNO   |#
#| LUEGO MODIFIQUELO SIGUIENDO LAS INSTRUCCIONES |#
;;;;
(require "env.rkt")

#|

Extensión de la gramática del lenguaje:

<prog> y <expr> no cambian


<fundef> ::= {define {<id> <arg>*} : <type> <expr>} 
 
<arg>    ::= {<id> : <type>} 
 
<binding> ::= {<id> [: <type>] <expr>}

<prog>   ::= {<fundef>* <expr>}

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
 
<type>   ::= Num | Bool | {Pair <type> <type>}
|#

;;;;;;;;;;;;;
;;   AST   ;;
;;;;;;;;;;;;;


(deftype Fundef
  (fundef name args type body))

(deftype Arg
  (arg id type))

(deftype Binding
  (binding id type expr))

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



(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;   PRETTY-PRINTING   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


; pp-type :: Type -> String
; Dado un tipo del lenguaje, retorna su representación como String.
(define (pp-type type)
  (match type
    [(numT) "Num"]
    [(boolT) "Bool"]
    [(pairT f s) (format "{Pair ~a ~a}" (pp-type f) (pp-type s))]
    ))




;;;;;;;;;;;;;;
;;  PARSER  ;;
;;;;;;;;;;;;;;

; parse-type s-expr -> Type
; dada una s-expr retorna el tipo dado o un error
(define (parse-type t)
  (match t
    ['Num (numT)]
    ['Bool (boolT)]
    [(list Pair l r) (pairT (parse-type l) (parse-type r))]
    [_ (error "Tipo no valido")]))
    

; parse-arg :: s-expr -> Arg
; dada una s-expr retorna un argumento 
(define (parse-arg  a)
  (match a
    [(list x ': y) (arg (id x) (parse-type y))]
    [_ (error "error al parsear arg")]))

; parse-fundef :: s-expr -> Fundef
; dada una s-expr retorna un un fundef 
(define (parse-fundef src)
  (match src
    [(list 'define (list fname args ...) : type body) (fundef fname (map parse-arg args) (parse-type type) (parse-expr body))]
    [_ (error "error al parsear fundef")]
   ))

; parse-binding :: s-expr -> Binding
; dada una s-expr retorna un binfing
(define (parse-binding src)
  (match src
    [(list id expr) (binding id #f (parse-expr expr))]
    [(list id ': type expr) (binding id (parse-type type) (parse-expr expr))]
    [_ (error "error al parsear binding")]))


; parse-expr :: s-expr -> Expr
; dada una s-expr retorna una expresión
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
    [(list name args ...) (app name (map parse-expr args))]
    [_ (error "error al parsear expr")]
    ))

; parse-prog :: s-expr -> Prog
; dada una s-expr retorna un programa 
(define (parse-prog src)
  (match src
    [(list fundefs ... expr) (prog (map parse-fundef fundefs) (parse-expr expr))]
    [_ (error "error al parsear prog")]
    ))


;;;;;;;;;;;;;;;
;; typecheck ;;
;;;;;;;;;;;;;;;

;; typecheck-arg :: Arg Env List(Fundef) -> Type
(define (typecheck-arg a envT funs)
  (match a
    [(arg id type) type]))


;; typecheck-expr :: Expr Env List(Fundef) -> Type
; chequea los tipos de una expresión dadoun ambiente y una lista de funciones
; reporte el tipo de error estatico si algun tipo esta mal
(define (typecheck-expr e envT funs)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    [(id n) (env-lookup n envT)]
    [(pair l r) (pairT (typecheck-expr l envT funs) (typecheck-expr r envT funs))]
    [(add1 a)
     (if (equal? (typecheck-expr a envT funs) (numT))
         (numT)
         (error (format "Static type error: operator add1 expected Num found ~a" (pp-type (typecheck-expr a envT funs)))))]
    [(add l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (match* (left rigth)
       [((numT) (numT)) (numT)]
       [(_ (numT)) (error (format "Static type error: operator + expected Num found ~a" (pp-type left)))]
       [((numT) _) (error (format "Static type error: operator + expected Num found ~a" (pp-type rigth)))])]
    [(sub l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (match* (left rigth)
       [((numT) (numT)) (numT)]
       [(_ (numT)) (error (format "Static type error: operator - expected Num found ~a" (pp-type left)))]
       [((numT) _) (error (format "Static type error: operator - expected Num found ~a" (pp-type rigth)))])]
    [(lt l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (match* (left rigth)
       [((numT) (numT)) (boolT)]
       [(_ (numT)) (error (format "Static type error: operator < expected Num found ~a" (pp-type left)))]
       [((numT) _) (error (format "Static type error: operator < expected Num found ~a" (pp-type rigth)))])]
    [(eq l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (match* (left rigth)
       [((numT) (numT)) (boolT)]
       [(_ (numT)) (error (format "Static type error: operator = expected Num found ~a" (pp-type left)))]
       [((numT) _) (error (format "Static type error: operator = expected Num found ~a" (pp-type rigth)))])]
    [(not-new x)
     (if (equal? (typecheck-expr x envT funs) (boolT))
         (boolT)
         (error (format "Static type error: operator ! expected Bool found ~a" (pp-type (typecheck-expr x envT funs)))))]
    [(and-new l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (match* (left rigth)
       [((boolT) (boolT)) (boolT)]
       [(_ (boolT)) (error (format "Static type error: operator && expected Bool found ~a" (pp-type left)))]
       [((boolT) _) (error (format "Static type error: operator && expected Bool found ~a" (pp-type rigth)))])]
    [(or-new l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (match* (left rigth)
       [((boolT) (boolT)) (boolT)]
       [(_ (boolT)) (error (format "Static type error: operator || expected Bool found ~a" (pp-type left)))]
       [((boolT) _) (error (format "Static type error: operator || expected Bool found ~a" (pp-type rigth)))])]
    [(fst e)
     (def expr (typecheck-expr e envT funs))
     (match expr
       [(pairT lT rT) lT]
       [_ (error (format "Static type error: operator fst expected Pair found ~a" (pp-type expr)))])]
    [(snd e)
     (def expr (typecheck-expr e envT funs))
     (match expr
       [(pairT lT rT) rT]
       [_ (error (format "Static type error: operator snd expected Pair found ~a" (pp-type expr)))])]
    [(if-new c t f)
     (def cond (typecheck-expr c envT funs))
     (def true (typecheck-expr t envT funs))
     (def false (typecheck-expr f envT funs))
     (if (equal? cond (boolT))
         (if (equal? true false) true (error (error (format "Static type error: expected ~a found ~a" (pp-type true) (pp-type false)))))
         (error (format "Static type error: expected Bool found ~a" (pp-type cond))))]
    [(with bindings body)
     (def new-env (foldl (λ (b e) (match b
                                   [(binding id type expr)
                                    (cond
                                      [(equal? type #f) (extend-env id (typecheck-expr expr envT funs) e)]
                                      [(equal? type (typecheck-expr expr envT funs)) (extend-env id type e)]
                                      [else (error (format "Static type error: expected ~a found ~a" (pp-type type) (pp-type (typecheck-expr expr envT funs))))])])) envT bindings))
     (typecheck-expr body new-env funs)]
    [(app f-name args)
     (def (fundef _ f-args type body) (look-up f-name funs))
     (if
       (equal? (length args) (length f-args))
       (if (foldl (λ (f-a a b) (if (and (equal? (typecheck-expr a envT funs) (typecheck-arg f-a envT funs)) b) b
                                   (error (format "Static type error: expected ~a found ~a" (pp-type (typecheck-arg f-a envT funs)) (pp-type (typecheck-expr a envT funs)))))) #t f-args args)
           type
           (error "los tipod de la función no calzan :("))
       (error (format "Static arity mismatch: function ~a expected ~a arguments, received ~a" f-name (length f-args) (length args))))
      ]
    [_ (error "no encontre la expresión :(")]
    ))

;; generate-new-env :: Env Listof(Args) Listof(FunDef) -> Env
;; genera un nuevo ambiente de tipos para entrar en una función 
(define (generate-new-envT envT args funs)
  (foldl (λ (a e) (match a
                    [(arg id type) (extend-env id type e)])) envT args))

;; typecheck-fundef :: Fundef list[FunDef] -> Type
;; recibe una función y devuelve el tipo de retorno
;; o un error de tipos
(define (typecheck-fundef f funs)
  (def (fundef name args type body) f)
  (def envT (generate-new-envT empty-env args funs))
  (if (equal? type (typecheck-expr body envT funs))
      type
      (error "tipos distintos")))

;; typecheck :: Prog -> Type
;; recibe un programa y devuelve el tipo de retorno
;; o un error de tipos
(define (typecheck p)
  (match p
    [(prog funs expr)(typecheck-expr expr empty-env funs)]
    [_ (error "error al machear prog")]))


;; look-up :: <sym> listof(FunDef) -> FunDef
;; searches a function definition within a list of definitions
(define (look-up f-name l)
  (match l
    [(list) (error 'look-up "Undefined function: ~a" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head)) head (look-up f-name tail))]))





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
                                    [(binding id type val)
                                     (extend-env id (interp val env funs) e)])) env bindings))
     (interp body new-env funs)]
    [(app f-name args-expr)
     (def (fundef _ the-args type the-body) (look-up f-name funs))
     (if (equal? (length the-args) (length args-expr))
     (interp the-body (generate-new-env env the-args args-expr funs) funs)
     (error (format "Arity mismatch: function ~a expected ~a arguments, received ~a" f-name (length the-args) (length args-expr))))]
    ))


;; generate-new-env :: Env Listof(Expr)Listof(Expr) Listof(FunDef) -> Env
;; genera un nuevo ambiente para entrar en una función
(define (generate-new-env env the-args args-expr funs) 
  (foldl (λ (args val e) (match args
                           [(arg i type)
                            (def (id x) i)
                            (extend-env x (interp val env funs) e)])) empty-env the-args args-expr))


;; run :: s-expr -> Val
;; ejecuta un programa (lo parsea y luego lo interpreta)
(define (run src)
  (def (prog funs expr) (parse-prog src))
  (def type (typecheck (parse-prog src)))
  (interp expr empty-env funs))


