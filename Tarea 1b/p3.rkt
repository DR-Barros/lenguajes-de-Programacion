
#lang play

;;;;
#|  COPIE Y PEGUE SU CODIGO DE LA PREGUNTA DOS   |#
#| LUEGO MODIFIQUELO SIGUIENDO LAS INSTRUCCIONES |#
;;;;


#|

Extensión a la sintaxis concreta del lenguaje:

<fundef> ::= {define {<id> <arg>*} : <type> <expr>} ; como antes
<arg>    ::= {<id> : <type>}        ; como antes
           | {<id> : <type> @ <contract>}  ; lo único nuevo
|#


(require "env.rkt")

#|

Extensión de la gramática del lenguaje:

<prog> y <expr> no cambian


<fundef> ::= {define {<id> <arg>*} : <type> <expr>} 
 
<arg>    ::= {<id> : <type>}
           | {<id> : <type> @ <contract>}
 
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
  (arg id type)
  (argc id type contract))

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
    [(list x ': y @ contract) (argc (id x) (parse-type y) contract)]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   AUX Type Functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; pp-type :: Type -> String
; Dado un tipo del lenguaje, retorna su representación como String.
(define (pp-type type)
  (match type
    [(numT) "Num"]
    [(boolT) "Bool"]
    [(pairT f s) (format "{Pair ~a ~a}" (pp-type f) (pp-type s))]
    ))


;; error-msg-type :: String Val Val -> Error
;; devuelve el mensaje de error en tiempo de ejecución
(define (error-msg-type fun e v)
  (error (format "Static type error: operator ~a expected ~a found ~a" fun (pp-type e) (pp-type v))))

;; single-type-check :: Type Type Type String -> Type
;; revisa si el valor dado es igual al esperado y devuelve el tipo de retorno
;; sino retorna un error 
(define (single-type-check eType vType rType op)
  (if (equal? eType vType)
      rType
      (error-msg-type op eType vType)))

;; double-type-check :: Type Type Type Type String -> Type
;; revisa si los valores dados son iguales al esperado y devuelve el tipo de retorno
;; sino retorna un error 
(define (double-type-check eType v1Type v2Type rType op)
  (if (equal? eType v1Type)
      (if (equal? eType v2Type) rType (error-msg-type op eType v2Type))
      (error-msg-type op eType v1Type)))


;; generate-new-env :: Env Listof(Args) Listof(FunDef) -> Env
;; genera un nuevo ambiente de tipos para entrar en una función 
(define (generate-new-envT envT args funs)
  (foldl (λ (a e) (match a
                    [(arg (id x) type) (extend-env x type e)]
                    [(argc (id x) type contract)
                     (def (fundef _ f-args f-type body) (look-up contract funs))
                     (if (equal? 1 (length f-args))
                         (if (equal? f-type (boolT))
                          (extend-env x type e)
                          (error (format "Static contract error: invalid type for ~a" contract))
                         )
                         (error (format "Static contract error: invalid type for ~a" contract)))
                     ])) envT args))


;;;;;;;;;;;;;;;
;; typecheck ;;
;;;;;;;;;;;;;;;

;; typecheck-arg :: Arg Env List(Fundef) -> Type
(define (typecheck-arg a envT funs)
  (match a
    [(arg id type) type]
    [(argc (id x) type contract) type]))


;; typecheck-expr :: Expr Env List(Fundef) -> Type
; chequea los tipos de una expresión dadoun ambiente y una lista de funciones
; reporte el tipo de error estatico si algun tipo esta mal
(define (typecheck-expr e envT funs)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    [(id n) (env-lookup n envT)]
    [(pair l r) (pairT (typecheck-expr l envT funs) (typecheck-expr r envT funs))]
    [(add1 a) (single-type-check (numT) (typecheck-expr a envT funs) (numT) "add1")]
    [(add l r) (double-type-check (numT) (typecheck-expr l envT funs) (typecheck-expr r envT funs) (numT) "+")]
    [(sub l r) (double-type-check (numT) (typecheck-expr l envT funs) (typecheck-expr r envT funs) (numT) "-")]
    [(lt l r) (double-type-check (numT) (typecheck-expr l envT funs) (typecheck-expr r envT funs) (boolT) "<")]
    [(eq l r) (double-type-check (numT) (typecheck-expr l envT funs) (typecheck-expr r envT funs) (boolT) "=")]
    [(not-new x) (single-type-check (boolT) (typecheck-expr x envT funs) (boolT) "!")]
    [(and-new l r) (double-type-check (boolT) (typecheck-expr l envT funs) (typecheck-expr r envT funs) (boolT) "&&")]
    [(or-new l r) (double-type-check (boolT) (typecheck-expr l envT funs) (typecheck-expr r envT funs) (boolT) "||")]
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



;; typecheck-fundef :: Fundef list[FunDef] -> Type
;; recibe una función y devuelve el tipo de retorno
;; o un error de tipos
(define (typecheck-fundef f funs)
  (def (fundef name args type body) f)
  (def envT (generate-new-envT empty-env args funs))
  (if (equal? type (typecheck-expr body envT funs))
      type
      (error "tipos distintos en funcion")))

;; typecheck :: Prog -> Type
;; recibe un programa y devuelve el tipo de retorno
;; o un error de tipos
(define (typecheck p)
  (match p
    [(prog funs expr)
     (map (λ (x) (typecheck-fundef x funs)) funs)
     (typecheck-expr expr empty-env funs)]
    [_ (error "error al machear prog")]))





;;;;;;;;;;;;;;;;;;;;;;;
;;   AUX Functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;


; pp-val :: Val -> String
; Dado un valor del lenguaje, retorna
; su representación como String.
(define (pp-val val)
  (match val
    [(numV n) (number->string n)]
    [(boolV b) (format "~a" b)]
    [(pairV f s) (format "{cons ~a ~a}" (pp-val f) (pp-val s))]
    ))

;; error-msg :: String Val -> Error
;; devuelve el mensaje de error en tiempo de ejecución
(define (error-msg word v)
  (error (format "Runtime type error: expected ~a found ~a" word (pp-val v))))

;; eval-add :: NumV NumV -> NumV
;; suma dos NumV o entrega un error
(define (eval-add l r)
  (match* (l r)
    [((numV l)(numV r)) (numV (+ l r))]
    [(_ (numV r)) (error-msg "Number" l)]
    [((numV l) _) (error-msg "Number" r)]))

;; eval-sub :: NumV NumV -> NumV
;; resta dos NumV o entrega un error
(define (eval-sub l r)
  (match* (l r)
    [((numV l)(numV r)) (numV (- l r))]
    [(_ (numV r)) (error-msg "Number" l)]
    [((numV l) _) (error-msg "Number" r)]))

;; eval-lt :: NumV NumV -> BoolV
;; evalua si un NumV es menor a otro o da un error si no son NumV
(define (eval-lt l r)
  (match* (l r)
    [((numV l)(numV r)) (boolV (< l r))]
    [(_ (numV r)) (error-msg "Number" l)]
    [((numV l) _) (error-msg "Number" r)]))

;; eval-eq :: NumV NumV -> BoolV
;; evalua si un numV es igual a otro o da un error si no son NumV
(define (eval-eq l r)
  (match* (l r)
    [((numV l)(numV r)) (boolV (= l r))]
    [(_ (numV r)) (error-msg "Number" l)]
    [((numV l) _) (error-msg "Number" r)]))

;; eval-not :: BoolV -> BoolV
;; negación de un BoolV o error
(define (eval-not expr)
  (match expr
    [(boolV n) (boolV (not n))]
    [_ (error-msg "Boolean" expr)]))


;; eval-and :: BoolV BoolV -> BoolV
;; hace una evaluacion logica de un and entre 2 BoolV o error
(define (eval-and l r)
  (match* (l r)
    [((boolV l)(boolV r)) (boolV (and l r))]
    [(_ (boolV r)) (error-msg "Boolean" l)]
    [((boolV l) _) (error-msg "Boolean" r)]))

;; eval-or :: BoolV BoolV -> BoolV
;; hace una evaluacion logica de un or entre 2 BoolV o error
(define (eval-or l r)
  (match* (l r)
    [((boolV l)(boolV r)) (boolV (or l r))]
    [(_ (boolV r)) (error-msg "Boolean" l)]
    [((boolV l) _) (error-msg "Boolean" r)]))


;; eval-fst :: PairV -> Val
;; entrega el primer elemento de un PairV o unerror
(define (eval-fst expr)
  (match expr
    [(pairV l r) l]
    [_ (error-msg "Pair" expr)]))

;; eval-snd :: PairV -> Val
;; entrega el segundo elemento de un PairV o un error
(define (eval-snd expr)
  (match expr
    [(pairV l r) r]
    [_ (error-msg "Pair" expr)]))

;; eval-if :: BoolV -> Boolean
;; entrega verdadero o falso segun el valor de la condición o un error
(define (eval-if cond)
  (match cond
    [(boolV c) c]
    [_ (error-msg "Boolean" cond)]))

;; check-contract :: String Val Listof(FunDef) -> BoolV
;; devuelve el resultado de evaluar e;; check-contract :: String Val Listof(FunDef) -> BoolV
;; devuelve el resultado de evaluar el contrato
(define (check-contract contract val funs)
  (def (fundef _ the-arg type the-body) (look-up contract funs))
  (def (arg (id y) _) (car the-arg))
  (interp the-body (extend-env y val empty-env) funs))

;; generate-new-env :: Env Listof(Expr) Listof(Expr) Listof(FunDef) -> Env
;; genera un nuevo ambiente para entrar en una función
;; chequea que se cumpla el contrato
(define (generate-new-env env the-args args-expr funs) 
  (foldl (λ (args val e) (match args
                           [(arg i type)(def (id x) i)(extend-env x (interp val env funs) e)]
                           [(argc i type contract)
                            (def v (interp val env funs))
                            (def (id x) i)
                            (def (boolV c) (check-contract contract v funs))
                            (if c
                                (extend-env x v e)
                                ;(interp the-body (extend-env y v empty-env) funs))])) empty-env the-args args-expr))
                                (error (format "Runtime contract error: ~a does not satisfy ~a" (pp-val v) contract)))])) empty-env the-args args-expr))


;; look-up :: <sym> listof(FunDef) -> FunDef
;; searches a function definition within a list of definitions
(define (look-up f-name l)
  (match l
    [(list) (error 'look-up "Undefined function: ~a" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head)) head (look-up f-name tail))]))

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
    [(add1 e) (eval-add (interp e env funs) (numV 1))]
    [(add l r) (eval-add (interp l env funs) (interp r env funs))]
    [(sub l r) (eval-sub (interp l env funs) (interp r env funs))]
    [(lt l r) (eval-lt (interp l env funs) (interp r env funs))]
    [(eq l r) (eval-eq (interp l env funs) (interp r env funs))]
    [(not-new e) (eval-not (interp e env funs))]
    [(and-new l r) (eval-and (interp l env funs) (interp r env funs))]
    [(or-new l r) (eval-or (interp l env funs) (interp r env funs))]
    [(fst e) (eval-fst (interp e env funs))]
    [(snd e) (eval-snd (interp e env funs))]
    [(if-new c t f) (if (eval-if (interp c env funs))
                        (interp t env funs)
                        (interp f env funs))]
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



;; run :: s-expr -> Val
;; ejecuta un programa (lo parsea y luego lo interpreta)
(define (run src)
  (def (prog funs expr) (parse-prog src))
  (def type (typecheck (prog funs expr)))
  (interp expr empty-env funs))

