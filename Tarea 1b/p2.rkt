#lang play

;;;;
#|  COPIE Y PEGUE SU CODIGO DE LA PREGUNTA UNO   |#
#| LUEGO MODIFIQUELO SIGUIENDO LAS INSTRUCCIONES |#
;;;;


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




;;;;;;;;;;;;;;
;;  PARSER  ;;
;;;;;;;;;;;;;;

(define (parse-type t)
  (match t
    [(list 'Num) (numT)]
    [(list 'Bool) (boolT)]
    [(list 'Pair l r) (pairT (parse-type l) (parse-type r))]
    [_ (error "Tipo no valido")]))
    

; parse-arg :: s-expr -> Arg
(define (parse-arg  a)
  (match a
    [(list x : y) (arg (id x) (parse-type y))]
    [_ (error "error al parsear arg")]))

; parse-fundef :: s-expr -> Fundef
(define (parse-fundef src)
  (match src
    [(list 'define (list fname args ...) body) (fundef fname (map parse-arg args) (parse-expr body))]
   ))

; parse-binding :: s-expr -> Binding
(define (parse-binding src)
  (match src
    [(list id expr) (binding id #f (parse-expr expr))]
    [(list id : type expr) (binding id type expr)]))


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








;; typecheck-expr :: ...
(define (typecheck-expr e envT funs)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    [(id n) (env-lookupT n envT)]
    [(pair l r) (pairT (typecheck-expr l envT funs) (typecheck-expr r envT funs))]
    [(add1 a)
     (if (equal? (typecheck-expr a envT funs) numT)
         (numT)
         (error "error en add1"))]
    [(add l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (if (and (equal? left numT) (equal? rigth numT))
         (numT)
         (error "error en add"))]
    [(sub l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (if (and (equal? left numT) (equal? rigth numT))
         (numT)
         (error "error en sub"))]
    [(lt l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (if (and (equal? left numT) (equal? rigth numT))
         (boolT)
         (error "error en lt"))]
    [(eq l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (if (and (equal? left numT) (equal? rigth numT))
         (boolT)
         (error "error en eq"))]
    [(not-new x)
     (if (equal? (typecheck-expr x envT funs) boolT)
         (boolT)
         (error "error en not-new"))]
    [(and-new l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (if (and (equal? left boolT) (equal? rigth boolT))
         (boolT)
         (error "error en and-new"))]
    [(or-new l r)
     (def left (typecheck-expr l envT funs))
     (def rigth (typecheck-expr r envT funs))
     (if (and (equal? left boolT) (equal? rigth boolT))
         (boolT)
         (error "error en or-new"))]
    [(fst e)
     (def expr (typecheck-expr e envT funs))
     (match expr
       [(pairT lT rT) lT]
       [_ (error "error de first")])]
    [(snd e)
     (def expr (typecheck-expr e envT funs))
     (match expr
       [(pairT lT rT) rT]
       [_ (error "error de first")])]
    [(if-new c t f)
     (def cond (typecheck-expr c envT funs))
     (def true (typecheck-expr t envT funs))
     (def false (typecheck-expr f envT funs))
     (if (equal? cond boolT)
         (if (equal? t f) t (error "los tipos no calzan"))
         (error "error de condicion"))]
    [(with bindings body)
     (def new-env (foldl (λ (b e) (match b
                                   [(binding id type expr) (extend-envT id (typecheck-expr type envT funs) e)])) envT bindings))
     (typecheck-expr body new-env funs)]
    [(app f-name args)
     (def (fundef _ args type body) (look-up f-name funs))
     (type)]
    [_ (error "not yet implemented")]
    ))

(define (generate-new-env envT args args-expr funs)
  (foldl (λ (id type e) (extend-envT id (typecheck type envT funs) e)) empty-envT args args-expr))

;; typecheck-fundef :: ...
(define (typecheck-fundef f)
  ; ...
  (error "not yet implemented"))

;; typecheck :: ...
(define (typecheck prog)
  (error "not yet implemented"))



#|-----------------------------
Environment abstract data type

empty-env  :: Env
extend-env :: Sym Type Env -> Env
env-lookup :: Sym Env -> Type

representation BNF:
<env> ::= (mtEnv)
| (aEnv <id> <type> <env>)
|#
(deftype EnvT
  (mtEnvT)
  (aEnvT id type envT))

(def empty-envT  (mtEnvT))

(def extend-envT aEnvT)

(define (env-lookupT x envT)
  (match envT
    [(mtEnvT) (error 'env-lookupT "free identifier: ~a" x)]
    [(aEnvT id type rest)
     (if (symbol=? id x)
         type
         (env-lookupT x rest))]))


