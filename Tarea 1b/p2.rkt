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


; pp-val :: Type -> String
; Dado un tipo del lenguaje, retorna
; su representación como String.
(define (pp-type type)
  (match type
    [(numT) "Num"]
    [(boolT) "Bool"]
    [(pairT f s) (format "{Pair ~a ~a}" (pp-type f) (pp-type s))]
    ))




;;;;;;;;;;;;;;
;;  PARSER  ;;
;;;;;;;;;;;;;;

(define (parse-type t)
  (match t
    ['Num (numT)]
    ['Bool (boolT)]
    [(list Pair l r) (pairT (parse-type l) (parse-type r))]
    [_ (error "Tipo no valido")]))
    

; parse-arg :: s-expr -> Arg
(define (parse-arg  a)
  (match a
    [(list x : y) (arg (id x) (parse-type y))]
    [_ (error "error al parsear arg")]))

; parse-fundef :: s-expr -> Fundef
(define (parse-fundef src)
  (match src
    [(list 'define (list fname args ...) : type body) (fundef fname (map parse-arg args) (parse-type type) (parse-expr body))]
    [_ (error "error al parsear fundef")]
   ))

; parse-binding :: s-expr -> Binding
(define (parse-binding src)
  (match src
    [(list id expr) (binding id #f (parse-expr expr))]
    [(list id : type expr) (binding id (parse-type type) (parse-expr expr))]
    [_ (error "error al parsear binding")]))


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
    [(list name args ...) (app name (map parse-expr args))]
    [_ (error "error al parsear expr")]
    ))

; parse-prog :: s-expr -> Prog
(define (parse-prog src)
  (match src
    [(list fundefs ... expr) (prog (map parse-fundef fundefs) (parse-expr expr))]
    [_ (error "error al parsear prog")]
    ))





;; typecheck-expr :: ...
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
     (if (equal? cond boolT)
         (if (equal? t f) t (error (error (format "Static type error: expected ~a found ~a" (pp-type true) (pp-type false)))))
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
     (def (fundef _ args type body) (look-up f-name funs))
     (type)]
    [_ (error "no encontre la expresión :(")]
    ))

(define (generate-new-env envT args funs)
  (foldl (λ (a e) (match a
                    [(arg id type) (extend-env id type e)])) envT args))

;; typecheck-fundef :: Fundef list[FunDef] -> Type
(define (typecheck-fundef f funs)
  (def (fundef name args type body) f)
  (def envT (generate-new-env empty-env args funs))
  (if (equal? type (typecheck-expr body envT funs))
      type
      (error "tipos distintos")))

;; typecheck :: Prog -> Type
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


