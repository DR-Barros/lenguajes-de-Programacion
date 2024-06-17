#lang play
(print-only-errors #t)

(require "env.rkt")
(require "core-base.rkt")

#| SL: Surface Language

<SL> ::= <num>
         | {+ <SL> <SL>}
         | {if0 <SL> <SL> <SL>}
         | {with {<sym> <SL>} <SL>}
         | <id>
         | {<SL> <SL>}
         | {fun {<sym> : <mtype>} → <mtype> : <SL>}
         | {printn <SL>}

<mtype> ::= {<mod> <type>} 
         | <type>          
 
<mod> ::= lazy   ; call-by-need / lazy
        | name   ; call-by-name

<type> ::= Num | {<type> -> <type>}

|#
(deftype SL         
  ; los constructores empiezan con 's' para diferenciar con el AST de CL
  ; tp es el tipo de cada nodo
  (snum tp n)
  (sadd tp l r)
  (sif0 tp c t f)
  (swith tp x e b)
  (sfun tp p b)
  (sprintn tp e)
  (sid tp s)
  (sapp tp f e))

(deftype Type
  (TNum)
  (TFun dom cod)
  (TMod mod type))

(deftype mod
  (eager)
  (lazy)
  (name))

; sl-type : SL -> Type
; accesor polimórfico a la info de tipo de una expr
(define (sl-type sl)
  (match sl
    [(snum tp _) tp]
    [(sadd tp _ _) tp]
    [(sif0 tp _ _ _) tp]
    [(swith tp _ _ _) tp]
    [(sfun tp _ _) tp]
    [(sprintn tp _) tp]
    [(sid tp _) tp]
    [(sapp tp _ _) tp]))

; parse-sl : s-expr -> SL
; parsea expresiones SL
(define (parse-sl s-expr)
  (match s-expr
    [(? number?) (snum #f s-expr)]
    [(? symbol?) (sid #f s-expr)]
    [(list '+ l r) (sadd #f (parse-sl l) (parse-sl r))]
    [(list 'if0 c t f) (sif0 #f (parse-sl c) (parse-sl t) (parse-sl f))]
    [(list 'with (list x e) b) (swith #f x (parse-sl e) (parse-sl b))]
    [(list 'printn e) (sprintn #f (parse-sl e))]
    [(list 'fun (list x ': t1) '-> t2 ': b) (sfun (parse-type (list t1 '-> t2)) x (parse-sl b))]
    [(list f a) (sapp #f (parse-sl f) (parse-sl a))]))

(define (parse-type s-expr)
  (match s-expr
    ['Num (TNum)]
    [(list t1 '-> t2) (TFun (parse-mtype t1) (parse-mtype t2))]
    [_ (error "Tipo no encontrado")]))



(define (parse-mtype s-expr)
  (match s-expr
    [(list 'lazy t) (TMod (lazy) (parse-type t))]
    [(list 'name t) (TMod (name) (parse-type t))]
    ['Num (TMod (eager) (TNum))]
    [(list t1 '-> t2) (TFun (parse-mtype t1) (parse-mtype t2))]
    [_ (error "Mtype no encontrado")]))

(define (type-mod t)
  (match t
    [(TFun _ _) (type-mod-dom t)]
    [(TMod x _) x]
    [(TNum) (eager)]))

(define (type-mod-rec t)
  (match t
    [(TFun _ rec)
     (match rec
       [(TMod x _) x])]
    [_ (error "Mod type invalid ~a" t)]))

(define (type-mod-dom t)
  (match t
    [(TFun dom _)
     (match dom
       [(TMod x _) x])]
    [_ (error "Mod type invalid ~a" t)]))

; type->str : Type -> String
; representación en string de un tipo
(define (type->str t)
  (match t
    [(TNum) "Num"]
    [(TFun t1 t2) (string-append "{" (type->str t1)
                                 " -> " (type->str t2) "}")]))

; check-type : Type Type -> Void
; falla si los dos tipos no son iguales
(define (check-type expected actual)
  (when (not (compatible? expected actual))  ; when es como un if con una sola rama (si la condicion es falsa no evalua nada)
    (error (format "type error: expected ~a, got ~a"
                   (type->str expected) (type->str actual)))))

; check-function-type : Type -> Void
; falla si el tipo no es un tipo función
(define (check-function-type t)
  (when (not (TFun? t))
    (error (format "type error: expected a function type, got ~a"
                   (type->str t)))))

; tipo num por defecto (usado por type-ast)
(define tnum (TNum))

; type-ast : SL Env -> SL
; retorna el ast decorado con tipos (o falla si la expr no es válida)
; se usa Env como un ambiente de tipos (mapea identificadores a tipos)
(define (type-ast expr tenv)
  (match expr 
    [(snum _ n) (snum tnum n)]
    
    [(sadd _ l r) (def tl (type-ast l tenv))
                  (def tr (type-ast r tenv))
                  (check-type tnum (sl-type tl))
                  (check-type tnum (sl-type tr))
                  (sadd tnum tl tr)]

    [(sfun t x b)  (check-function-type t)
                   (def tb (type-ast b (extend-env x (TFun-dom t) tenv)))
                   (check-type (TFun-cod t) (sl-type tb))
                   (sfun t x tb)]
    
    [(sid _ x) (sid (env-lookup x tenv) x)]

    [(sapp _ f a) (def tf (type-ast f tenv))
                  (def t (sl-type tf))
                  (check-function-type t)
                  (def ta (type-ast a tenv))
                  (check-type (TFun-dom t) (sl-type ta))
                  (sapp (TFun-cod t) tf ta)]

    [(swith _ x e b) (def te (type-ast e tenv))
                     (def tb (type-ast b (extend-env x (sl-type te) tenv)))
                     (swith (sl-type tb) x te tb)]

    [(sif0 _ c t f) (def tc (type-ast c tenv))
                    (check-type tnum (sl-type tc))
                    (def tt (type-ast t tenv))
                    (def tf (type-ast f tenv))
                    (check-type (sl-type tt) (sl-type tf))
                    (sif0 (sl-type tt) tc tt tf)]

    [(sprintn _ e) (def te (type-ast e tenv))
                   (check-type tnum (sl-type te))
                   (sprintn tnum te)]))
    

(define (transform expr)
  (match expr
    [(snum _ n)      (num n)]
    [(sid tp x)       
      ;(print (type-mod tp))
      (match (type-mod tp)
        [(eager) (id x)]
        [(lazy)  (app (id x) (num 0))]
        [(name)  (app (id x) (num 0))]
        [_ (error (format "type error: expected a function type, got ~a" (type-mod tp)))]
        )]
    [(swith _ x e b) (app (fun x (transform b)) (transform e))]
    [(sadd _ l r)    (add (transform l) (transform r))]
    [(sif0 _ c t f)  (if0 (transform c) (transform t) (transform f))]
    [(sfun t x b)
      (match (type-mod-dom t)
        [(eager)(fun x (transform b))]
        [(lazy) (mfun x (transform b))]
        [(name) (fun x (transform b))]
        [_ (error (format "type error: expected a function type, got ~a" (type-mod-rec t)))]
        )]
      [(sapp t f a)
        (def tf (sl-type f))
        #| (def (app (id df) da) (transform f))
        (print df)
        (print (transform f)) |#
        (match (type-mod-dom tf)
        [(eager)(app (transform f) (transform a))]
        [(lazy) 
          (def (app (id df) da) (transform f))
          (app (id df) (mfun 'x (transform a)))]
        [(name) 
          (def (app (id df) da) (transform f))
          (app (id df) (fun 'x (transform a)))]
        [_ (error (format "type error: expected a function type, got ~a"(type-mod-dom tf)))])]
      [(sprintn _ e)   (printn (transform e))]))

(define (run-sl prog)
  (interp-top (transform (type-ast (parse-sl prog) empty-env))))


(define (run-p-sl prog)
  (interp-p (transform (type-ast (parse-sl prog) empty-env))))


(define (compatible? t1 t2)
  (equal? (type? t1) (type? t2)))

(define (type? t)
  (match t
    [(TMod _ t) (type? t)]
    [_ t]))

;; Test
(test (type-ast
        (snum 1 5)
        empty-env)
      (snum (TNum) 5))

(test (type-ast
        (sadd 1 (snum 2 3) (snum 4 5))
        empty-env)
      (sadd (TNum) (snum (TNum) 3) (snum (TNum) 5)))

(test (type-ast
        (sfun (TFun (TMod 'eager (TNum)) (TMod 'eager (TNum))) 'x (sadd 1 (sid 2 'x) (sid 3 'x)))
        empty-env)
      (sfun (TFun (TMod 'eager (TNum)) (TMod 'eager (TNum))) 'x (sadd (TNum) (sid (TMod 'eager (TNum)) 'x) (sid (TMod 'eager (TNum)) 'x))))

(test (type-ast
        (sapp 1 (sfun (TFun (TMod 'eager (TNum)) (TMod 'eager (TNum))) 'x (sadd 1 (sid 2 'x) (sid 3 'x))) (snum 4 5))
        empty-env)
      (sapp (TMod 'eager (TNum)) (sfun (TFun (TMod 'eager (TNum)) (TMod 'eager (TNum))) 'x (sadd (TNum) (sid (TMod 'eager (TNum)) 'x) (sid (TMod 'eager (TNum)) 'x))) (snum (TNum) 5)))

(test (type-ast
        (swith 1 'x (snum 2 5) (sadd 3 (sid 4 'x) (snum 5 6)))
        empty-env)
      (swith (TNum) 'x (snum (TNum) 5) (sadd (TNum) (sid (TNum) 'x) (snum (TNum) 6))))

(test (type-ast
        (sif0 1 (snum 2 0) (snum 3 1) (sadd 4 (snum 5 2) (snum 6 3)))
        empty-env)
      (sif0 (TNum) (snum (TNum) 0) (snum (TNum) 1) (sadd (TNum) (snum (TNum) 2) (snum (TNum) 3))))

(test (type-ast
        (sprintn 1 (snum 2 5))
        empty-env)
      (sprintn (TNum) (snum (TNum) 5)))


;; Testear

(test (run-p-sl '{with {f {fun {x : Num} -> Num : {+ x x}}}   
                     {f {printn 10}}}) (result (numV 20) '(10)))


(test (run-p-sl '{with {f {fun {x : {lazy Num}} -> Num : {+ x x}}}   
                     {f {printn 10}}}) (result (numV 20) '(10)))


(test (run-p-sl '{with {f {fun {x : {name Num}} -> Num : {+ x x}}} 
                     {f {printn 10}}}) (result (numV 20) '(10 10)))

(test (run-p-sl '{with {f {fun {x : {lazy Num}} -> Num : 1}}   
                     {f {printn 10}}}) (result (numV 1) '()))
(test (run-p-sl '{with {f {fun {x : {name Num}} -> Num : 1}}   
                     {f {printn 10}}}) (result (numV 1) '()))
(test (run-p-sl '{with {f {fun {x : Num} -> Num : 1}}   
                     {f {printn 10}}}) (result (numV 1) '(10)))

;; test de run-p-sl con funciones anidadas para probar el manejo de los modificador de tipo
(test (run-p-sl '{with {f {fun {x : Num} -> Num : {+ x x}}}   
                     {with {g {fun {y : Num} -> Num : {f {+ y y}}}}
                          {g {printn 10}}}}) (result (numV 40) '(10)))

(test (run-p-sl '{with {f {fun {x : Num} -> Num : {+ x x}}}  
                     {with {g {fun {y : {lazy Num}} -> Num : {f {+ y y}}}}
                          {g {printn 10}}}}) (result (numV 40) '(10)))

(test (run-p-sl '{with {f {fun {x : Num} -> Num : {+ x x}}}
                      {with {g {fun {y : {name Num}} -> Num : {f {+ y y}}}}
                            {g {printn 10}}}}) (result (numV 40) '(10 10)))

(test (run-p-sl '{with {f {fun {x : {lazy Num}} -> Num : {+ x x}}}   
                      {with {g {fun {y : Num} -> Num : {f {+ y y}}}}
                            {g {printn 10}}}}) (result (numV 40) '(10)))      

(test (run-p-sl '{with {f {fun {x : {lazy Num}} -> Num : {+ x x}}}
                      {with {g {fun {y : {lazy Num}} -> Num : {f {+ y y}}}}
                            {g {printn 10}}}}) (result (numV 40) '(10)))

(test (run-p-sl '{with {f {fun {x : {lazy Num}} -> Num : {+ x x}}}
                      {with {g {fun {y : {name Num}} -> Num : {f {+ y y}}}}
                            {g {printn 10}}}}) (result (numV 40) '(10 10)))

(test (run-p-sl '{with {f {fun {x : {name Num}} -> Num : {+ x x}}}
                      {with {g {fun {y : Num} -> Num : {f {+ y y}}}}
                            {g {printn 10}}}}) (result (numV 40) '(10)))

(test (run-p-sl '{with {f {fun {x : {name Num}} -> Num : {+ x x}}}
                      {with {g {fun {y : {lazy Num}} -> Num : {f {+ y y}}}}
                            {g {printn 10}}}}) (result (numV 40) '(10)))

(test (run-p-sl '{with {f {fun {x : {name Num}} -> Num : {+ x x}}}
                      {with {g {fun {y : {name Num}} -> Num : {f {+ y y}}}}
                            {g {printn 10}}}}) (result (numV 40) '(10 10 10 10)))