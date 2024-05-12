#lang play

(print-only-errors #t)

(require "env.rkt")

#|
<CL> ::= <num>
         | {+ <CL> <CL>}
         | {if0 <CL> <CL> <CL>}
         | {with {<sym> <CL>} <CL>}
         | <id>
         | {<CL> <CL>}
         | {fun {<sym>} <CL>}
         | {printn <CL>}
|#
(deftype CL
  (num n)
  (add l r)
  (if0 c t f)
  (fun id body)
  (id s)
  (app fun-expr arg-expr)
  (printn e))

;; parse :: s-expr -> CL
(define (parse-cl s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse-cl l) (parse-cl r))]
    [(list 'if0 c t f) (if0 (parse-cl c)
                            (parse-cl t)
                            (parse-cl f))]
    [(list 'with (list x e) b)
     (app (fun x (parse-cl b)) (parse-cl e))]
    [(list 'fun (list x) b) (fun x (parse-cl b))]
    [(list 'printn e) (printn (parse-cl e))]
    [(list f a) (app (parse-cl f) (parse-cl a))]))

;; values
(deftype Val
  (numV n)
  (closV id body env))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closV id body env)]
    [(add l r) (num+ (interp l env) (interp r env))]
    [(if0 c t f)
     (if (num-zero? (interp c env))
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    [(printn e) 
      (def (numV n) (interp e env))
      (println2 n)
      (println n)
      (numV n)]
    [(app fun-expr arg-expr)
     (match (interp fun-expr env)
       [(closV id body fenv)
        (interp body
                (extend-env id
                            (interp arg-expr env)
                            fenv))])]))

(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))
 
(define (num-zero? n)
  (zero? (numV-n n)))
 
;; interp-top :: CL -> number
;; interpreta una expresión y retorna el valor final
(define (interp-top expr)
  (match (interp expr empty-env)
    [(numV n) n]
    [_ 'procedure]))
    
;; run-cl :: s-expr -> number
(define (run-cl prog)
  (interp-top (parse-cl prog)))

;; tests
(test (run-cl '{with {addn {fun {n}
                          {fun {m}
                            {+ n m}}}}
                 {{addn 10} 4}})
      14)

;; Parte 1 Testing de efectos

(deftype Result 
    (result val log))


#|
;; definiciión de un log global
(define log (box '()))

;; println-g :: Number -> Void
;; funcion que toma un numero y lo agrega al log global
(define (println-g n)
  (set-box! log (cons n (unbox log))))

;; interp-g :: Expr -> Result
;; función que vacia el box global, interpreta una expresión y guarda los prints correspondientes en el box
(define (interp-g e)
  (set-box! log '())
  (interp e empty-env))


;; testeo de la salida de impresión
(test (interp-g {printn {num 4}}) (numV 4))
(test (unbox log) '(4))
;; nos aseguramos que luego de cada ejecución se vacie el log global
(test (interp-g {printn {num 5}}) (numV 5))
(test (unbox log) '(5))
|#


(define printer (make-parameter println))

(define (println2 msg)
  ((printer) msg))

(define (interp-p expr)
  (let ([local-log '()])  ; Iniciar log local como una lista vacía
    (parameterize ([printer (lambda (msg) (set! local-log (cons msg local-log)))])  ; Redefinir la impresión para acumular en local-log
      (let ([result (interp expr empty-env)])  ; Ejecutar interp que usará el nuevo printer
        (values result (reverse local-log))))))  ; Devolver el resultado y el log

#|
(define (interp-p e)
  (parameterize ([log  '()])
  (interp e empty-env)))
    |#