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
         | {mfun {<id>} <CL>}
|#
(deftype CL
  (num n)
  (add l r)
  (if0 c t f)
  (fun id body)
  (id s)
  (app fun-expr arg-expr)
  (printn e)
  (mfun id body))

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
    [(list 'mfun (list x) b) (mfun x (parse-cl b))]
    [(list f a) (app (parse-cl f) (parse-cl a))]))

;; values
(deftype Val
  (numV n)
  (closV id body env)
  (mclosV id body env memo)) ;; nose si agregar la tabla aca

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
      ((print-fn) n)
      (numV n)]
    [(mfun id body)(mclosV id body env (make-hash))]
    [(app fun-expr arg-expr)
     (define arg (interp arg-expr env))
     (match (interp fun-expr env)
       [(mclosV id body fenv memo)
        (if (hash-has-key? memo arg)
            (hash-ref memo arg)
            (let ([result (interp body
                                  (extend-env id arg fenv))])
              (hash-set! memo arg result)
              result))]             
       [(closV id body fenv)
        (interp body
                (extend-env id arg fenv))])]))

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

(define print-fn (make-parameter println))


(define (interp-p expr)
  (define log (box '()))
  (define (log-print n)
      (set-box! log (cons n (unbox log))))
  (parameterize ([print-fn log-print])
    (let ([val (interp expr empty-env)])
      (result val (reverse (unbox log))))))


;; test parte 1
(test (interp-p {printn {num 5}}) (result (numV 5) '(5)))


;; test parte 2
(test (interp-p {parse-cl '{with {doble {mfun {n} {+ (printn n) n}}} (+ {doble 5} {doble 5})}}) (result (numV 20) '(5)))
(test (interp-p {parse-cl '{with {doble {mfun {n} {+ (printn n) n}}} (+ {doble (printn 3)} {doble (printn 3)})}}) (result (numV 12) '(3 3 3)))
(test (interp-p {parse-cl '{with {doble {fun {n} {+ (printn n) n}}} (+ {doble (printn 3)} {doble (printn 3)})}}) (result (numV 12) '(3 3 3 3)))
(test (interp-p {parse-cl '{with {doble {fun {n} {+ (printn n) n}}} (+ {doble (printn 3)} {+ {doble (printn 5)} {doble (printn 3)}})}}) (result (numV 22) '(3 3 5 5 3 3)))

