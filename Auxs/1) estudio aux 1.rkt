#lang play


;; Auxiliar 1


;; P1
'(p1)
;;a) la diferencia entre (cons 0 1) y (list 0 1) es que por implementación
;; (list 0 1) = (cons 0 (cons 0 empty))
'(a)
(equal? (cons 0 1) (list 0 1))
(equal? (cons 0 (cons 1 empty)) (list 0 1))


;; b) ¿Cómo se relaciona lo anterior con ' (0 . 1) y ' (0 1)?
;; el primero es una forma compacta de hacer un (cons 0 1)
'(b)
(equal? '(0 . 1) (list 0 1))
(equal? '(0 . 1) (cons 0 1))

;; Exprese en notación de pares las siguientes listas:
'(c)
;; ( list 1 2) <=> (cons 1 (cons 2 empty))
(equal? ( list 1 2) (cons 1 (cons 2 empty)))
;; ( list (cons #f #t) (cons 1 0)) <=> (cons (cons #f #t) (cons (cons 1 0) empty))
(equal? ( list (cons #f #t) (cons 1 0)) (cons (cons #f #t) (cons (cons 1 0) empty)))
;; ( list 'a ( list 'b 'c) '( f . g)) <=> (cons a (cons (cons b (cons c empty)) (cons (cons f (cons . (cons g empty))) empty)))
(equal? ( list 'a ( list 'b 'c) '( f . g)) (cons 'a (cons (cons 'b (cons 'c empty)) (cons (cons 'f 'g) empty))))

;;Exprese en notación de lista el par ' ((a . (b . ())) . ((c . (d . ())) . ()))
'(d)
(equal? '((a . (b . ())) . ((c . (d . ())) . ())) (cons (cons 'a (cons 'b  empty)) (cons (cons 'c (cons 'd empty)) empty)))


;; P2
'(p2)
'(a)
;; Define la función (par? n), que recibe como argumento un entero n y retorna un booleano que dice si el numero es par.

;; par? :: Number -> Boolean
(define (par? n)
  (zero? (modulo n 2))) ;; remainder o modulo
(par? 2)
(par? 3)


'(b)
;; Defina la función (gcd a b), que recibe dos enteros a y b, y retorna el máximo común divisor usando el algoritmo euclidiano.
;; gcd :: Number Number -> Number
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b)))) ;; remainder o modulo

(gcd 120 235)
(gcd 2 10)
      
'(c)
;; Define la función (coprime? a b), que recibe dos enteros a y b, y retorna un
;; booleano que dice estos números son primos relativos. Dos números son primos
;; relativos si su único factor común es 1 (ejemplo: 14 y 15).

;; coprime? :: Number Number -> Boolean
(define (coprime? a b)
  (equal? (gcd a b) 1))

(coprime? 14 15)
(coprime? 10 15)



;; P3
'(p3)
'(a)
;; Implemente la función (my-max list), que recibe como argumento una lista de
;; números, y retorna su mayor elemento

;; my-max :: list[Number] -> Boolean
(define  (my-max list)
  (cond
    [(empty? list) 0]
    [else (max (first list) (my-max (rest list)))]))

(my-max '(1 23 5  1 0))
(my-max '(1 23 54  1 0))

'(b)
;; Implemente la función (my-add1 list), que recibe como argumento una lista de
;; números y retorna la lista resultante de aplicar (add1 x) a cada elemento.

;; my-add1 :: list[Number] -> list[Number]
(define  (my-add1 l)
  (cond
    [(empty? l) '()]
    [else (append
           (list (add1 (first l)))
           (my-add1 (rest l)))
     ]))

(my-add1 '(1 23 5  1 0))