#lang play


'(P1)
'(a)

;; cap-strings :: List[Strings] -> List[Strings]
(define ( cap-strings l )
  (map string-upcase l))
(cap-strings (list "hola" "poto" "buena"))


'(b)
;; multiples-of-five :: List[Number] -> List[Number]
(define ( multiples-of-five l )
  (filter (λ (x) (zero? (remainder x 5))) l)) ;; remainder o modulo
(multiples-of-five (list 5 10 9 4 12 17 20))


'(c)
;; list-product :: List[Number] -> Number
(define ( list-product l )
  (foldr (λ (x e) (* e x)) 1 l))
(list-product (list 5 10 9))


'(d)
;; reverse-string :: String -> String
(define (reverse-string s)
  (list->string
   (foldr (λ (x e) (append e (list x))) '() (string->list s))))

(reverse-string "hola")


'(P2)
'(a)
;; la diferencia entre machear (list x y) y '(x y) es que '(x y) <=> (list 'x 'y)
(equal? '(x y) (list 'x 'y))

'(b)
;; Sin usar condicionales (if , cond et al.), implemente la función (sign n) definida de la siguiente manera:

;; sign :: Number -> Number
(define (sign n)
  (define (mayor? x) (> x 0))
  (match n
    [(? mayor?) 1]
    [(? zero?) 0]
    [_ -1]))

(sign -1)
(sign 0)
(sign 1)

'(c)
;; picky-foo ::  List[Number] -> Number
(define (picky-foo l)
  (match l
    [(? (λ (x) (zero? (modulo (length x)  2)))) (foldl (λ (x e) (+ x e)) 0 l)]
    [_ 0]))
(picky-foo '(1 2 3))
(picky-foo '(1 2 3 4))


'(P3)


