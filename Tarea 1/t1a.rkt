#lang play


(print-only-errors)
;=====================================
#|

Gramática BNF del lenguaje core:

<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr>   ::= <num>
           | <id>
           | <bool>
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {- <expr> <expr>}
           | {<id> <expr>*}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr>}
           | {if <expr> <expr> <expr>}
           | {with {<binding>*} <expr>}


<binding> ::= {<id> <expr>}

|#
;=====================================

#|
    Definiciones de los Tipos de Datos
|#

(deftype Fundef
  (fundef name args body))

(deftype Expr
  (num n)
  (id s)
  (bool b)
  (add1 e)
  (add l r)
  (sub l r)
  (app name args)
  (minor l r)
  (mayor l r)
  (minor-equal l r)
  (mayor-equal l r)
  (equal l r)
  (dist l r)
  (si cond true false)
  (con bindings e))

(deftype Binding
  (binding id expr))

(deftype Prog
  (prog funs body))

#|
    Parser
|#


; parse-fundef :: <s-expr> -> Fundef
; 
(define (parse-fundef src)
  (match src
    [(list 'define (list name) args) (fundef name '() (parse-expr args))]
    [(list 'define (list name args ...) cont) (fundef name args (parse-expr cont))]
    [else (error "parse fundef error")]))


; parse-expr :: <s-expr> -> Expr
; retorna un expr para el s-expr si es valido, si no retorna error
(define (parse-expr src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?) (bool src)]
    [(list 'add1 x) (add1 (parse-expr x))]
    [(list '+ l r) (add (parse-expr l) (parse-expr r))]
    [(list '- l r) (sub (parse-expr l) (parse-expr r))]
    [(list '< l r) (minor (parse-expr l) (parse-expr r))]
    [(list '> l r) (mayor (parse-expr l) (parse-expr r))]
    [(list '<= l r) (minor-equal (parse-expr l) (parse-expr r))]
    [(list '>= l r) (mayor-equal (parse-expr l) (parse-expr r))]
    [(list '= l r) (equal (parse-expr l) (parse-expr r))]
    [(list 'if cond t f) (si (parse-expr cond) (parse-expr t) (parse-expr f))]
    [(list 'con b e) (con (map parse-binding b) (parse-expr e))]
    [(list name) (app name '())]
    [(list name args ...) (app name (map parse-expr args))]
    [else (error "parse expr error")]))



; parse-prog :: <s-expr> -> Prog
; Parsea un programa desde una s-expr, retorna error si no es posible
(define (parse-prog src)
  (match src
    [(list x) (prog (list) (parse-expr x))]
    [(list f ... x) (prog (map parse-fundef f) (parse-expr x))]
    [else (error "parse prog error")]))


;; parse-binding :: <s-expr> -> Binding
;;
(define (parse-binding src)
  (match src
    [(list i e) (binding (id i) (parse-expr e))]
    [else (error "parse binding error")]))