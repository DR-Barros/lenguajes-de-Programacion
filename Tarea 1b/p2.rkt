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
 
<type>   ::= Num | Bool | {Pair <type> <type>}
|#


(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))



;; typecheck-expr :: ...
(define (typecheck-expr e)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    ; ...
    [_ (error "not yet implemented")]
    ))

;; typecheck-fundef :: ...
(define (typecheck-fundef f)
  ; ...
  (error "not yet implemented"))

;; typecheck :: ...
(define (typecheck prog)
  (error "not yet implemented"))

