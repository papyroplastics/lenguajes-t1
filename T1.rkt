#lang play
(require math/flonum)

#|
Complete sus datos personales:
NOMBRE Y APELLIDO: Lucas Llort
RUT: 21.225.405-3
|#


;; Parte a)
#|
CFraction:
Representa una fracción continua finita a coeficientes enteros,
se expresa como la suma entre un coeficiente a y la división entre
un coeficiente b y una CFraction.

<Cfraction> ::= (simple <integer>)
              | (compound <integer> <integer> <Cfraction>)
|#
(deftype Cfraction
  (simple n)
  (compound a b d))


;; Parte b)
;; eval :: CFraction -> Rational
;; Evalúa una Cfraction, devolviendo el número racional que representa.
(define (eval frac)
  (match frac
    [(simple n) n]
    [(compound a b d) (+ a (/ b (eval d)))]))


;; Parte c)
;; degree ::  CFraction -> Integer
;; Retorna del grado de una Cfraction.
(define (degree frac)
  (match frac
    [(simple _) 0]
    [(compound _ _ d) (add1 (degree d))]))


;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Retorna una función recursiva de agregación sobre una Cfraction, el 
;; primer argumento es la función que se aplica sobre el caso base y 
;; el segundo es para el caso general.
(define (fold-cfraction base aggregate)
  (define (aggregate-function frac)
    (match frac
      [(simple n) (base n)]
      [(compound a c d) (aggregate a c (aggregate-function d))]))

  aggregate-function)

;; Parte e)
;; eval2 :: CFraction -> Rational
;; Evalúa una Cfraction, devolviendo el número racional que representa.
(define eval2
  (fold-cfraction identity (lambda (x y A) (+ x (/ y A)))))


;; degree2 ::  CFraction -> Integer
;; Retorna del grado de una Cfraction.
(define degree2
  (fold-cfraction (lambda (n) 0) (lambda (x y A) (add1 A))))


;; Parte f)
;; mysterious-cf :: Integer -> CFraction
;; Genera una Cfraction en base a una secuencia misteriosa
(define (mysterious-cf n)

  ;; mysterious-cf-recurse :: Integer -> CFraction
  ;; función auxiliar para mysterious-cf
  (define (mysterious-cf-recurse m)
    (if (= m n)
      (simple 6)
      (compound 6 (sqr (add1 (* m 2))) (mysterious-cf-recurse (add1 m)))))

  (if (< n 0)
    (error "Error: argumento negativo")
    (mysterious-cf-recurse 0)))


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; Crea una lista ordenada con todos los enteros entre
;; l (inclusivo) y r (exclusivo)
(define (from-to l r)
  (if (>= l r)
    empty
    (cons l (from-to (add1 l) r))))


;; mysterious-list-rational :: Integer -> ListOf Rational
;; Función con proposito de testeo, retorna lo mismo que mysterious-list 
;; pero sin transformar los racionales exactos a float
(define (mysterious-list-rational n)
  (map 
    (lambda (i) (- (eval (mysterious-cf i)) 3))
    (from-to 0 n)))

;; mysterious-list :: Integer -> ListOf Float
;; Crea una lista de tamaño n que que en cada indice contiene la evaluació
;; de (mysterious-cf i) menos 3
(define (mysterious-list n)
  (map fl (mysterious-list-rational n)))


;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
;; Como se puede ver en uno de los tests, (mysterious-cf i) tiende 
;; a pi cuando i tiende a infinito


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; Retorna la representación en fracción continua de un numero racional exacto
(define (rac-to-cf x)
  (define int_part (floor x))
  (define frac_part (- x int_part))

  (if (zero? frac_part)
    (simple x)
    (compound int_part 1 (rac-to-cf (/ 1 frac_part)))))




