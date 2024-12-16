#lang play
(require "T1.rkt")
(require math/flonum)

(print-only-errors #t)

(define small-compound
  (compound 2 10 (simple 5)))

(define nested-compound
  (compound -27 51 (compound 0 -102 small-compound)))

(define rational-compound
  (compound 3 1 (compound 1 1 (compound 1 1 (compound 1 1 (simple 4))))))

(define null-compound
  (compound 4 28 (compound 6 -65 (simple 5))))

(define invalid-compound
  (compound 38 9 null-compound))


(test (eval (simple 5)) 5)
(test (eval (simple 0)) 0)
(test (eval (simple -218)) -218)
(test (eval small-compound) 4)
(test (eval nested-compound) -29)
(test (eval rational-compound) 51/14)
(test (eval null-compound) 0)

(test (degree small-compound) 1)
(test (degree nested-compound) 3)
(test (degree rational-compound) 4)
(test (degree null-compound) 2)
(test (degree invalid-compound) 3)

(define even-coeficients?
  (fold-cfraction even? (lambda (x y A) (and (even? x) (even? y) A))))

(define sum
  (fold-cfraction identity +))

(test (even-coeficients? (simple 1024)) #t)
(test (even-coeficients? (simple -3)) #f)
(test (even-coeficients? (compound 4 0 (compound -78 12 (simple -0)))) #t)
(test (even-coeficients? nested-compound) #f)
(test (even-coeficients? rational-compound) #f)

(test (sum small-compound) 17)
(test (sum nested-compound) -61)
(test (sum rational-compound) 14)
(test (sum (simple 1293123)) 1293123)


(test (eval2 (simple 5)) 5)
(test (eval2 (simple 0)) 0)
(test (eval2 (simple -218)) -218)
(test (eval2 small-compound) 4)
(test (eval2 nested-compound) -29)
(test (eval2 rational-compound) 51/14)
(test (eval2 null-compound) 0)

(test (degree2 small-compound) 1)
(test (degree2 nested-compound) 3)
(test (degree2 rational-compound) 4)
(test (degree2 null-compound) 2)
(test (degree2 invalid-compound) 3)


(test (mysterious-cf 0) (simple 6))
(test (mysterious-cf 1) (compound 6 (sqr 1) (simple 6)))
(test (mysterious-cf 2) (compound 6 (sqr 1) (compound 6 (sqr 3) (simple 6))))
(test (mysterious-cf 3) (compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5) (simple 6)))))
(test (mysterious-cf 4) (compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5) (compound 6 (sqr 7) (simple 6))))))
(test/exn (mysterious-cf -1) "Error: argumento negativo")


(test (from-to 0 5) '(0 1 2 3 4))
(test (from-to 5 10) '(5 6 7 8 9))
(test (from-to -123 -117) '(-123 -122 -121 -120 -119 -118))
(test (from-to -3 3) '(-3 -2 -1 0 1 2))
(test (from-to 15 10) empty)
(test (from-to 5 -10) empty)
(test (from-to -40 -50) empty)


(test (mysterious-list-rational 3) '(3 19/6 47/15))
(test (mysterious-list-rational 4) '(3 19/6 47/15 1321/420))
(test (andmap rational? (mysterious-list 20)) #t)
(test (andmap flrational? (mysterious-list 20)) #t)
(test (mysterious-list-rational 4) '(3 19/6 47/15 1321/420))

(define mysterious-number
  (list-ref (mysterious-list 200) 199))

(test (< (abs (- mysterious-number pi)) 0.0001) #t)


(test (rac-to-cf 649/200) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(test (rac-to-cf 21/13) (compound 1 1 (compound 1 1 (compound 1 1 (compound 1 1 (compound 1 1 (simple 2)))))))
(test (rac-to-cf 5) (simple 5))
(test (rac-to-cf -1023) (simple -1023))
(test (rac-to-cf 51/14) rational-compound)



