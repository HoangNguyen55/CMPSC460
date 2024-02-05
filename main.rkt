#lang plait

(define-type Exp
  (num [n : Number])
  (plus [left : Exp]
        [right : Exp])
  (mult [left : Exp]
        [right : Exp]))

(define prog `(+ (* 4 2) 7))

(define (parse s)
  (cond
    [(s-exp-number? s) (num (s-exp->number s))]
    [(s-exp-list? s) (0)]))
