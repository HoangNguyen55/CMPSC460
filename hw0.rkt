#lang plait

#|PROBLEM 1
returns a list containing n copies of x|#
(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (- n 1) x))))

(test (duple 2 3)
      '(3 3))
(test (duple 4 '(ha ha))
      '((ha ha) (ha ha) (ha ha) (ha ha)))
(test (duple 0 '(word))
      '())

#|PROBLEM 2
return a sorted list of integers based on operator
less than < ascending
greater than > descending
> |#
(define (merge [op : (Number Number -> Boolean)]
               [int-list1 : (Listof Number)]
               [int-list2 : (Listof Number)]) : (Listof Number)
  (cond
    [(empty? int-list1) int-list2]
    [(empty? int-list2) int-list1]
    [(op (first int-list1) (first int-list2))
     (cons (first int-list1) (merge op (rest int-list1) int-list2))]
    [else
     (cons (first int-list2) (merge op int-list1 (rest int-list2)))]
    ))


(test (merge < '(1 4 6) '(2 5 8))
      '(1 2 4 5 6 8))
(test (merge > '(6 4 1) '(8 5 2))
      '(8 6 5 4 2 1))

#|Problem 3
return an association list from a list of symbols and a list of numbers
define a type to allow for the output of a list of associations
Assoc is to be replaced with your appropriately named type   |#
(define-type Association
  (Assoc [key : Symbol]
         [value : Number]))

(define (make-assoc [names : (Listof Symbol)] [values : (Listof Number)]): (Listof Association)
  (cond [(or (empty? names) (empty? values)) '()]
        [else (cons
               (Assoc (first names) (first values))
               (make-assoc (rest names) (rest values))
               )]
        )
  )

#| Broken testing, you can't use `'()` when creating a list that isn't literal values.
https://docs.racket-lang.org/plait/lists-tutorial.html
|#

;; (test (make-assoc '(a b c d) '(1 2 3 4))
;;       '((Assoc 'a 1) (Assoc 'b 2) (Assoc 'c 3) (Assoc 'd 4)))
;; (test (make-assoc '(t a c o tuesday) '(0 1 34 1729 42))
;;       '((Assoc 't 0) (Assoc 'a 1) (Assoc 'c 34) (Assoc 'o 1729) (Assoc 'tuesday 42)))

;; Fixed testing
(test (make-assoc '(a b c d) '(1 2 3 4))
      (list (Assoc 'a 1) (Assoc 'b 2) (Assoc 'c 3) (Assoc 'd 4)))
(test (make-assoc '(t a c o tuesday) '(0 1 34 1729 42))
      (list (Assoc 't 0) (Assoc 'a 1) (Assoc 'c 34) (Assoc 'o 1729) (Assoc 'tuesday 42)))
