#lang plait

(define-type Tree
  (leaf [val : Number])
  (node [val : Number]
        [left : Tree]
        [right : Tree]))

(define (sum [tree : Tree]) : Number
  (cond [(leaf? tree) (leaf-val tree)]
        [else (+ (+ (node-val tree)
                    (sum (node-left tree)))
                 (sum (node-right tree)))]))

(test (sum (node 5 (leaf 6) (leaf 7))) 18)

(define (negate [tree : Tree]) : Tree
  (cond [(leaf? tree) (leaf (* -1 (leaf-val tree)))]
        [else (node (* -1 (node-val tree)) (negate (node-left tree)) (negate (node-right tree)))]))

(test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))

(define (contains? [tree : Tree] [num : Number]) : Boolean
  (cond [(leaf? tree) (= num (leaf-val tree))]
        [else (or (= num (node-val tree))
                  (contains? (node-left tree) num)
                  (contains? (node-right tree) num))]
        ))

(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)

(define (bigger-leaves? [tree : Tree] [accum : Number]): Boolean
  (cond [(leaf? tree) (> (leaf-val tree) accum)]
        [else (and (bigger-leaves? (node-left tree) (+ accum (node-val tree)))
                   (bigger-leaves? (node-right tree) (+ accum (node-val tree)))
                   )]))

(define (big-leaves? [tree : Tree]): Boolean
  (bigger-leaves? tree 0))

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)

(define (positive-trees? [trees : (Listof Tree) ]): Boolean
  (cond [(empty? trees) #t]
        [else (and (> (sum (first trees)) 0)
                   (positive-trees? (rest trees))
                   )]))

(test (positive-trees? (cons (leaf 6)
                             empty))

      #t)
(test (positive-trees? (cons (leaf -6)
                             empty))
      #f)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                             empty))
      #t)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                             (cons (node 0 (leaf 0) (leaf 1))
                                   empty)))
      #t)

(test (positive-trees? (cons (node -1 (leaf 6) (leaf -6))
                             (cons (node 0 (leaf 0) (leaf 1))
                                   empty)))
      #f)

(define (flatten-helper [tree : Tree] [acc : (Listof Number)]): (Listof Number)
  (cond [(leaf? tree) (cons (leaf-val tree) acc)]
        [else (flatten-helper (node-right tree)
                              (flatten-helper (node-left tree)
                                              (cons (node-val tree) acc)))]
        ))

(define (flatten [tree : Tree]): (Listof Number)
  (reverse (flatten-helper tree '())))

(test (flatten (node 5 (node 4 (leaf 1) (leaf 9)) (leaf 7))) '(5 4 1 9 7))
(test (flatten (node 5 (leaf 4) (node 1 (leaf 9) (leaf 7)))) '(5 4 1 9 7))

; positive thinking is too much of a pain so im not doing it
