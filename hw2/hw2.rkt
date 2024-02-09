#lang plait

(define-type Exp
  (numE [n : Number])
  (boolE [b : Number])
  (idE [s : Symbol])
  (plusE [l : Exp]
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (greaterE [l : Exp]
            [r : Exp])
  (appE [s : Symbol]
        [args : (Listof Exp)]))

(define-type Func-Defn
  (fd [name : Symbol]
      [args : (Listof Symbol)]
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [else
     (let ([s-list (s-exp->list s)])
       (cond
         [(s-exp-match? `{+ ANY ANY} s)
          (plusE (parse (second s-list))
                 (parse (third s-list)))]
         [(s-exp-match? `{* ANY ANY} s)
          (multE (parse (second s-list))
                 (parse (third s-list)))]
         [(s-exp-match? `{> ANY ANY} s)
          (greaterE (parse (second s-list))
                    (parse (third s-list)))]
         [(s-exp-match? `{SYMBOL ANY ...} s)
          (appE (s-exp->symbol (first s-list))
                (parse-helper (list->s-exp (rest s-list)))
                )]
         [else (error 'parse "invalid input")]))]))

(define (parse-helper [s : S-Exp]): (Listof Exp)
  (let ([s-list (s-exp->list s)])
    (cond [(empty? s-list) '()]
          [else (cons (parse(first s-list))
                      (parse-helper (list->s-exp (rest s-list))))])))


(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s)))))
         (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{double 9})
        (appE 'double (list (numE 9))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double '(x) (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(boolE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(greaterE l r) (cond [(> (interp l defs) (interp r defs)) 1]
                          [else 0])]
    [(appE s args) (local [(define fd (get-fundef s defs))]
                     (interp (interp-helper args (fd-body fd) (fd-args fd) defs) defs))]))

;; Use a helper function to recursively apply substitution to each one of the args
;; pseudo code
;; func = Exp
;; for i in args
;;   func = subst(i, val(i), func)

(define (interp-helper [args : (Listof Exp)] [fdbody : Exp] [fdargs : (Listof Symbol)] [defs : (Listof Func-Defn)]) : Exp
  (cond [(empty? args) fdbody]
        [else (let ([arg (first args)])
                (interp-helper
                 (rest args)
                 (subst (numE (interp arg defs))
                        (first fdargs)
                        fdbody)
                 (rest fdargs)
                 defs
                 ))]
        ))

(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32))

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : Exp] [for : Symbol] [in : Exp]) : Exp
  (type-case Exp in
    [(numE n) in]
    [(boolE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(greaterE l r) (greaterE (subst what for l)
                              (subst what for r))]
    [(appE s args) (appE s (map (lambda (arg) (subst what for arg)) args))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8})))

;; max ----------------------------------------
(define max-def
  (parse-fundef `{define {max x y} {+ {* {> x y} x} {* {> y x} y}}}))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  ;; (test (parse `{max 3 4})
  ;;       (maxE (numE 3) (numE 4)))
  (test (interp (parse `{max 1 2})
                (list max-def)) 2)
  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                (list max-def)) 9)
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{double 9})
        (appE 'double (list (numE 9))))
  (test (parse `{area 3 4})
        (appE 'area (list (numE 3) (numE 4))))
  (test (parse `{five})
        (appE 'five (list)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))
(module+ test
  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  (test (parse-fundef `{define {area w h} {* w h}})
        (fd 'area (list 'w 'h) (multE (idE 'w) (idE 'h))))
  (test (parse-fundef `{define {five} 5})
        (fd 'five (list) (numE 5)))
  (test/exn (parse-fundef `{define {f x x} x})
            "bad syntax")
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")
  )
(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{max 1 2}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                empty)
        9)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)

  (test/exn (interp (parse `{double})
                    (list double-def))
            "wrong arity"))


(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))


(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{max y x}))
        (parse `{max y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8}))
  (test (subst (parse `8) 'x (parse `{area x y}))
        (parse `{area 8 y})))
