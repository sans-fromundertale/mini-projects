#lang racket




(define (in->pre infix-expr)
  (cond
    [(or (number? infix-expr) (and (symbol? infix-expr) (not (symbol=? '+ infix-expr)) (not (symbol=? '* infix-expr)))) infix-expr]
    [(cons? infix-expr) (cond
                          [(= (length infix-expr) 1) (in->pre (first infix-expr))]
                          [(= (modulo (length infix-expr) 2) 0) (error "bad expression")]
                          [else (in->pre/lst infix-expr (second infix-expr) empty)])]
    [else (error "bad expression")]))

(define (in->pre/lst inlst op acc)
  (cond
    [(= (length inlst) 1) (cons op (reverse (cons (in->pre (first inlst)) acc)))]
    [(or (not (symbol? (second inlst))) (not (or (symbol=? (second inlst) '+) (symbol=? (second inlst) '*)))) (error "bad expression")]
    [(and (symbol=? op '*) (symbol=? (second inlst) '+)) (in->pre/lst (rest (rest inlst)) '+ (cons (cons op (reverse (cons (in->pre (first inlst)) acc))) empty))]
    [(and (symbol=? op '+) (symbol=? (second inlst) '*))
     (local [(define prefix-rest (in->pre/lst inlst '* empty))]
     (if (symbol=? (first prefix-rest) '+) (append (cons '+ (reverse acc)) (rest (in->pre/lst inlst '* empty)))
         (cons op (reverse (cons (in->pre/lst inlst '* empty) acc)))))]
    [else (in->pre/lst (rest (rest inlst)) (second inlst) (cons (in->pre (first inlst)) acc))]))

;(in->pre '*)
;(in->pre '(1 +))
;(in->pre '(+ 1))
;(in->pre '(1 1 1))
;(in->pre '(+ + +))
;(in->pre '(!))
;(in->pre '(2 / 1))

(in->pre '(1 + 2 + 3))
(in->pre '(1 * 2 + 3 + 4))
(in->pre '(1 + 2 * 3 + 4))
(in->pre '(1 + 2 * 3 * 4))
(in->pre '(1 * 2 + 3 * 4 * 5 + 6 + 7 * 8))
(in->pre '((1 + 2 + 3 * 4) * 5 + 6 + 7 * 8))
(in->pre '((2)))



