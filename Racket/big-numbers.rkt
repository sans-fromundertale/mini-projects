#lang racket

;; Bigit list data definition:

;; empty if n = 0
;; (cons b bl) if n > 0
;;   n = b + 10000k where k = bl

;; A Bigit is a number from 0 to 9999.
;; They can be used to handle arbitrarily large numbers with finitely large integers.



(define (nat-to-blist nat)
  (cond
    [(zero? nat) empty]
    [else (cons (modulo nat 10000) (nat-to-blist (floor (/ nat 10000))))]))

;(nat-to-blist 0)
;(nat-to-blist 9999)
;(nat-to-blist 12345678)
;(nat-to-blist 18446744073709551616)

(define (blist-to-nat blist)
  (cond
    [(empty? blist) 0]
    [else (+ (first blist) (* 10000 (blist-to-nat (rest blist))))]))

;(blist-to-nat (nat-to-blist 0))
;(blist-to-nat (nat-to-blist 9999))
;(blist-to-nat (nat-to-blist 12345678))
;(blist-to-nat (nat-to-blist 18446744073709551616))


(define (add blist1 blist2)
  (add/carry blist1 blist2 0))

;; Time analysis:
;; add/carry is O(x), therefore this is O(x)

(define (add/carry blist1 blist2 carry)
  (cond
    [(empty? blist1) (add-carry-to-first blist2 carry)]
    [(empty? blist2) (add-carry-to-first blist1 carry)] 
    [(> (+ (first blist1) (first blist2)) 9999) (cons (+ (first blist1) (first blist2) -10000 carry) (add/carry (rest blist1) (rest blist2) 1))]
    [else (cons (+ (first blist1) (first blist2) carry) (add/carry (rest blist1) (rest blist2) 0))]))

;; Time analysis:
;; +, which would be O(x), is O(1) because the input size is capped at 9999
;; recursive function call - O(x)


(define (add-carry-to-first blist carry)
  (cond [(not (empty? blist)) (cons (+ (first blist) carry) (rest blist))]
        [(zero? carry) blist]
        [else (list carry)]))

;; Time analysis:
;; O(1) time

(add (nat-to-blist 1) (nat-to-blist 9999))
(add (nat-to-blist 100032421952) (nat-to-blist 128491294919240))
(add empty empty)
(add (nat-to-blist 45678) (nat-to-blist 45678))

(define (mult blist1 blist2)
  (cond
    [(or (empty? blist1) (empty? blist2)) empty]
    [(empty? (rest blist1)) (mult/carry (first blist1) blist2 0)]
    [else (add (cons 0 (mult (rest blist1) blist2)) (mult/carry (first blist1) blist2 0))])) ;; I think this line is x^2 time

;; Time analysis:
;; (add x y) - linear time
;; (mult/carry nat blist carry) - linear time
;; (mult (rest blist1) blist2) - recursive function call means everything runs x times
;; Therefore, O(x^2), because we are calling only O(x) and O(1) things x times



(define (mult/carry nat blist carry)
  (cond
    [(empty? blist) (if (zero? carry) empty (list carry))]
    [else (cons (+ carry (modulo (* (first blist) nat) 10000)) (mult/carry nat (rest blist) (quotient (* (first blist) nat) 10000)))]))

;; Time analysis:
;; all functions (such as modulo, +, etc) that would be O(x) are O(1) because the input size is capped at 9999
;; recursive function call - O(x)
                            
;;(mult (list 100) (list 100))
;;(mult (list) (list))
;;(mult (list 1) (list 9999 9999 9999 9999))
;;(mult (list 0 1) (list 9999 9999 9999 9999))
;;(mult (list 0 9999) (list 9999 9999 9999 9999))

