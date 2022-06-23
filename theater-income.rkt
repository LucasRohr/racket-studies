;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname theater-income) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; total-viewers : Number → Number
;; Objective: given the ticket price, it returns the viewers total after base values

;; Examples:

;; (total-viewers 5.00) = 120
;; (total-viewers 5.10) = 100
;; (total-viewers 4.90) = 140
;; (total-viewers 4.80) = 160

(define (total-viewers price)
  ;; 5 is the base ticket price, with an increase of 20 viewers when there
  ;; is a 0.1 decrease of ticket price and a 20 viwers decrease with a 0.1 increase
  (+ 120 ;; 120 is the base viwers audience for the base ticket price
     (* 200 (- 5 price)) 
  )
)

;; Tests:

(check-expect (total-viewers 5.00) 120)
(check-expect (total-viewers 5.10) 100)
(check-expect (total-viewers 4.90) 140)
(check-expect (total-viewers 4.80) 160)

;; ========================

;; cost : Number → Number
;; Objective: given the ticket price, it returns the total cost of the show based on the total viewers and the price

;; Examples:

;; (cost 5.00) = 184.8
;; (cost 5.10) = 154
;; (cost 4.90) = 215.6
;; (cost 4.80) = 246.4

(define (cost price)
  (* (total-viewers price) 1.50) ;; Each viewer takes 1.50 of cost to the show, having to multiply it
)

;; Tests:

(check-expect (cost 5.00) 184.8)
(check-expect (cost 5.10) 154)
(check-expect (cost 4.90) 215.6)
(check-expect (cost 4.80) 246.4)

;; ========================

;; revenue : Number → Number
;; Objective: given the ticket price, it returns the revenue for the show after the total viewers number

;; Examples:

;; (revenue 5.00) = 600
;; (revenue 5.10) = 510
;; (revenue 4.90) = 686
;; (revenue 4.80) = 768

(define (revenue price)
  (* (total-viewers price) price)
)

;; Tests:

(check-expect (revenue 5.00) 600)
(check-expect (revenue 5.10) 510)
(check-expect (revenue 4.90) 686)
(check-expect (revenue 4.80) 768)

;; ========================

;; theater-income : Number → Number
;; Objective: given the ticket price, it returns the total income using the revenue and cost functions

;; Examples:

;; (theater-income 5.00) = 415.2
;; (theater-income 5.10) = 356
;; (theater-income 4.90) = 470.4
;; (theater-income 4.80) = 521.6

(define (theater-income ticket-price)
  (- (revenue ticket-price) (cost ticket-price))
)

;; Tests:

(check-expect (theater-income 5.00) 415.2)
(check-expect (theater-income 5.10) 356)
(check-expect (theater-income 4.90) 470.4)
(check-expect (theater-income 4.80) 521.6)

