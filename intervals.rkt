;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname intervals) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define (first_interval value)
  (and (> value 3) (<= value 7))
)

(first_interval 5)
(first_interval 3)

;; ===========================

(define (second_interval value)
  (and (>= value 3) (<= value 7))
)

(second_interval 5)
(second_interval 3)

;; ===========================

(define (third_interval value)
  (and (>= value 3) (< value 9))
)

(third_interval 7.2)
(third_interval 11)

;; ===========================

(define (fourth_interval value)
  (or
   (and (> value 1) (< value 6))
   (and (> value 9) (< value 14))
  )
)

(fourth_interval 3)
(fourth_interval 11)
(fourth_interval 23)

;; ===========================

(define (fifth_interval value)
   (or (<= value 1) (>= value 3))
)

(fifth_interval -3)
(fifth_interval 2.5)
(fifth_interval 4)

