;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname numbers) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Function for showing a number inside a circle

(define (circle_number number)
  (overlay
     (text (number->string number) 32 "white")
     (circle 45 "solid" "red")
  )
)

(circle_number 12)

; Function for calculation of a bakery shopping list total price

(define (bakery_products_price bread_quantity cake_quantity croissant_quantity)
  (circle_number
     (+
       (* bread_quantity 2)
       (* cake_quantity 3)
       (* croissant_quantity 4)
     )
  )
)

(bakery_products_price 4 2 3)