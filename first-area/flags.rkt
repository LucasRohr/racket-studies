;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname flags) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; === Class flags construction exercises ===

; First Flag:

(define (cross color)
  (beside
     (rectangle 18 15 "solid" color)
     (rectangle 18 48 "solid" color)
     (rectangle 18 15 "solid" color)
   )
)

(overlay
 (cross "white")
 (rectangle 80 80 "solid" "crimson")
)

; Second Flag:

(define (bar color)
    (rectangle 75 15 "solid" color)
)

(beside
 (rectangle 18 45 "solid" "darkred")
 (above
    (bar "darkgreen")
    (bar "white")
    (bar "black")
 )
)

; Third Flag:

(define (flag_slice color)
    (rectangle 44 30 "solid" color)
)

(define (starred_flag_slice color)
    (overlay
       (star 10 "solid" color)
       (flag_slice "white")
    )
)

(above
 (beside
   (starred_flag_slice "royalblue")
   (flag_slice "crimson")
  )
 (beside
   (flag_slice "royalblue")
   (starred_flag_slice "crimson")
  )   
)

; Bar Flag function

(define (flag_bar color)
    (rectangle 15 30 "solid" color)
)

(define (flag color1 color2 color3)
    (beside
     (flag_bar color1)
     (flag_bar color2)
     (flag_bar color3)
   ) 
)

(flag "black" "gold" "red")
(flag "blue" "white" "red")

; Starred Bar Flag function

(define (big_flag_bar color)
    (rectangle 18 40 "solid" color)
)

(define (starred_flag_bar color star_color)
    (overlay
       (star 7 "solid" star_color)
       (big_flag_bar color)
    )
)

(define (starred_flag color1 color2 color3 star_color)
    (beside
     (big_flag_bar color1)
     (starred_flag_bar color2 star_color)
     (big_flag_bar color3)
   ) 
)

(starred_flag "darkgreen" "red" "yellow" "yellow")
(starred_flag "chocolate" "green" "darkblue" "darkblue")
