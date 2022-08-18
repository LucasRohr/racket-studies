;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname scenes) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ======= Cenas =======

(define (casa cor)
  (above
    (isosceles-triangle 25 120 "solid" "orangered")
    (rectangle 40 20 "solid" cor)
  )
)

(define NUVEM
(ellipse 140 50 "solid" "cyan"))

(define SOL
(circle 30 "solid" "yellow"))

(define UFO
(overlay
(circle 8 "solid" "dimgray")
(ellipse 40 5 "solid" "dimgray")))

(define CASA1 (casa "lawngreen"))
(define CASA2 (casa "gold"))
(define CASA3 (casa "pink"))

(define CENA-VAZIA (empty-scene 500 400))

(place-image SOL 400 50 CENA-VAZIA)

(define (insereCasas-pos cena num-casas coord-x coord-y)
  (cond
    [(= num-casas 0) (place-image empty-image 0 0 cena)]
    [else (insereCasas-pos
            (place-image (casa "gold") coord-x coord-y cena)
            (- num-casas 1) (+ coord-x 50) coord-y)
    ]
  )
)

(define (insereCasas cena num-casas)
  (cond
    [(= num-casas 0) empty-image]
    [else (insereCasas-pos cena num-casas 100 300)]
  )
)

(define CIDADE10 (insereCasas CENA-VAZIA 3))

(define (calcula-distância tempo)
  (* tempo 5)
)

(define ALTURA_UFO 150)

(define (desenha-UFO tempo)
  (place-image UFO (calcula-distância tempo) ALTURA_UFO CIDADE10)
)

(desenha-UFO 30)

(animate desenha-UFO)