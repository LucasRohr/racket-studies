(require racket/format)

;; ================== Number 1 ==================

;; Objective: constant to store a car drawing

(define CARRO
  (above
     (rectangle 15 10 "solid" "red")
     (overlay/offset
        (circle 5 "solid" "white")
        8 -2
        (overlay/offset
           (circle 5 "solid" "white")
           -8 -5
           (rectangle 30 9 "solid" "red")
        )
     )
  )
)

;; ====================================


;; ================== Number 2 ==================

;; render-star-grade: Number => Image
;; Objective: given the grade of a student, it returns it rendered inside a yellow star

(define (render-star-grade average)
  (overlay
     (text (~r average) 15 "black")
     (star 45 "solid" "yellow")
  )
)

;; Examples:
;; (render-star-grade 8.7) =  (overlay
;;     (text (~r 8.7) 15 "black")
;;     (star 45 "solid" "yellow")
;;  )
;; (render-star-grade 9) =  (overlay
;;     (text (~r 9) 15 "black")
;;     (star 45 "solid" "yellow")
;;  )

(check-expect (render-star-grade 8.7)
      (overlay
         (text (~r 8.7) 15 "black")
         (star 45 "solid" "yellow")
      )
)

(check-expect (render-star-grade 9)
      (overlay
         (text (~r 9) 15 "black")
         (star 45 "solid" "yellow")
      )
)

;; ====================================

;; render-red-grade Number => Image
;; Objective: given a student grade, it renders the value inside a red circle

;; Examples:
;; (render-red-grade 8.5) =
;; (overlay
;;     (text (~r 8.5) 18 "white")
;;     (circle 45 "solid" "red")
;;  )

(define (render-red-grade average)
  (overlay
     (text (~r average) 18 "white")
     (circle 45 "solid" "red")
  )
)

(check-expect (render-red-grade 8.5)
     (overlay
       (text (~r 8.5) 18 "white")
       (circle 45 "solid" "red")
     )
)

;; ================== Number 3 ==================

;; student-average: Number, Number, Number => Number
;; Objective: given three grades of a student, it returns the weighted average of them by trimester

;; Examples:
;; (student-average 7 9 8)
;; (student-average 7 5 4)

(define (student-average first_grade second_grade third_grade)
  (/ (+ (* first_grade 1) (* second_grade 2) (* third_grade 3)) 6)
)

(check-expect (student-average 7 9 8) 8.16)
(check-expect (student-average 7 5 4) 4.83)


;; student-average-render: Number, Number, Number => Image
;; Objective: given three grades of a student, it returns the weighted average of them by trimester inside an image

(define (student-average-render first_grade second_grade third_grade)
  (cond
    [(>= (student-average first_grade second_grade third_grade) 7) (render-star-grade (student-average first_grade second_grade third_grade))] 
    [else (render-red-grade (student-average first_grade second_grade third_grade))]
  )
)

;; Examples:
;; (student-average-render 7 9 8) = (render-star-grade (student-average 7 9 8))
;; (student-average-render 7 5 4) = (render-red-grade (student-average 7 5 4))

(check-expect (student-average-render 7 9 8) (render-star-grade (student-average 7 9 8)))
(check-expect (student-average-render 7 5 4) (render-red-grade (student-average 7 5 4)))

;; ====================================

;; ================== Number 4 ==================

(define SEGMENTO
   (rectangle 30 30 "outline" "white")
)

;; renderiza-segmento: Number, Number -> Image
;; Objective: given the segment and car position, show the car in the position or an empty position

;; Examples:
;; (renderiza-segmento 1 2) = SEGMENTO
;; (renderiza-segmento 5 5) = (overlay CARRO SEGMENTO)

(define (renderiza-segmento numero-segmento posicao-carro)
   (cond
      [
       (= numero-segmento posicao-carro)
       (overlay CARRO SEGMENTO)
      ]
      [else SEGMENTO]
   )
)

(check-expect (renderiza-segmento 1 2) SEGMENTO)
(check-expect (renderiza-segmento 5 5) (overlay CARRO SEGMENTO))

;; calcula-posicao-final: Number, Number, Number -> Number
;; Objective: given the current car position, driving direction and positions to move, returns the final position of the car rescpecting the road limits

;; Examples:
;; (calcula-posicao-final 2 "E" 10) = 1
;; (calcula-posicao-final 2 "D" 8) = 10

(define (calcula-posicao-final posicao-atual sentido-deslocamento numero-segmentos)
   (cond
      [
       (string=? sentido-deslocamento "E" )
       (cond
         [(>= (- posicao-atual numero-segmentos) 1) (- posicao-atual numero-segmentos)]
         [else 1]
       )
      ]
      [else
         (cond
           [(<= (+ posicao-atual numero-segmentos) 10) (+ posicao-atual numero-segmentos)]
           [else 10]
         )
      ]
   )
)

(check-expect (calcula-posicao-final 2 "E" 10) 1)
(check-expect (calcula-posicao-final 2 "D" 8) 10)

;; calcula-posicao-final: Number -> Image
;; Objective: given the current car position, renders 10 segments besides each other with the car rendered in its position

;; Examples:
;; (renderiza-trilho 3) =
;; (beside
;;     (renderiza-segmento 1 3)
;;     (renderiza-segmento 2 3)
;;     (renderiza-segmento 3 3)
;;     (renderiza-segmento 4 3)
;;     (renderiza-segmento 5 3)
;;     (renderiza-segmento 6 3)
;;     (renderiza-segmento 7 3)
;;     (renderiza-segmento 8 3)
;;     (renderiza-segmento 9 3)
;;     (renderiza-segmento 10 3)
;;  )

(define (renderiza-trilho posicao-carro)
  (beside
     (renderiza-segmento 1 posicao-carro)
     (renderiza-segmento 2 posicao-carro)
     (renderiza-segmento 3 posicao-carro)
     (renderiza-segmento 4 posicao-carro)
     (renderiza-segmento 5 posicao-carro)
     (renderiza-segmento 6 posicao-carro)
     (renderiza-segmento 7 posicao-carro)
     (renderiza-segmento 8 posicao-carro)
     (renderiza-segmento 9 posicao-carro)
     (renderiza-segmento 10 posicao-carro)
  )
)

(check-expect (renderiza-trilho 3)
  (beside
     (renderiza-segmento 1 3)
     (renderiza-segmento 2 3)
     (renderiza-segmento 3 3)
     (renderiza-segmento 4 3)
     (renderiza-segmento 5 3)
     (renderiza-segmento 6 3)
     (renderiza-segmento 7 3)
     (renderiza-segmento 8 3)
     (renderiza-segmento 9 3)
     (renderiza-segmento 10 3)
  )
)

;; move-no-trilho: Number, Number, Number -> Image
;; Objective: given the current car position, driving direction and positions to move, renders the initial road and the road after the car was moved

;; Examples:
;; (move-no-trilho 2 "D" 8) =
;;  (beside
;;     (renderiza-trilho 2)
;;     (rectangle 20 20 "solid" "black")
;;     (renderiza-trilho (calcula-posicao-final 2 "D" 8))
;;   )
;; (move-no-trilho 2 "E" 8) =
;;  (beside
;;     (renderiza-trilho 2)
;;     (rectangle 20 20 "solid" "black")
;;     (renderiza-trilho 2 "D" 8)
;;   )

(define (move-no-trilho posicao-atual sentido-deslocamento numero-posicoes)
  (beside
     (renderiza-trilho posicao-atual)
     (rectangle 20 20 "solid" "black")
     (renderiza-trilho (calcula-posicao-final posicao-atual sentido-deslocamento numero-posicoes))
   )
)

(check-expect (move-no-trilho  2 "D" 8)
     (beside
       (renderiza-trilho 2)
       (rectangle 20 20 "solid" "black")
       (renderiza-trilho (calcula-posicao-final 2 "D" 8))
     )
)

(check-expect (move-no-trilho  2 "E" 8)
     (beside
       (renderiza-trilho 2)
       (rectangle 20 20 "solid" "black")
       (renderiza-trilho (calcula-posicao-final 2 "E" 8))
     )
)

