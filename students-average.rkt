(require racket/format)

;; student-average: Number, Number, Number => Number
;; Objective: given three grades of a student, it returns the weighted average of them by trimester

(define (student-average first_grade second_grade third_grade)
  (/ (+ (* first_grade 1) (* second_grade 2) (* third_grade 3)) 6)
)

;; Examples:
;; (student-average 7 9 8)
;; (student-average 7 5 4)

(check-expect (student-average 7 9 8) 8.16)
(check-expect (student-average 7 5 4) 4.83)

(define (render-star-grade average)
  (overlay
     (text (~r average) 15 "black")
     (star 45 "solid" "yellow")
  )
)

(define (render-red-grade average)
  (overlay
     (text (~r average) 18 "white")
     (circle 45 "solid" "red")
  )
)

;; student-average-render: Number, Number, Number => Image
;; Objective: given three grades of a student, it returns the weighted average of them by trimester inside an image

(define (student-average-render first_grade second_grade third_grade)
  (cond
    [(>= (student-average first_grade second_grade third_grade) 7) (render-star-grade (student-average first_grade second_grade third_grade))] 
    [else (render-red-grade (student-average first_grade second_grade third_grade))]
  )
)

(student-average-render 7 9 8)

(student-average-render 7 5 4)