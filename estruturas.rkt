;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname estruturas) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; === prova ===

(define-struct prova (nota-1 nota-2))

;; Um elemento do conjunto prova tem o formato
;; (make-prova 5 9)

;; onde:
;; nota-1: Numero, representa a primeira nota
;; nota-2: Numero, representa a segunda nota


;; === nota-exercicio ===

(define-struct nota-exercicio (nota-codigo nota-doc))

;; Um elemento do conjunto nota-exercicio tem o formato
;; (make-nota-exercicio 8 7)

;; onde:
;; nota-codigo: Numero, representa a nota dos exercicios de codigo
;; nota-doc: Numero, representa a nota dos exercicios de documentacao


;; === exercicios ===

(define-struct exercicios (nota-exercicio-1 nota-exercicio-2 possui-ponto-extra))

;; Um elemento do conjunto exercicios tem o formato
;;   (make-exercicios(
;;     (make-nota-exercicio 8 7)
;;     (make-nota-exercicio 8 7)
;;     #false
;;   ))

;; onde:
;; nota-exercicio-1: nota-exercicio, representa a nota composta do primeiro exercicio
;; nota-exercicio-2: nota-exercicio, representa a nota composta do segundo exercicio
;; possui-ponto-extra: Boolean, representa se o aluno possui ponto extra ou nao


;; === aluno ===

(define-struct aluno (nome prova-1 prova-2 exercicios))

;; Um elemento do conjunto aluno tem o formato
;; (make-aluno
;;   "Jorge"
;;   (make-prova 1 10)
;;   (make-prova 5 9)
;;   (make-exercicios
;;     make-nota-exercicio(8 7)
;;     make-nota-exercicio(8 7)
;;     #false
;;   )
;; )

;; onde:
;; nome: String, representa o nome do aluno
;; prova-1: prova, representa a primeira prova
;; prova-2: prova, representa a segunda prova
;; exercicio: exercicio, representa os exercicios do aluno e se tem ponto extra


;; === Exercicios ===


;; (a) Dado um aluno, devolve seu nome.

;;retorna-nome-aluno: aluno -> string
;; Objetivo: dado uma estrutura aluno, devolve seu nome string

;; Exemplo:
;; (retorna-nome-aluno
;;   (make-aluno
;;     "Jorge"
;;     (make-prova 1 10)
;;     (make-prova 5 9)
;;     (make-exercicios
;;       (make-nota-exercicio(8 7))
;;       (make-nota-exercicio(8 7))
;;       #false
;;     )
;;   )
;; )

(define (retorna-nome-aluno aluno)
  (aluno-nome aluno)
)

;; Testes:
(check-expect (retorna-nome-aluno
                  (make-aluno
                    "Jorge"
                     (make-prova 1 10)
                     (make-prova 5 9)
                     (make-exercicios
                       (make-nota-exercicio 8 7)
                       (make-nota-exercicio 8 7)
                       #false
                     )
                   )
  ) "Jorge"
)


;; (b) Dada uma prova, soma as notas de suas questões.

;;retorna-total-prova prova -> Numero
;; Objetivo: dada uma estrutura prova, devolve a soma de suas notas

;; Exemplo:
;; (retorna-total-prova (make-prova 5 5) (make-prova 5 5))

(define (retorna-total-prova prova)
  (+ (prova-nota-1 prova) (prova-nota-2 prova))
)

;; Testes:
(check-expect (retorna-total-prova (make-prova 5 5)) 10)
(check-expect (retorna-total-prova (make-prova 4 2)) 6)


;; (c) Dado um aluno, devolve a média aritmética de suas provas.

;;retorna-media-provas: aluno -> Numero
;; Objetivo: dado um aluno, soma as notas de suas provas e divide por 2 para calcular a sua media aritmetica

;; Exemplo:
;; (retorna-media-provas
;;       (make-aluno
;;          "Jorge"
;;          (make-prova 5 4)
;;          (make-prova 4 3)
;;          (make-exercicios
;;             (make-nota-exercicio 8 7)
;;             (make-nota-exercicio 8 7)
;;              #false
;;          )
;;       )
;;)

(define (retorna-media-provas aluno)
  (/
    (+ (retorna-total-prova (aluno-prova-1 aluno)) (retorna-total-prova (aluno-prova-2 aluno)))
    2
  )
)

;; Testes:

(check-expect
  (retorna-media-provas
       (make-aluno
          "Jorge"
          (make-prova 5 4)
          (make-prova 4 4)
          (make-exercicios
             (make-nota-exercicio 8 7)
             (make-nota-exercicio 8 7)
              #false
          )
       )
  )
  8.5
)

