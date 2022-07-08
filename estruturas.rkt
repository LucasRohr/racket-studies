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
;; ) = "Jorge"

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
;; (retorna-total-prova (make-prova 5 5)) = 10

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
;;) = 8.5

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

;; (d) Dados 2 alunos, devolve o aluno com melhor média aritmética das provas.

;;retorna-aluno-melhor-media: aluno, aluno -> aluno
;; Objetivo: dado dois alunos, calcula a media aritmetica de cada um e retorna aquele aluno com a maior media

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
;;) =
;;  (make-aluno
;;     "Miguel"
;;     (make-prova 5 5)
;;     (make-prova 4 5)
;;     (make-exercicios
;;        (make-nota-exercicio 8 7)
;;        (make-nota-exercicio 8 7)
;;        #false
;;     )
;;  )

(define (retorna-aluno-melhor-media aluno-1 aluno-2)
  (cond
     [(>= (retorna-media-provas aluno-1) (retorna-media-provas aluno-2)) aluno-1]
     [else aluno-2]
   )
)

;; Testes:

(check-expect
  (retorna-aluno-melhor-media
       (make-aluno
          "Jorge"
          (make-prova 5 4)
          (make-prova 3 2)
          (make-exercicios
             (make-nota-exercicio 8 7)
             (make-nota-exercicio 8 7)
              #false
          )
       )
       (make-aluno
          "Miguel"
          (make-prova 5 5)
          (make-prova 4 5)
          (make-exercicios
             (make-nota-exercicio 8 7)
             (make-nota-exercicio 8 7)
              #false
          )
       )
  )
  (make-aluno
     "Miguel"
     (make-prova 5 5)
     (make-prova 4 5)
     (make-exercicios
        (make-nota-exercicio 8 7)
        (make-nota-exercicio 8 7)
        #false
     )
  )
)


;; (e) Dados 2 alunos, devolve o nome do aluno com melhor média das provas.

;;retorna-nome-aluno-melhor-media: aluno, aluno -> String
;; Objetivo: dado dois alunos, calcula a media aritmetica de cada um e retorna o nome daquele aluno com a maior media

;; Exemplo:
;; (retorna-nome-aluno-melhor-media
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
;;       (make-aluno
;;          "Miguel"
;;          (make-prova 5 5)
;;          (make-prova 4 5)
;;          (make-exercicios
;;             (make-nota-exercicio 8 7)
;;             (make-nota-exercicio 8 7)
;;              #false
;;          )
;;       )
;;) = "Miguel"

(define (retorna-nome-aluno-melhor-media aluno-1 aluno-2)
  (aluno-nome (retorna-aluno-melhor-media aluno-1 aluno-2))
)

(check-expect
  (retorna-nome-aluno-melhor-media
       (make-aluno
          "Jorge"
          (make-prova 5 4)
          (make-prova 3 2)
          (make-exercicios
             (make-nota-exercicio 8 7)
             (make-nota-exercicio 8 7)
              #false
          )
       )
       (make-aluno
          "Miguel"
          (make-prova 5 5)
          (make-prova 4 5)
          (make-exercicios
             (make-nota-exercicio 8 7)
             (make-nota-exercicio 8 7)
              #false
          )
       )
  )
  "Miguel"
)

;; (f) Dado um registro de exercício, verifica se o aluno deve ganhar ponto extra: se tiver nota de
;; documentação maior ou igual a 3, nos 2 exercícios, ganha. O resultado deve ser o registro atualizado.


;; verifica-ponto-extra: exercicios -> Boolean
;; Objetivo: dado um registro de exercicios, verifica se o aluno possui ponto extra ou não e retorna o valor lógico

;; Exemplo:
;; (verifica-ponto-extra
;;   (make-exercicios(
;;     (make-nota-exercicio 8 7)
;;     (make-nota-exercicio 8 7)
;;     #false
;;   ))
;; ) = #true

(define (verifica-ponto-extra registro-exercicios)
   (and
      (>= (nota-exercicio-nota-doc (exercicios-nota-exercicio-1 registro-exercicios)) 3)
      (>= (nota-exercicio-nota-doc (exercicios-nota-exercicio-2 registro-exercicios)) 3)
   )
)

;; Testes:

(check-expect
   (verifica-ponto-extra
     (make-exercicios
       (make-nota-exercicio 4 4)
       (make-nota-exercicio 4 5)
       #false
     )
   )
   #true
)

(check-expect
   (verifica-ponto-extra
     (make-exercicios
       (make-nota-exercicio 4 2)
       (make-nota-exercicio 4 5)
       #false
     )
   )
   #false
)


;; atualiza-ponto-extra: exercicios -> exercicios
;; Objetivo: dado um registro de exercicios, verifica se o aluno possui ponto extra ou não e retorna o registrado atualizado

;; Exemplo:
;; (atualiza-ponto-extra
;;   (make-exercicios(
;;     (make-nota-exercicio 8 7)
;;     (make-nota-exercicio 8 7)
;;     #false
;;   ))
;;) =
;;   (make-exercicios
;;     (make-nota-exercicio 4 4)
;;     (make-nota-exercicio 4 5)
;;     #true
;;   ) 

(define (atualiza-ponto-extra registro-exercicios)
  (make-exercicios
     (exercicios-nota-exercicio-1 registro-exercicios)
     (exercicios-nota-exercicio-2 registro-exercicios)
     (verifica-ponto-extra registro-exercicios)
  )
)

;; Testes:

(check-expect
   (verifica-ponto-extra
     (make-exercicios
       (make-nota-exercicio 4 4)
       (make-nota-exercicio 4 5)
       #false
     )
   )
   (make-exercicios
     (make-nota-exercicio 4 4)
     (make-nota-exercicio 4 5)
     #true
   ) 
)

(check-expect
   (verifica-ponto-extra
     (make-exercicios
       (make-nota-exercicio 4 2)
       (make-nota-exercicio 4 5)
       #false
     )
   )
   (make-exercicios
     (make-nota-exercicio 4 2)
     (make-nota-exercicio 4 5)
     #false
   )  
)

;; (g) Dado um aluno, registra se ele deve ganhar ponto extra ou não.

;; atualiza-aluno-ponto-extra: aluno -> aluno
;; Objetivo: dado um aluno, atualiza o registro se o mesmo possui ponto extra com base na suta de exercícios do tipo doc

;; Exemplo:
;; (atualiza-aluno-ponto-extra
;;     (make-aluno
;;        "Miguel"
;;        (make-prova 5 5)
;;        (make-prova 4 5)
;;        (make-exercicios
;;           (make-nota-exercicio 8 7)
;;           (make-nota-exercicio 8 7)
;;           #false
;;        )
;;     )
;;) =
;;  (make-aluno
;;     "Miguel"
;;     (make-prova 5 5)
;;     (make-prova 4 5)
;;     (make-exercicios
;;        (make-nota-exercicio 4 5)
;;        (make-nota-exercicio 4 4)
;;        #true
;;     )
;;  )

(define (atualiza-aluno-ponto-extra aluno)
  (make-aluno
     (aluno-nome aluno)
     (aluno-prova-1 aluno)
     (aluno-prova-2 aluno)
     (atualiza-ponto-extra (aluno-exercicios aluno))
  )
)

;; Testes:

(check-expect
  (atualiza-aluno-ponto-extra
     (make-aluno
        "Miguel"
        (make-prova 5 5)
        (make-prova 4 5)
        (make-exercicios
           (make-nota-exercicio 4 5)
           (make-nota-exercicio 4 4)
           #false
        )
     )
  )
  (make-aluno
     "Miguel"
     (make-prova 5 5)
     (make-prova 4 5)
     (make-exercicios
        (make-nota-exercicio 4 5)
        (make-nota-exercicio 4 4)
        #true
     )
  )
)

(check-expect
  (atualiza-aluno-ponto-extra
     (make-aluno
        "Jorge"
        (make-prova 3 2)
        (make-prova 4 5)
        (make-exercicios
           (make-nota-exercicio 3 2)
           (make-nota-exercicio 4 4)
           #false
        )
     )
  )
  (make-aluno
     "Jorge"
     (make-prova 3 2)
     (make-prova 4 5)
     (make-exercicios
        (make-nota-exercicio 3 2)
        (make-nota-exercicio 4 4)
        #false
     )
  )
)

;; (h) Dado um aluno, calcula sua nota: a nota é a média ponderada da prova 1, com peso 4, prova 2, com peso 5,
;; e a nota dos exercícios, com peso 1. A nota dos exercícios deve ser a soma das notas dos exercícios 1 e 2, mais o ponto extra,
;; se for o caso. Para cada exercício, a nota é média aritmética das notas de codificação e documentação.

;; Auxiliar 1

;; retorna-nota-exercicio: nota-exercicio -> Numero
;; Objetivo: dado um exercício, calcula a sua média aritmética e retorna o valor

;; Exemplo:
;; (retorna-nota-exercicio (make-nota-exercicio 3 2)) = 2.5

(define (retorna-nota-exercicio nota-exercicio)
  (/
    (+ (nota-exercicio-nota-codigo nota-exercicio) (nota-exercicio-nota-doc nota-exercicio))
    2
  )
)

;; Teste:

(check-expect
  (retorna-nota-exercicio (make-nota-exercicio 3 2))
  2.5
)

;; Auxiliar 2

;; retorna-nota-registro-exercicios: exercicios -> Numero
;; Objetivo: dado um registro de exercícios, soma a nota de dois exercícios e um ponto extra, caso exista

;; Exemplo:
;; (retorna-nota-registro-exercicios
;;       (make-exercicios
;;          (make-nota-exercicio 3 2)
;;          (make-nota-exercicio 4 4)
;;          #false
;;       )
;; ) = 6.5

(define (retorna-nota-registro-exercicios exercicios)
  (+
    (retorna-nota-exercicio (exercicios-nota-exercicio-1 exercicios))
    (retorna-nota-exercicio (exercicios-nota-exercicio-2 exercicios))
    (cond
      [(verifica-ponto-extra exercicios) 1]
      [else 0]
    )
  )
)

;; Teste:

(check-expect
   (retorna-nota-registro-exercicios
       (make-exercicios
          (make-nota-exercicio 3 2)
          (make-nota-exercicio 4 4)
          #false
       )
   )
   6.5
)

(check-expect
   (retorna-nota-registro-exercicios
       (make-exercicios
          (make-nota-exercicio 3 3)
          (make-nota-exercicio 4 4)
          #true
       )
   )
   8
)

;; retorna-nota-aluno: aluno -> Numero
;; Objetivo: dado um aluno, calcula sua média ponderada usando a nota de suas provas e exercícios, retornando o valor

;; Exemplo:
;; (retorna-nota-aluno
;;   (make-aluno
;;     "Jorge"
;;     (make-prova 3 2)
;;     (make-prova 4 5)
;;     (make-exercicios
;;        (make-nota-exercicio 3 2)
;;        (make-nota-exercicio 4 4)
;;        #false
;;     )
;;   )
;; ) = 2.5

(define (retorna-nota-aluno aluno)
  (/
    (+
      (* (retorna-total-prova (aluno-prova-1 aluno)) 4)
      (* (retorna-total-prova (aluno-prova-2 aluno)) 5)
      (retorna-nota-registro-exercicios (aluno-exercicios aluno))
    )
    10
  )
)

;; Testes:

(check-expect
   (retorna-nota-aluno
     (make-aluno
        "Jorge"
        (make-prova 3 2)
        (make-prova 4 5)
        (make-exercicios
           (make-nota-exercicio 3 2)
           (make-nota-exercicio 4 4)
           #false
        )
      )
   )
   7.15
)

(check-expect
   (retorna-nota-aluno
     (make-aluno
        "Miguel"
        (make-prova 3 3)
        (make-prova 4 5)
        (make-exercicios
           (make-nota-exercicio 3 3)
           (make-nota-exercicio 4 5)
           #true
        )
      )
   )
   7.75
)
