;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname listas) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; === Exercícios de Exemplo (Slides) ===

;; Uma ListaDeNúmeros pode ser
;; 1. vazia (empty) ou
;; 2. (cons n l), onde
;;    n : Número
;;    l : ListaDeNúmeros

(define L1
  (cons 1.5
    (cons -6 empty)
  )
)

(define L2
  (cons 4
    (cons -6 (cons 7 empty))
  )
)

(define L3
  (cons 89
    (cons 0 empty)
  )
)

;; tamanho: ListaDeNúmeros → Número
;; Dada uma lista de números, devolver o número de elementos da lista
;; Exemplos:
;; (tamanho L2) = 3
;; (tamanho empty) = 0
(define (tamanho lista-de-numeros)
  (cond
    ;; Se a lista lista-de-numeros está vazia, ...
    [(empty? lista-de-numeros) 0]
    ;; Senão, ...
    [else (+ 1 (tamanho (rest lista-de-numeros)))]
  )
)

(check-expect (tamanho L2) 3)

(tamanho (cons 3 (cons -2 (cons 0 (cons 1 empty)))))


;; somaNúmeros: ListaDeNúmeros → Número
;; Dada uma lista de números, devolver a soma dos números da lista
;; Exemplos:
;; (somaNúmeros L2) = 5
;; (somaNúmeros empty) = 0
(define (somaNúmeros lista-de-numeros)
  (cond
    ;; Se a lista lista-de-numeros está vazia, ...
    [(empty? lista-de-numeros) 0]
    ;; Senão, ...
    [else (+ (first lista-de-numeros) (somaNúmeros (rest lista-de-numeros)))]
  )
)

(somaNúmeros L2)

;; somaInversos: ListaDeNúmeros → Número
;; Dada uma lista de números, devolver a soma dos inversos dos números da lista
;; Exemplos:
;; (somaInversos L2) = 1.25
;; (somaInversos empty) = 0
(define (somaInversos lista-de-numeros)
  (cond
    ;; Se a lista lista-de-numeros está vazia, ...
    [(empty? lista-de-numeros) 0]
    ;; Senão, ...
    [else (+ (/ 1 (first lista-de-numeros)) (somaNúmeros (rest lista-de-numeros)))]
  )
)

(somaInversos L2)


;; imgNúmeroGenerelizada: Número String → Image
;; Dado um número, devolver uma imagem do número
;; (imgNúmeroGenerelizada 2) = retorna a imagem de um 2
;; (imgNúmeroGenerelizada -8) = retorna a imagem de um -8
(define (imgNúmero numero)
  (text (number->string numero) 16 "white")
)


;; imgNúmeros: ListaDeNúmeros String → Image
;; Dada uma lista de números e uma cor, devolver uma imagem da lista de números com um em cima do outro
;; com a cor de acordo com a informada
;; Exemplos:
;; (imgNúmeros L2) = retorna a imagem dos valores um em cima do outro
;; (imgNúmeros empty) = retorna nenhuma imagem
(define (imgNúmeros lista-de-numeros cor)
  (cond
    ;; Se a lista lista-de-numeros está vazia, ...
    [(empty? lista-de-numeros) empty-image]
    ;; Senão, ...
    [else
     (above
      (imgNúmeroGenerelizada (first lista-de-numeros) cor)
      (imgNúmeros (rest lista-de-numeros)
                  (cond [(string=? cor "red") "blue"] [else "red"])))]))


;; === Exercícios (Slides) ===

;; 1) Generalização : Generalize a função imgNúmero para receber a cor na qual o número deve ser desenhado.

;; imgNúmeroGenerelizada: Número String → Image
;; Dado um número e uma cor, devolver uma imagem do número usando a cor informada
;; Exemplos:
;; (imgNúmeroGenerelizada 2 "blue") = retorna a imagem de um 2 azul
;; (imgNúmeroGenerelizada -8 "red") = retorna a imagem de um -8 vermelho
(define (imgNúmeroGenerelizada numero cor)
  (text (number->string numero) 16 cor)
)


;; 2) Decomposição: Usando as funções que você já construiu, construa a função
;; média-harmônica que, dada uma lista de números diferentes de zero, calcula a
;; média harmônica dos elementos da lista.

;; média-harmônica: ListaDeNúmeros → Número
;; Dada uma lista de números, retorna a médica harmônica dos valores
;; Exemplos:
;; (média-harmônica L1) = retorna a imagem de um 2 azul
;; (média-harmônica L2) = retorna a imagem de um -8 vermelho
(define (média-harmônica lista-de-numeros)
  (/ (tamanho lista-de-numeros) (somaInversos lista-de-numeros))
)

(check-expect (média-harmônica L1) -0.375)
(check-expect (média-harmônica L2) 2.4)


;; 3) Imagine agora que a lista de entrada pode conter zeros. Neste caso não é possível
;; construir a média harmônica (pois teríamos divisão por zero). Construa uma função
;; média-harmônica-ou-msg que, dada uma lista de números, devolve a média
;; harmônica dos números, se não houver zeros na lista. Se houver, devolve a
;; mensagem "Lista de entrada com zeros - impossível calcular média harmônica."


;; Um ResultadoMedicaHarmonica pode ser
;; 1. Um Número ou
;; 2. Uma String

(define MENSAGEM_SEM_MEDIA "Lista de entrada com zeros - impossível calcular média harmônica.")

;; possui-divisao-zero: ListaDeNúmeros → Número
;; Dada uma lista de números, retorna o número de zeros presentes na lista, adicionando uma
;; unidade no contador sempre que um zero for encontrado
;; Exemplos:
;; (possui-divisao-zero L3) = 1
;; (possui-divisao-zero L2) = 0
(define (possui-divisao-zero lista-de-numeros)
   (cond
    ;; Se a lista lista-de-numeros está vazia, ...
    [(empty? lista-de-numeros) 0]
    ;; Se a lista tiver divisão por zero...
    [(= (first lista-de-numeros) 0) (+ (possui-divisao-zero (rest lista-de-numeros)) 1)]
    ;; Senão, ...
    [else (+ (possui-divisao-zero (rest lista-de-numeros)) 0)]
  )
)

(check-expect (possui-divisao-zero L3) 1)
(check-expect (possui-divisao-zero L2) 0)

;; média-harmônica-ou-msg: ListaDeNúmeros → ResultadoMedicaHarmonica
;; Dada uma lista de números, retorna a médica harmônica dos valores caso não haja divisão por 0
;; ou uma mensagem de erro caso haja
;; Exemplos:
;; (média-harmônica-ou-msg L3) = MENSAGEM_SEM_MEDIA
;; (média-harmônica-ou-msg L2) = 2.4
(define (média-harmônica-ou-msg lista-de-numeros)
  (cond
    ;; Se a lista tiver divisão por zero...
    [(> (possui-divisao-zero lista-de-numeros) 0) MENSAGEM_SEM_MEDIA]
    ;; Senão, ...
    [else (média-harmônica lista-de-numeros)]
  )
)

(check-expect (média-harmônica-ou-msg L3) MENSAGEM_SEM_MEDIA)
(check-expect (média-harmônica-ou-msg L2) 2.4)


;; 4) Construa uma função lista-números-com-média que gera uma imagem com os elementos da
;; lista, escritos em cores alternadas um sobre o outro (um vermelho, um azul, um vermelho, um azul,
;; ...), colocando no final a média harmônica dos elementos da lista.

;; Exemplos:
;; (lista-números-com-média L3) = Desenha lista de números dizendo que divisão é impossível
;; (lista-números-com-média L2) = Desenha lista de números mostrando resultado da média

(define (lista-números-com-média lista-de-numeros)
  (above
    (imgNúmeros lista-de-numeros "red")
    (beside
      (text "media harmonica: " 16 "white")
      (cond
        [(string? (média-harmônica-ou-msg lista-de-numeros)) (text MENSAGEM_SEM_MEDIA 16 "red")]
        [else
         (text (number->string (/ (round (* (exact->inexact (média-harmônica-ou-msg lista-de-numeros)) 100)) 100) ) 16 "white")
        ]
      )
    )
  )
)

(lista-números-com-média L3)
(lista-números-com-média L2)