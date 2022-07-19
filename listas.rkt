;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname listas) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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
    (cons -11 empty)
  )
)

;; tamanho: ListaDeNúmeros → Número
;; Dada uma lista de números, devolver o número de elementos da lista
;; Exemplos:
;; (tamanho L2) = 3
;; (tamanho empty) = 0
(define (tamanho lista-de-numeros)
  (cond
    ;; Se a lista ln está vazia, ...
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
    ;; Se a lista ln está vazia, ...
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
    ;; Se a lista ln está vazia, ...
    [(empty? lista-de-numeros) 0]
    ;; Senão, ...
    [else (+ (/ 1 (first lista-de-numeros)) (somaNúmeros (rest lista-de-numeros)))]
  )
)

(somaInversos L2)

(define (imgNúmero numero)
  (text (number->string numero) 16 "white")
)


;; imgNúmeros: ListaDeNúmeros → Image
;; Dada uma lista de números, devolver uma imagem da lista de números com um em cima do outro
;; Exemplos:
;; (somaInversos L2) = 1.25
;; (somaInversos empty) = 0
(define (imgNúmeros lista-de-numeros)
  (cond
    ;; Se a lista ln está vazia, ...
    [(empty? lista-de-numeros) empty-image]
    ;; Senão, ...
    [else (above (imgNúmero (first lista-de-numeros)) (imgNúmeros (rest lista-de-numeros)))]
  )
)

(imgNúmeros L2)


