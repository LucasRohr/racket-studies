;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname listas-2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct carta (naipe número))
;; Um elemento do conjunto Carta tem o formato
;; (make-carta n v)
;; onde:
;; n : String, representa o naipe da carta
;; v : Número, representa o valor da carta


(define ÁsCopas (make-carta "copas" 1))
(define DezOuros (make-carta "ouros" 10))
(define RainhaPaus (make-carta "paus" 12))


(define PILHA_1 (list ÁsCopas DezOuros ÁsCopas RainhaPaus DezOuros))

;; Uma PilhaDeCartas pode ser
;; 1. vazia (empty), ou
;; 2. (cons c l) , onde
;; c : Carta
;; l : PilhaDeCartas

(define (filtra-copas pilha-cartas)
  (cond
    [(empty? pilha-cartas) empty]
    [(string=? (carta-naipe (first pilha-cartas)) "copas") (cons (first pilha-cartas) (filtra-copas (rest pilha-cartas)))]
    [else (filtra-copas (rest pilha-cartas))]
  )
)

(filtra-copas PILHA_1)

;; =========

(define (filtra-cartas pilha-cartas naipe)
  (cond
    [(empty? pilha-cartas) empty]
    [(string=? (carta-naipe (first pilha-cartas)) naipe) (cons (first pilha-cartas) (filtra-copas (rest pilha-cartas)))]
    [else (filtra-copas (rest pilha-cartas))]
  )
)

(filtra-cartas PILHA_1 "ouros")

;; =========

(define (retorna-numeros-cartas pilha-cartas)
  (cond
    [(empty? pilha-cartas) empty]
    [else (cons (carta-número (first pilha-cartas)) (retorna-numeros-cartas (rest pilha-cartas)))]
  )
)

(retorna-numeros-cartas PILHA_1)


;; =========

(define (retorna-soma-ouros pilha-cartas)
  (cond
    [(empty? pilha-cartas) 0]
    [(string=? (carta-naipe (first pilha-cartas)) "ouros") (+ (carta-número (first pilha-cartas)) (retorna-soma-ouros (rest pilha-cartas)))]
    [else (retorna-soma-ouros (rest pilha-cartas))]
  )
)

(retorna-soma-ouros PILHA_1)
