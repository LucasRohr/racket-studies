;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname high-order) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercicios 1.0

;; 1) Crie uma função de alta ordem calculadora que realize operações aritméticas sobre um par de números.

;; calculadora : (Número Número -> Número) Número Número
(define (calculadora operacao valor1 valor2)
  (operacao valor1 valor2))


(calculadora + 1 2)

;; 2) Crie uma função mapeia que, dadas uma função unária sobre números e uma lista de números, gera uma lista
;; com os resultados da função aplicada a cada elemento da lista original.

;; mapeia : (Número -> Número) ListaDeNúmeros
(define (mapeia operacao lista-numeros)
  (cond
    [(empty? lista-numeros) empty]
    [else
     (cons (operacao (first lista-numeros)) (mapeia operacao (rest lista-numeros)))]))

(mapeia sqr (list 2 5 10 3))


;; 3) Crie uma nova função mapeia que, dadas uma função unária que a partir de um número gera um string, e uma
;; lista de números, gera uma lista com os resultados da função aplicada a cada elemento da lista original.

;; mapeia-mod : (Número -> String) ListaDeNúmeros
(define (mapeia-mod op-numero-string lista-numeros)
  (cond
    [(empty? lista-numeros) empty]
    [else
     (cons (op-numero-string (first lista-numeros)) (mapeia-mod op-numero-string (rest lista-numeros)))]))

(define (cor? valor)
  (cond
    [(= valor 1) "black"]
    [(= valor 2) "red"]
    [(= valor 3) "green"]
    [else "black"]))

(mapeia-mod cor? (list 1 2 3 4))


;; 4) Apresente uma função filtraNum que é usada para selecionar elementos de uma lista de números. A função
;; deve receber uma função que recebe um número e retorna um booleano e uma lista e produzir uma lista
;; contendo somente os elementos da lista original para os quais a função fornecida retorna true.

(define (filtraNum opecao-filtro lista-numeros)
  (cond
    [(empty? lista-numeros) empty]
    [(opecao-filtro (first lista-numeros)) (cons (first lista-numeros) (filtraNum opecao-filtro (rest lista-numeros)))]
    [else (filtraNum opecao-filtro (rest lista-numeros))]))

(filtraNum even? (list 1 2 3 4))


;; Exercicios 2.0


(define-struct ponto (x y))

(define LISTA-PONTOS-1 (list
                        (make-ponto 1 2)
                        (make-ponto 4 -3)
                        (make-ponto 1 7)
                        (make-ponto -8 14)
                        (make-ponto -42 -5)))

;; 1) Dada uma lista de pontos, devolva todos pontos com coordenada x igual a 1

(define (retorna-pontos-x-1 lista-pontos)
  (filter (lambda (ponto) (= (ponto-x ponto) 1)) lista-pontos))

(retorna-pontos-x-1 LISTA-PONTOS-1)

;; 2) Dada uma lista de pontos, devolva as coordenadas y dos pontos com a coordenada x igual a 1

(define (retorna-y-pontos-x-1 lista-pontos)
  (map (lambda (ponto) (ponto-y ponto)) (retorna-pontos-x-1 lista-pontos)))

(retorna-y-pontos-x-1 LISTA-PONTOS-1)

;; 3) Dada uma lista de pontos, devolva a soma das coordenadas y dos pontos com a coordenada x igual a 1

(define (retorna-soma-y-pontos-x-1 lista-pontos)
  (foldl + 0 (retorna-y-pontos-x-1 lista-pontos)))

(retorna-soma-y-pontos-x-1 LISTA-PONTOS-1)

;; 4) Dada uma lista de pontos e um número, devolva os pontos com a coordenada x igual maior ou igual a este número

(define (filtra-lista-x-comparado lista-pontos numero)
  (filter (lambda (ponto) (>= (ponto-x ponto) numero)) lista-pontos))

(filtra-lista-x-comparado LISTA-PONTOS-1 2)


