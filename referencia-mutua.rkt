;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname referencia-mutua) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct genitor (nome ano olhos filhos))
;; Um elemento do conjunto Genitor é
;; (make-genitor n a o f)
;; onde:
;; n : String, representa o nome da pessoa
;; a : Número, representa o ano de nascimento da pessoa
;; o : String, representa a cor dos olhos da pessoa
;; p : ListaDeFilhos, representa os filhos da pessoa

;; Um elemento do conjunto ListaDeFilhos pode ser
;; 1. empty, ou
;; 2. (cons p lf)
;; onde:
;; p : Genitor

;; lf : ListaDeFilhos

;; descendenteOlhosAzuis?: Genitor -> Boooleano
;; Dada uma pessoa, verificar se ela ou algum de seus descendentes tem olhos azuis.
;; Exemplo:
;; (descendentes-olhos-azuis? CARLOS) = true
;; (descendentes-olhos-azuis? JOAO) = false

(define (descendenteOlhosAzuis? g)
  (cond
    ;; Se g tem olhos azuis, devolver true.
    [(string=? (genitor-olhos g) "azuis") true]
    ;; Senão, existe ancestral de olhos azuis se
    ;; existe alguém de olhos azuis entre
    [else
     (descendenteOlhosAzuisLista? (genitor-filhos g))]))

(define (descendenteOlhosAzuisLista? lf)
  (cond
    [(empty? lf) false]
    [else
     (or
      (descendenteOlhosAzuis? (first lf))
      (descendenteOlhosAzuisLista? (rest lf)))]))

;; Constantes:

(define JOAO (make-genitor "João" 1952 "verdes" empty))
(define MARIA (make-genitor "Maria" 1955 "castanhos" empty))

(define SOFIA (make-genitor "Sofia" 1986 "azuis" empty))
(define GUSTAVO (make-genitor "Gustavo" 1988 "verdes" empty))

(define LF-EVA (list SOFIA GUSTAVO))
(define EVA (make-genitor "Eva" 1960 "azuis" LF-EVA))

(define LF-CARLOS (list JOAO MARIA EVA))
(define CARLOS (make-genitor "Carlos" 1926 "verdes" LF-CARLOS))


;; Exercicios

;; 1)

;; contaDescendenteOlhosAzuis : Genitor -> Número
;; Objetivo : dado um genitor, conta quantos descendentes de olhos azuis ele possui

(define (contaDescendenteOlhosAzuisLista lista-filhos)
  (cond
    [(empty? lista-filhos) 0]
    [else
     (+
      (contaDescendenteOlhosAzuis (first lista-filhos))
      (contaDescendenteOlhosAzuisLista (rest lista-filhos)))]))

(define (contaDescendenteOlhosAzuis genitor)
  (cond
    [(string=? (genitor-olhos genitor) "azuis") (+ 1 (contaDescendenteOlhosAzuisLista (genitor-filhos genitor)))]
    [else (contaDescendenteOlhosAzuisLista (genitor-filhos genitor))]))


(contaDescendenteOlhosAzuis CARLOS)

;; 2)

(define (retornaNomesDescendenteOlhosAzuisLista lista-filhos)
  (cond
    [(empty? lista-filhos) empty]
    [else
     (cons
      (retornaNomesDescendenteOlhosAzuis (first lista-filhos))
      (retornaNomesDescendenteOlhosAzuisLista (rest lista-filhos)))]))

(define (retornaNomesDescendenteOlhosAzuis genitor)
  (cond
    [(string=? (genitor-olhos genitor) "azuis")
     (cons (genitor-nome genitor) (retornaNomesDescendenteOlhosAzuisLista (genitor-filhos genitor)))]
    [else (retornaNomesDescendenteOlhosAzuisLista (genitor-filhos genitor))]))

(retornaNomesDescendenteOlhosAzuis CARLOS)

;; 3)

(define (desenha-circulo-olho nome-genitor cor)
  (overlay
   (text nome-genitor 16 "white")
   (circle 45 "solid" cor)))

(define (desenhaNomesDescendenteOlhosAzuisLista lista-filhos)
  (cond
    [(empty? lista-filhos) empty-image]
    [else
     (beside
      (desenhaNomesDescendenteOlhosAzuis (first lista-filhos))
      (desenhaNomesDescendenteOlhosAzuisLista (rest lista-filhos)))]))

(define (desenhaNomesDescendenteOlhosAzuis genitor)
  (cond
    [(string=? (genitor-olhos genitor) "azuis")
     (beside (desenha-circulo-olho (genitor-nome genitor) "blue")
             (desenhaNomesDescendenteOlhosAzuisLista (genitor-filhos genitor)))]
    [else (desenhaNomesDescendenteOlhosAzuisLista (genitor-filhos genitor))]))

(desenhaNomesDescendenteOlhosAzuis CARLOS)

;; 4)

(define (traduz-cor-olhos cor)
  (cond
    [(string=? cor "azuis") "blue"]
    [(string=? cor "verdes") "darkgreen"]
    [(string=? cor "castanhos") "brown"]
    [else "black"]))

(define (desenhaNomesDescendenteOlhosLista lista-filhos cor-olhos)
  (cond
    [(empty? lista-filhos) empty-image]
    [else
     (beside
      (desenhaNomesDescendenteOlhos (first lista-filhos) cor-olhos)
      (desenhaNomesDescendenteOlhosLista (rest lista-filhos) cor-olhos))]))

(define (desenhaNomesDescendenteOlhos genitor cor-olhos)
  (cond
    [(string=? (genitor-olhos genitor) cor-olhos)
     (beside (desenha-circulo-olho (genitor-nome genitor) (traduz-cor-olhos cor-olhos))
             (desenhaNomesDescendenteOlhosLista (genitor-filhos genitor) cor-olhos))]
    [else (desenhaNomesDescendenteOlhosLista (genitor-filhos genitor) cor-olhos)]))

(desenhaNomesDescendenteOlhos CARLOS "verdes")

;; 5)

(define (desenhaTodosDescendenteOlhosLista lista-filhos)
  (cond
    [(empty? lista-filhos) empty-image]
    [else
     (beside
      (desenhaTodosDescendenteOlhos (first lista-filhos))
      (desenhaTodosDescendenteOlhosLista (rest lista-filhos)))]))

(define (desenhaTodosDescendenteOlhos genitor)
     (beside (desenha-circulo-olho (genitor-nome genitor) (traduz-cor-olhos (genitor-olhos genitor)))
             (desenhaTodosDescendenteOlhosLista (genitor-filhos genitor))))

(desenhaTodosDescendenteOlhos CARLOS)