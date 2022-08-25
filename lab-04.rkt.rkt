;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab4-2022-1-template) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; ==========================================================
;; DEFINIÇÕES DE DADOS:
;; ==========================================================

;; Uma página web pode ser modelada com um nome e um conteúdo:

;; ----------------------
;; TIPO PAG-WEB
;; ----------------------
(define-struct pag-web (nome conteudo))
;; Um elemento do conjunto Pag-web é uma estrutura
;;     (make-pag-web um-nome um-cont) onde
;; um-nome: String, é o nome da página e
;; um-cont: Lista-conteudo, é uma lista de conteúdo de páginas-web.

;; -----------------------------------
;; TIPO LISTA-CONTEUDO:
;; -----------------------------------
;; Uma Lista-conteudo é
;; 1. empty (vazia), ou
;; 2. (cons p lc), onde
;;     p : String
;;    lc : Lista-conteúdo, ou
;; 3. (cons i lc), onde
;;     i : Imagem
;;    lc : Lista-conteúdo, ou
;; 4. (cons p lc), onde
;;     p : Pag-web
;;    lc : Lista-conteúdo.

;; =========================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; =========================================
;; Dê exemplos de elementos dos conjuntos Pag-web e Lista-conteúdo.
;; Dê, pelo menos, quatro exemplos de elementos de cada conjunto, sendo que um dos elementos de Pag-web deve ter no mínimo dois níveis de sub-páginas (ou seja, uma página que contém outra página que contém outra página).

(define CIRCULO_VERMELHO (circle 30 "solid" "red"))
(define RETANGULO_AZUL (rectangle 50 60 "solid" "blue"))



(define CONTEÚDO1 (list "Sua home" CIRCULO_VERMELHO "Footer"))
(define PAG1 (make-pag-web "Home" CONTEÚDO1))

(define CONTEÚDO2 (list PAG1 RETANGULO_AZUL "Subtitle"))
(define PAG2 (make-pag-web "Busca" CONTEÚDO2))

(define CONTEÚDO3 (list "Produto daora" CIRCULO_VERMELHO PAG2))
(define PAG3 (make-pag-web "Produto" CONTEÚDO3))

(define CONTEÚDO4 (list "Compre o produto" RETANGULO_AZUL "Baratinho"))
(define PAG4 (make-pag-web "Compra" CONTEÚDO4))

;; ==========================================================
;; DEFINIÇÕES DE FUNÇÕES:
;; ==========================================================

;; =========================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
;; =========================================
;; Construa a função lista-palavras que, dada uma página web, devolve a lista de palavras que a página contém, sem considerar suas sub-páginas.

;; ListaDePalavras
;; 1. empty (vazia), ou
;; 2. (cons p lp), onde
;;     p : String
;;    lp : ListaDePalavras

;; lista-palavras : Pag-web -> ListaDePalavras
;; Objetivo : dada uma pagina, retorna uma lista das palavras nela, sem considerar sub paginas
;; Exemplos/Testes:
(check-expect (lista-palavras PAG4) (list "Compre o produto" "Baratinho"))
(check-expect (lista-palavras PAG2) (list "Subtitle"))

(define (lista-palavras pagina)
  (cond
    [(empty? (pag-web-conteudo pagina)) empty]
    [(string? (first (pag-web-conteudo pagina)))
     (cons (first (pag-web-conteudo pagina)) (lista-palavras (make-pag-web (pag-web-nome pagina) (rest (pag-web-conteudo pagina)))))]
    [else (lista-palavras (make-pag-web (pag-web-nome pagina) (rest (pag-web-conteudo pagina))))]))


;; =========================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; =========================================
;; Desenvolva a função mostra-imagens que, dada uma página web, devolve uma imagem contendo as imagens contidas na página e em suas sub-páginas, lado a lado

(define (mostra-imagens-lista lista-conteudo)
  (cond
    [(empty? lista-conteudo) empty-image]
    [(image? (first lista-conteudo))
     (beside
      (first lista-conteudo)
      (mostra-imagens-lista (rest lista-conteudo)))]
    [(pag-web? (first lista-conteudo))
     (beside
      (mostra-imagens (first lista-conteudo))
      (mostra-imagens-lista (rest lista-conteudo)))]
    [else (mostra-imagens-lista (rest lista-conteudo))]))

;; mostra-imagens : Pag-web -> Imagem
;; Objetivo : dada uma pagina web, retorna as imagens de seu conteudo e suas subpaginas lado a lado
;; Exemplos:
;; (mostra-imagens PAG3) = desenha CIRCULO_VERMELHO CIRCULO_VERMELHO RETANGULO_AZUL
;; (mostra-imagens PAG4) = desenha RETANGULO_AZUL

(define (mostra-imagens pagina)
  (cond
    [(image? (first (pag-web-conteudo pagina)))
     (beside (first (pag-web-conteudo pagina)) (mostra-imagens-lista (rest (pag-web-conteudo pagina))))]
    [else (mostra-imagens-lista (pag-web-conteudo pagina))]))

(mostra-imagens PAG3)

;; =========================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
;; =========================================
;; Modifique a função da questão anterior para colocar o nome da página à qual a imagem pertence em cima de cada imagem. Use fonte 15 e cor azul. Não coloque espaço entre o nome da página e a imagem.

(define (desenha-imagem-com-nome imagem nome)
  (above (text nome 15 "blue") imagem))

(define (mostra-imagens-lista-nome lista-conteudo nome-pagina)
  (cond
    [(empty? lista-conteudo) empty-image]
    [(image? (first lista-conteudo))
     (beside
      (desenha-imagem-com-nome (first lista-conteudo) nome-pagina)
      (mostra-imagens-lista-nome (rest lista-conteudo) nome-pagina))]
    [(pag-web? (first lista-conteudo))
     (beside
      (mostra-imagens-nome (first lista-conteudo))
      (mostra-imagens-lista-nome (rest lista-conteudo) nome-pagina))]
    [else (mostra-imagens-lista-nome (rest lista-conteudo) nome-pagina)]))

;; mostra-imagens-nome : Pag-web -> Imagem
;; Objetivo : dada uma pagina web, retorna as imagens de seu conteudo e suas subpaginas lado a lado, com o nome da pagina em cima de cada imagem
;; Exemplos:
;; (mostra-imagens PAG3) = desenha CIRCULO_VERMELHO + Produto e CIRCULO_VERMELHO + Home e RETANGULO_AZUL + Busca
;; (mostra-imagens PAG4) = desenha RETANGULO_AZUL + Compra

(define (mostra-imagens-nome pagina)
  (cond
    [(image? (first (pag-web-conteudo pagina)))
     (beside (desenha-imagem-com-nome (first (pag-web-conteudo pagina)) (pag-web-nome pagina)) (mostra-imagens-lista-nome (rest (pag-web-conteudo pagina)) (pag-web-nome pagina)))]
    [else (mostra-imagens-lista-nome (pag-web-conteudo pagina) (pag-web-nome pagina))]))

(mostra-imagens-nome PAG3)

;; =========================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
;; =========================================
;; Construa a função gera-imagem-pagina, que gera uma imagem para uma página web, mostrando seu conteúdo de forma estruturada, indicando o nome da página bem como seu conteúdo.

(define (mostra-conteudo-pagina lista-conteudo)
  (cond
    [(empty? lista-conteudo) empty-image]
    [(image? (first lista-conteudo))
     (beside
      (first lista-conteudo)
      (mostra-conteudo-pagina (rest lista-conteudo)))]
    [(pag-web? (first lista-conteudo))
     (beside
      (gera-imagem-pagina (first lista-conteudo))
      (mostra-conteudo-pagina (rest lista-conteudo)))]
    [(string? (first lista-conteudo))
     (beside
      (text (first lista-conteudo) 15 "black")
      (mostra-conteudo-pagina (rest lista-conteudo)))]
    [else (mostra-conteudo-pagina (rest lista-conteudo))]))

(define (gera-imagem-pagina pagina)
  (cond
    [(pag-web? (first (pag-web-conteudo pagina)))
     (beside (mostra-conteudo-pagina (pag-web-conteudo (first (pag-web-conteudo pagina)))) (gera-imagem-pagina (make-pag-web (pag-web-nome pagina) (rest (pag-web-conteudo pagina)))))]
    [else (mostra-conteudo-pagina (pag-web-conteudo pagina))]))

(gera-imagem-pagina PAG3)
