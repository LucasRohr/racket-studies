;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test-02) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; ==== Autorreferencia ====


(define-struct pessoa (nome ano olhos pai mãe))
;; Um elemento do conjunto Pessoa pode ser:

;; 1. empty, representando a falta de informação, ou
;; 2. (make-pessoa n a o p m), onde:
;; n : String, o nome da pessoa
;; a : Número, o ano de nascimento da pessoa
;; o : String, a cor do olho da pessoa
;; p : Pessoa, o pai da pessoa
;; n : Pessoa, a mãe da pessoa

;; == ListaDePessoas ==

;; Uma ListaDePessoas pode ser:
;; 1) empty, vazia ou
;; 2) (cons p lp), onde:
;;    p : Pessoa
;;    lp : ListaDePessoas

;; Definição de constantes:

(define CARLOS (make-pessoa "Carlos" 1926 "verdes" empty empty))
(define BETINA (make-pessoa "Betina" 1927 "verdes" empty empty))
(define EVA (make-pessoa "Eva" 1960 "azuis" CARLOS BETINA))
(define FRED (make-pessoa "Fred" 1958 "castanhos" empty empty))
(define GUSTAVO (make-pessoa "Gustavo" 1988 "castanhos" FRED EVA))


(define (ancestralOlhosAzuis? pessoa)
  (cond
    [(empty? pessoa) #false]
    [(string=? (pessoa-olhos pessoa) "azuis") #true]
    [else
     (or (ancestralOlhosAzuis? (pessoa-pai pessoa)) (ancestralOlhosAzuis? (pessoa-mãe pessoa)))]))

(ancestralOlhosAzuis? GUSTAVO)


(define (conta-ancestrais pessoa)
  (cond
    [(empty? pessoa) 0]
    [else
     (+ 1
        (conta-ancestrais (pessoa-pai pessoa))
        (conta-ancestrais (pessoa-mãe pessoa)))]))

(conta-ancestrais GUSTAVO)


;; Árvore Binária:

(define-struct nó (id conteúdo esq dir))
;; Um elemento do conjunto AB (Árvore Binária) pode ser:

;; 1) empty para representar falta de informação, ou
;; 2) no formato (make-nó i c e d), onde:

;; i : Número, o ID do nó
;; c : String, representa o conteúdo do nó
;; e : AB, representa a sub-árvore da esquerda
;; d : AB, representa a sub-árvore da direita

;; =======

;; Um elemento do conjunto ABP (Árvore Binária de Pesquisa) pode ser

;; 1) empty , representando a falta de informação, ou
;; 2) (make-nó id c e d) onde:

;; id : Número, representa o identificador do nó
;; c : String, representa o conteúdo do nó
;; e : ABP, representa a sub-árvore da esquerda e
;; contém apenas nós com identificadores menores que id
;; d : ABP, representa a sub-árvore da direita
;; contém apenas nós com identificadores maiores que id

(define AB1
  (make-nó 10 "A"
           (make-nó 12 "B" empty empty)
           (make-nó 3 "C"
                    (make-nó 15 "D"
                             (make-nó 1 "F" empty empty)
                             empty)
                    (make-nó 20 "E" empty empty))))

(define (retorna-ids-arvore-binaria no)
  (cond
    [(empty? no) empty]
    [else (append
           (list (nó-id no))
           (retorna-ids-arvore-binaria (nó-esq no))
           (retorna-ids-arvore-binaria (nó-dir no)))]))

(retorna-ids-arvore-binaria AB1)



(define-struct par (id cont))
;; Um elemento do conjunto Par tem o formato
;; (make-par id c), onde:
;; id : Número, representa o id
;; c : String, representa um conteúdo

;; Uma ListaPar é
;; 1. empty, ou
;; 2. (cons p lp), onde p: Par e lp: ListaPar

;; insere-nó: Número String ABP -> ABP
;; Dados um identificador, um conteúdo e uma ABP, insere um nó com este identificador e conteúdo na ABP.
;; Exemplo:
;; (insere-nó 20 “E" empty) = (make-nó 20 “E” empty empty)
(define (insere-nó id conteudo abp)
  (cond
    [(empty? abp) (make-nó id conteudo empty empty)]
    [(> id (nó-id abp))
     (make-nó
      (nó-id abp)
      (nó-conteúdo abp)
      (nó-esq abp)
      (insere-nó id conteudo (nó-dir abp)))]
    [(<= id (nó-id abp))
     (make-nó
      (nó-id abp)
      (nó-conteúdo abp)
      (insere-nó id conteudo (nó-esq abp))
      (nó-dir abp))]))

;; constróiABP: ListaPar -> ABP
;; Dada uma lista de pares de identificador e conteúdo, gera uma ABP com estes pares.
(define (constróiABP lp)
  (cond
    [(empty? lp) empty]
    [else
     (insere-nó ;; inserir um nó com
      (par-id (first lp)) ;; o identificador do primeiro nó da lista
      (par-cont (first lp)) ;; o conteúdo do primeiro nó da lista
      (constróiABP(rest lp)))])) ;; na árvore com o resto dos pares da lista

(define L1 (list (make-par 11 "F") (make-par 20 "E") (make-par 3 "B")
(make-par 15 "D") (make-par 17 "C") (make-par 10 "A")))

(constróiABP L1)



;; ==== Referência Mútua ====


(define-struct genitor (nome ano olhos filhos))
;; Um elemento do conjunto Genitor é
;; (make-genitor n a o f)
;; onde:
;; n : String, representa o nome da pessoa
;; a : Número, representa o ano de nascimento da pessoa
;; o : String, representa a cor dos olhos da pessoa
;; p : ListaDeFilhos, representa os filhos da pessoa


;; === ListaDeFilhos ===

;; Uma ListaDeFilhos pode ser:
;; 1. empty, vazia ou:
;; 2. (cons g lf), onde:
;;     g : Genitor
;;     lf: ListaDeFilhos


;; Definições de constantes:

(define SOFIA (make-genitor "Sofia" 1986 "azuis" empty))
(define GUSTAVO2 (make-genitor "Gustavo" 1988 "verdes" empty))
(define LF-EVA (list SOFIA GUSTAVO2))
(define EVA2 (make-genitor "Eva" 1960 "azuis" LF-EVA))

(define JOAO (make-genitor "João" 1952 "verdes" empty))
(define MARIA (make-genitor "Maria" 1955 "castanhos" empty))
(define LF-CARLOS (list JOAO MARIA EVA2))
(define CARLOS2 (make-genitor "Carlos" 1926 "verdes" LF-CARLOS))

(define (descendenteOlhosAzuisLista? lista-filhos)
  (cond
    [(empty? lista-filhos) #false]
    [else
     (or
      (descendenteOlhosAzuis? (first lista-filhos))
      (descendenteOlhosAzuisLista? (rest lista-filhos)))]))


;; descendenteOlhosAzuis? : Genitor -> Booleano
;; Objetivo : dado um genitor, verifica se ele ou um de seus descendentes possui olhos azuis
(define (descendenteOlhosAzuis? genitor)
  (cond
    [(empty? genitor) #false]
    [(string=? (genitor-olhos genitor) "azuis") #true]
    [(empty? (genitor-filhos genitor)) #false]
    [else (descendenteOlhosAzuisLista? (genitor-filhos genitor))]))

(descendenteOlhosAzuis? CARLOS2)
(descendenteOlhosAzuis? GUSTAVO2)


(define (contaGenitoresOlhosAzuisLista lista-filhos)
  (cond
    [(empty? lista-filhos) 0]
    [else (+
             (contaGenitoresOlhosAzuis (first lista-filhos))
             (contaGenitoresOlhosAzuisLista (rest lista-filhos)))]))

(define (contaGenitoresOlhosAzuis genitor)
  (cond
    [(empty? genitor) 0]
    [(string=? (genitor-olhos genitor) "azuis") (+ 1 (contaGenitoresOlhosAzuisLista (genitor-filhos genitor)))]
    [else (contaGenitoresOlhosAzuisLista (genitor-filhos genitor))]))

(contaGenitoresOlhosAzuis CARLOS2)
(contaGenitoresOlhosAzuis GUSTAVO2)



;; ==== Definições Locais ====

(define (bhaskara a b c)
  (local
    ((define delta (- (* b b) (* 4 a c)))
     (define menosB (* -1 b)))
    (cond
      [(< delta 0) empty]
      [(= delta 0) (/ menosB (* 2 a))]
      [else
       (list
        (/ (+ menosB (sqrt delta)) (* 2 a))
        (/ (- menosB (sqrt delta)) (* 2 a)))])))

(bhaskara 1 6 2)


(define x 1)
(define y 3)

(define (teste-local-func x y)
  (local ((define x 5)
          (define (multiplica valor) (* x valor)))
    (multiplica y)))

(teste-local-func x y)


;; ==== Funções de alta ordem ====

;; calculadora : (Número -> Número) Número Número
(define (calculadora operacao valor1 valor2)
  (operacao valor1 valor2))


(define (mapeia operacao lista)
  (cond
    [(empty? lista) empty]
    [else (cons
           (operacao (first lista)) (mapeia operacao (rest lista)))]))

(mapeia sqr (list 2 5 10 3))


;;ListaDeStrings
;; Uma ListaDeStrings pode ser:
;; 1. empty, vazia ou
;; 2. (cons s ls), onde:
;;     s : String
;;     ls : ListaDeStrings

;; mapeia-mod : (Número -> String) ListaDeNúmeros -> ListaDeStrings
(define (mapeia-mod operacao-str lista)
  (cond
    [(empty? lista) empty]
    [else (cons
           (operacao-str (first lista)) (mapeia-mod operacao-str (rest lista)))]))

;; cor? : Número -> String
(define (cor? valor)
  (cond
    [(= valor 2) "red"]
    [(= valor 3) "green"]
    [else "black"]))

(mapeia-mod cor? (list 1 2 3 4))

(define (filtraNum operacao-filtro lista)
  (cond
    [(empty? lista) empty]
    [(operacao-filtro (first lista)) (cons (first lista) (filtraNum operacao-filtro (rest lista)))]
    [else (filtraNum operacao-filtro (rest lista))]))

(filtraNum odd? (list 1 2 3 4))

;; Definição de estrutura
(define-struct ponto (x y))

;;ListaDePontos
;; Uma ListaDePontos pode ser:
;; 1. empty, vazia ou
;; 2. (cons p lp), onde:
;;     p : Ponto
;;     lp : ListaDePontos


(define LISTA_PONTOS (list (make-ponto 2 3) (make-ponto 1 3) (make-ponto 1 7) (make-ponto 0 89)))

(define (filtra-pontos-x-1 lista-pontos)
  (filter (lambda (ponto) (= (ponto-x ponto) 1)) lista-pontos))

(define (retorna-y-pontos-x-1 lista-pontos)
  (map (lambda (ponto) (ponto-y ponto)) (filtra-pontos-x-1 lista-pontos)))

(retorna-y-pontos-x-1 LISTA_PONTOS)

(define (retorna-soma-y-pontos-x-1 lista-pontos)
  (foldl + 0 (retorna-y-pontos-x-1 lista-pontos)))

(retorna-soma-y-pontos-x-1 LISTA_PONTOS)

(define (filtra-pontos-numero-x lista-pontos valor-filtro)
  (filter (lambda (ponto) (>= (ponto-x ponto) valor-filtro)) lista-pontos))

(filtra-pontos-numero-x LISTA_PONTOS 1)


;; ==== recursão generativa e terminação ====

;; Terminação : o caso trivial de para da função é para quando o resto da divisão dos valores
;; parametrizados for zero, o que eventualmente será verdade, uma vez que a cada chamada da função
;; ocorre uma atualização dos valores com o resto da divisão, diminuindo o valor

(define (mdc valor1 valor2)
  (cond
    [(= (remainder valor1 valor2) 0) 1]
    [else (* (remainder valor1 valor2)
             (mdc valor1 (remainder valor1 valor2)))]))

(mdc 3 2)