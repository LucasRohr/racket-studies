;; Revisão para Prova 1:

;; Declaração de Constantes:

(define PI 3.14)
(define NOME_JOGADOR "Afonso")

;; Funções base de cálculo

;; calcula-com-pi-sem-divisor : Número -> Número
;; Objetivo : dado um valor, multiplica PI por este valor e divide por 3

;; Exemplos:
;; (calcula-com-pi-sem-divisor 3) = PI
;; (calcula-com-pi-sem-divisor 1) = 1.046

(define (calcula-com-pi-sem-divisor multiplicador)
  (/ (* PI multiplicador) 3))

;; Testes:

(check-expect (calcula-com-pi-sem-divisor 3) PI)
(check-expect (calcula-com-pi-sem-divisor 1) 1.046)

;; calcula-com-pi : Número Número -> Número
;; Objetivo : dado um valor multiplicador e um valor divisor,
;; multiplica PI pelo multiplicador e divide pelo divisor, sendo
;; uma generalização da função calcula-com-pi-sem-divisor

;; Exemplos:
;; (calcula-com-pi 3 5) = 1.884
;; (calcula-com-pi 2 2) = PI

(define (calcula-com-pi multiplicador divisor)
  (/ (* PI multiplicador) divisor))

;; Testes:

(check-expect (calcula-com-pi 3 5) 1.884)
(check-expect (calcula-com-pi 2 2) PI)

(calcula-com-pi-sem-divisor 1)
(calcula-com-pi 3 5)

;; Tipos Base:

;; Número = 2, 5.4, 0
;; String = "juvenaldo"
;; Booleano = #true e #false
;; Imagem

;; Imagens

(define (monta-carta-uno valor cor)
  (overlay
    (text (number->string valor) 20 "white")
    (rectangle 90 140 "solid" cor)
    (rectangle 100 150 "outline" "black")))

(monta-carta-uno 7 "blue")


;; Testes de tipo:

(define TESTE_BOOLEANO_1 (= 2 1))
(define TESTE_BOOLEANO_2 (string=? NOME_JOGADOR NOME_JOGADOR))

(and  TESTE_BOOLEANO_1 TESTE_BOOLEANO_2)
(or  TESTE_BOOLEANO_1 TESTE_BOOLEANO_2)

;; Condicionais

(define (par-ou-impar valor)
  (cond
    [(even? valor) "Par"]
    [else "Impar"]))

(par-ou-impar 2)
(par-ou-impar 3)


;; Estruturas

;; == Tipo Jogador ==

(define-struct jogador (nome idade pontos possui-especial))

;; Um elemento do conjunto Jogador tem o formato:
;; (make-jogador n i p pe), onde:

;; n : String, nome do jogador
;; i : Número, idade do jogador
;; p : Número, pontos acumulados do jogador
;; pe : Booleano, indicador para caso o jogador tenha especial

(define JOGADOR-1 (make-jogador "cleyton rasta" 19 42 #false))

(define (verifica-especial-jogador jogador pontos-extras)
  (> (+ (jogador-pontos jogador) pontos-extras) 50)
)

(define (atualiza-pontos-jogador jogador pontos-extras)
  (make-jogador
   (jogador-nome jogador)
   (jogador-idade jogador)
   (+ (jogador-pontos jogador) pontos-extras)
   (verifica-especial-jogador jogador pontos-extras)
  )
)

(atualiza-pontos-jogador JOGADOR-1 9)



;; === ResultadoDeJogada ===

;; Um elemento do conjunto ResultadoDeJogada é:
;; i) um Jogador, ou
;; ii) uma String

;; Definição de constante:

(define MENSAGEM-INVALIDA "Jogada mal sucedida")

;; faz-jogada : Jogador Número -> ResultadoDeJogada
;; Objetivo : dado um jogador e os pontos da jogada, usa os pontos do jogador
;; para verificar se a jogada é válida ou não, retornando o jogador se sim e
;; uma mensagem se não

;; Exemplos:
;; (faz-jogada JOGADOR-1 41) = JOGADOR-1
;; (faz-jogada JOGADOR-1 50) = MENSAGEM-INVALIDA

(define (faz-jogada jogador pontos-jogada)
  (cond
    [(> (jogador-pontos jogador) pontos-jogada) jogador]
    [else MENSAGEM-INVALIDA]))

(check-expect (faz-jogada JOGADOR-1 41) JOGADOR-1)
(check-expect (faz-jogada JOGADOR-1 50) MENSAGEM-INVALIDA)



;; Listas e recursão

(define LISTA-1 (cons 4 (cons 5 (cons -2 empty))))
(define LISTA-2 (list "joao" "lungas" "matias"))

(define (lista-tem-nome? lista-nomes nome-busca)
  (cond
    ;; Se a lista estiver vazia, então retornar falso
    [(empty? lista-nomes) #false]
    ;; Se o primeiro nome da lista for igual ao nome buscaso, então retornar verdadeiro
    [(string=? (first lista-nomes) nome-busca) #true]
    ;; Senão, continuar a busca com o resto da lista de nomes
    [else (lista-tem-nome? (rest lista-nomes) nome-busca)]))


(lista-tem-nome? LISTA-2 "kleber")
(lista-tem-nome? LISTA-2 "matias")


;; Cenas:


;; Uma Cena (Scene) pode ser:

;; 1. vazia (empty-scene larg alt), onde:
;; larg : Número
;; alt : Número

;; 2. (place-scene img larg alt cena), onde:
;; img : imagem
;; larg : Número
;; alt : Número
;; cena : Cena


(define SOL (circle 50 "solid" "yellow"))

(define CENA (empty-scene 500 300))

(define (insere-sol-posicao cena numero-sois coord-x coord-y)
  (cond
    [(= numero-sois 0) (place-image empty-image 0 0 cena)]
    [else (insere-sol-posicao
           (place-image SOL coord-x coord-y cena)
           (- numero-sois 1)
           (+ coord-x 120)
           coord-y)]))

(define (desenha-sois numero-sois coord-x cena)
  (cond
    [(= numero-sois 0) empty-image]
    [else (insere-sol-posicao cena numero-sois 100 100)]))


(desenha-sois 3 100 CENA)




;; === Revisão Lab 3 ===

;; -----------------
;; TIPO CARTA:
;; -----------------
(define-struct carta (cor valor))
;; Um elemento do conjunto Carta é
;; (make-carta c v) onde:
;; c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo", "vermelho" ou "preto" ou "livre"
;; v : Número, é o valor da carta, que pode ser qualquer inteiro entre 0 e 9,ou um número negativo:
;; -1 (PulaVez), -2 (Compra2), -3 (Inverte),-4 (Curinga-Compra4) ou -5 (Curinga)


;; === Tipo ListaDeCartas ===

;; Uma ListaDeCartas pode ser:
;; i) vazio (empty) ou
;; ii) (cons c l), onde:
;;     c : Carta
;;     l : ListaDeCartas

(define-struct jogador-uno (nome mão pontos))
;; Um elemento do conjunto JogadorUno é:
;;  (make-jogado n m p), onde:
;;   n : String, nome
;;   m : ListaDeCartas, as cartas do jogador
;;   p : Número, pontos

(define CARTA-1 (make-carta "blue" 2))
(define CARTA-2 (make-carta "red" -2))
(define CARTA-3 (make-carta "yellow" 7))
(define CARTA-4 (make-carta "red" -1))
(define CARTA-5 (make-carta "black" 0))

(define LISTA-CARTAS-1 (list CARTA-1 CARTA-2 CARTA-3))


(define (desenha-carta carta)
  (overlay
    (text (number->string (carta-valor carta)) 20 "white")
    (rectangle 90 140 "solid" (carta-cor carta))
    (rectangle 100 150 "outline" "black")))

(define (desenha-cartas lista-cartas)
  (cond
    ;; Se a lista de cartas estiver vazia, então retornar uma imagem vazia
    [(empty? lista-cartas) empty-image]
    ;; Senão, desenhar a primeira carta ao lado dos desenhos do resto das cartas
    [else (beside (desenha-carta (first lista-cartas)) (desenha-cartas (rest lista-cartas)))]))


(desenha-cartas LISTA-CARTAS-1)


(define (jogada-válida? carta-mao carta-mesa)
  (cond
    [(or (string=? (carta-cor carta-mao) "black") (string=? (carta-cor carta-mesa) "black")) #true]
    [else (or
           (string=? (carta-cor carta-mao) (carta-cor carta-mesa))
           (= (carta-valor carta-mao) (carta-valor carta-mesa)))]
  )
)

(jogada-válida? CARTA-2 CARTA-4)
(jogada-válida? CARTA-5 CARTA-4)
(jogada-válida? CARTA-1 CARTA-2)