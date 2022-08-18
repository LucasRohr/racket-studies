;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pratica-03) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; ========================================================================
;;                        DEFINIÇÕES DE DADOS
;; ========================================================================
;;  
;; CONSTANTES:

(define PULA_VEZ -1)
(define COMPRA2 -2)
(define INVERTE -3)
(define CURINGA -5)
(define CURINGA_COMPRA4 -4)

;; -----------------
;; TIPO CARTA:
;; -----------------
(define-struct carta (cor valor))  
;; Um elemento do conjunto Carta é
;;   (make-carta c v)
;;   c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo", "vermelho" ou "preto" ou "livre"
;;   v : Número, é o valor da carta, que pode ser qualquer inteiro entre 0 e 9,
;;               ou um número negativo -1 (PulaVez), -2 (Compra2), -3 (Inverte),-4 (Compra4) ou -5 (Curinga)

;; Definição de constantes:

(define AZUL3  (make-carta "azul" 3))
(define CARTA_CURINGA  (make-carta "preto" CURINGA))
(define CARTA_CURINGA_COMPRA4  (make-carta "preto" CURINGA_COMPRA4))
(define VERMELHO5 (make-carta "vermelho" 5))
(define VERMELHO-COMPRA2 (make-carta "vermelho" -2))
(define VERDE3 (make-carta "verde" 3))
(define VERDE_INVERTE (make-carta "amarelo" -3))

;; ========================================================================
;;                                 QUESTÃO 1
;; Definição de listas de cartas e jogador.
;; =========================================================================

;; --------------------
;; TIPO LISTA DE CARTAS:
;; --------------------

;; Uma ListaDeCartas pode ser
;; 1. vazia (empty) ou
;; 2. (cons c l), onde
;;    c : Carta
;;    l : ListasDeCartas

(define LISTA_CARTAS_1 (list VERDE_INVERTE CARTA_CURINGA VERMELHO5 VERDE3))
(define LISTA_CARTAS_2 (list VERMELHO5 VERDE3 AZUL3 VERDE3))
(define LISTA_CARTAS_3 (list VERMELHO-COMPRA2 AZUL3 VERMELHO5 VERDE_INVERTE))
(define LISTA_CARTAS_4 (list CARTA_CURINGA VERMELHO-COMPRA2 VERMELHO-COMPRA2 VERDE3))

;; -----------------
;; TIPO JOGADOR:
;; -----------------

(define-struct jogador (nome cartas pontos))
;; Um elemento do conjunto Jogador é
;;   (make-jogador n c p), onde:
;;   n : String, nome do jogador
;;   c : ListaDeCartas, lista de cartas do jogador
;;   p : Número, pontuação do jogador

(define JOGADOR_1 (make-jogador "Malaquias" LISTA_CARTAS_1 20))
(define JOGADOR_2 (make-jogador "Matias" LISTA_CARTAS_3 40))

;; ========================================================================
;;                                 QUESTÃO 2
;; Dada uma lista de cartas, devolve as imagens das cartas desta lista, lado a lado.
;; ========================================================================

;; desenha-cartas : ListaDeCartas -> Imagem
;; Obj: dada uma lista de cartas, desenha as mesmas lado a lado
;; Exemplos:
;; (desenha-cartas LISTA_CARTAS_1) = desenha a lista 1
;; (desenha-cartas LISTA_CARTAS_2) = desenha a lista 2

;; Dada uma lista lista-cartas
(define (desenha-cartas lista-cartas)
  (cond
    ;; Se a lista estiver vazia, então devolve uma imagem vazia
    [(empty? lista-cartas) empty-image]
    ;; Senão, desenha a primeira carta da lista chamando a função desenha-carta
    ;; ao lado das imagens das cartas restantes na lista
    [else
     (beside
      (desenha-carta (first lista-cartas))
      (desenha-cartas (rest lista-cartas)))]))

;; desenha-carta : Carta -> Imagem
;; Objetivo: A função cria uma imagem para a carta que é recebida de entrada.
;; Exemplo:
;;   (desenha-carta (make-carta "azul" 3)) desenha a carta de UNO 3 azul
(define (desenha-carta carta)
  (overlay ;; sobrepor:
              ;; texto da carta:
              (desenha-texto (carta-valor carta))
              ;; círculo branco:
              (circle 45 "solid" "white")
              ;; retângulo da cor da carta:
              (rectangle 100 150 "solid" (cor carta))
              ;; contorno preto:
              (rectangle 110 160 "outline" "black")))

;; desenha-texto-valor: Número -> Imagem
;; Dado um valor que representa o valor de uma carta UNO, gerar a imagem correspondente.
;; Exemplo:
;;   (desenha-texto-valor COMPRA2) gera a imagem da string "+2" em preto com fonte 70
(define (desenha-texto valor)
  (text (cond ;; escolher o símbolo a ser desenhado, dependendo do valor:
               [(= valor CURINGA_COMPRA4) "+4"]
               [(= valor COMPRA2)         "+2"]
               [(= valor INVERTE)         "«"]
               [(= valor PULA_VEZ)        "Ø"]
               [(= valor CURINGA)         "T"]
               [else (number->string valor)])
        ;; o texto será desenhado em tamanho 70 em preto
        70 "black"))

;; cor : Carta -> String
;; Objetivo: a função recebe uma carta UNO, e retorna a respectiva cor
;; em ingles, ou seja, "blue", "green", "yellow", "red" ou "black".
;; Exemplos/testes:
     (check-expect (cor (make-carta "azul" 3))  "blue")
     (check-expect (cor (make-carta "verde" 3)) "green")
     (check-expect (cor (make-carta "beje" 3))  "black")
(define (cor c)
  (cond
    [(string=? "azul" (carta-cor c)) "blue"]
    [(string=? "verde" (carta-cor c)) "green"]
    [(string=? "vermelho" (carta-cor c)) "red"]
    [(string=? "amarelo" (carta-cor c)) "yellow"]
    [else "black"]))


(desenha-cartas LISTA_CARTAS_1)
(desenha-cartas LISTA_CARTAS_2)

;; ========================================================================
;;                                 QUESTÃO 3
;; Dada uma lista de cartas e uma carta da mesa, dizer quantas opções de jogadas
;; há nesta lista.
;; ========================================================================

;; número-opções : ListaDeCartas Carta -> Número
;; Obj: dada uma lista de cartas e uma carta da mesa, itera sobre a lista de cartas
;; para verificar quais jogadas são válidas e retorna quantas são
;; Exemplos:
;; (número-opções LISTA_CARTAS_1 VERMELHO5) = 2
;; (número-opções LISTA_CARTAS_2 CURINGA) = 4

(define (número-opções lista-cartas carta-mesa)
  (cond
    ;; Se a lista estiver vazia, então devolve zero para soma
    [(empty? lista-cartas) 0]
    ;; Se a jogada da primeira carta for válida, soma uma unidade ao restante do total de jogadas válidas
    [(jogada-válida? (first lista-cartas) carta-mesa) (+ 1 (número-opções (rest lista-cartas) carta-mesa))]
    ;; Senão, retorna o total atual de jogadas válidas sem alterar o valor, pois a jogada é inválida
    [else (número-opções (rest lista-cartas) carta-mesa)]))

;; Testes:
(check-expect (número-opções LISTA_CARTAS_1 VERMELHO5) 2)
(check-expect (número-opções LISTA_CARTAS_2 CARTA_CURINGA) 4)

;; jogada-válida? : Carta Carta -> Booleano
;; Objetivo: Dada uma carta da mao e uma da mesa, nesta ordem,
;; verifica se é possível jogar a carta da mão, de acordo com as regras do UNO
;; Exemplos/testes:
     (check-expect (jogada-válida? (make-carta "azul" 3) (make-carta "azul" 4))  #t)
     (check-expect (jogada-válida? (make-carta "azul" 3) (make-carta "preto" CURINGA_COMPRA4))  #t)
     (check-expect (jogada-válida? (make-carta "azul" 3) (make-carta "vermelho" 4))  #f)
(define (jogada-válida? carta-mao carta-mesa)
     (cond
          ;; se a carta da mão for preta, a jogada é válida
          [(string=? (carta-cor carta-mao) "preto") #t]
          ;; se a carta da mesa for preta, a jogada é válida
          [(string=? (carta-cor carta-mesa) "preto") #t] 
          ;; se as duas cartas forem da mesma cor ou do mesmo tipo/valor, a jogada é válida
          [(or (string=? (carta-cor carta-mesa)(carta-cor carta-mao))
               (= (carta-valor carta-mesa) (carta-valor carta-mao))) #t]
          ;; senão, a jogada é inválida
          [else #f] ))


;; ========================================================================
;;                                 QUESTÃO 4
;; Dada uma lista de cartas, devolve a soma dos pontos das cartas desta lista.
;; ========================================================================

;; Constantes:

(define PONTOS_ESPECIAL 20)
(define PONTOS_CURINGA 50)

(define (verifica-carta-especial carta)
  (or (= (carta-valor carta) PULA_VEZ) (= (carta-valor carta) COMPRA2) (= (carta-valor carta) INVERTE))
)

;; soma-pontos : ListaDeCartas -> Número
;; Obj: dada uma lista de cartas, devolve a soma dos pontos das cartas
;; de acordo com as regras do Uno
;; Exemplos:
;; (soma-pontos LISTA_CARTAS_1) = 2
;; (soma-pontos LISTA_CARTAS_2) = 4

(define (soma-pontos lista-cartas)
  (cond
    ;; Se a lista estiver vazia, então devolve zero para soma
    [(empty? lista-cartas) 0]
    ;; Se a primeira carta for numérica, retorna o valor da carta somado ao valor do resto da soma
    [(and (>= (carta-valor (first lista-cartas)) 0) (<= (carta-valor (first lista-cartas)) 9)) (+ (carta-valor (first lista-cartas)) (soma-pontos (rest lista-cartas)))]
     ;; Se a primeira carta for especial, retorna o valor 20 somado ao valor do resto da soma
    [(verifica-carta-especial (first lista-cartas)) (+ PONTOS_ESPECIAL (soma-pontos (rest lista-cartas)))]
    ;; Senão, retorna o valor 50 somado ao valor do resto da soma, por ser carta curinga
    [else (+ PONTOS_CURINGA (soma-pontos (rest lista-cartas)))]))

;; Testes:
(check-expect (soma-pontos LISTA_CARTAS_1) 78)
(check-expect (soma-pontos LISTA_CARTAS_2) 14)

;; ========================================================================
;;                                 QUESTÃO 5 
;;  Construa a função que, dados um jogador e uma carta da mesa, nesta ordem, retorna uma lista com as cartas do
;;  jogador que podem ser jogadas, de acordo com as regras do Uno. 
;; =========================================================================

;; monta-jogador-resto-cartas : Jogador -> Jogador
;; Obj: dado um jogador, monta um novo jogador usando o resto das cartas do passado
;; Exemplos:
;; (monta-jogador-resto-cartas JOGADOR_1)
(define (monta-jogador-resto-cartas jogador)
  (make-jogador
   (jogador-nome jogador)
   (rest (jogador-cartas jogador))
   (jogador-pontos jogador)))

(check-expect (monta-jogador-resto-cartas JOGADOR_1)
              (make-jogador "Malaquias" (list CARTA_CURINGA VERMELHO5 VERDE3) 20))

;; cartas-válidas : Jogador Carta -> ListaDeCartas
;; Dado um jogador e uma carta da mesa, retorna uma lista de cartas válidas do jogador para a jogada
;; Exemplos:
;; (cartas-válidas JOGADOR_1 VERMELHO5)
;; (cartas-válidas JOGADOR_2 CARTA_CURINGA)
(define (cartas-válidas jogador carta-mesa)
  (cond
    ;; Se a lista estiver vazia, então devolve um valor vazio
    [(empty? (jogador-cartas jogador)) empty]
    ;; Se a jogada for válida, adiciona a primeira carta do jogador ao restante da lista
    [(jogada-válida? (first (jogador-cartas jogador)) carta-mesa)
     (cons (first (jogador-cartas jogador)) (cartas-válidas (monta-jogador-resto-cartas jogador) carta-mesa))]
    ;; Senão, retorna a lista atual de cartas
    [else (cartas-válidas (monta-jogador-resto-cartas jogador) carta-mesa)]
  )
)

;;Testes:
(check-expect (cartas-válidas JOGADOR_1 VERMELHO5) (list CARTA_CURINGA VERMELHO5))
(check-expect (cartas-válidas JOGADOR_2 CARTA_CURINGA) (list VERMELHO-COMPRA2 AZUL3 VERMELHO5 VERDE_INVERTE))

;; ========================================================================
;;                                 QUESTÃO 6
;; Defina um tipo de dado CartaPtLm que deve armazenar uma carta, sua pontuação associada e um número.
;; Defina o tipo de dado ListaCartaPtLm que contém todas as listas (finitas) de CartaPtLm.
;; Defina duas constantes do tipo ListaCartaPtLm.
;; =========================================================================

;; -----------------
;; TIPO CARTAPTLM
;; -----------------
(define-struct cartaptlm (carta pontuação número)) 
;; Um elemento do conjunto CartaPtLm é
;;   (make-cartaptlm c p l), onde:
;;   c : Carta, uma carta
;;   p : Número, a pontuação associada
;;   l : Número, número limite da carta

;; --------------------
;; TIPO LISTA DE CARTAPTLM:
;; --------------------

;; Uma ListaCartaPtLm pode ser
;; 1. vazia (empty) ou
;; 2. (cons c l), onde
;;    c : CartaPtLm
;;    l : ListaCartaPtLm

(define LISTA_LIMITES_1 (list (make-cartaptlm VERMELHO5 5 9) (make-cartaptlm VERDE3 3 9)))
(define LISTA_LIMITES_2 (list (make-cartaptlm CURINGA 50 20) (make-cartaptlm VERDE3 3 9)))

;; ========================================================================
;;                                 QUESTÃO 7
;; Construa a função cartas-pontos-limite que, dados um jogador e um número limite, nesta ordem,
;; retorna uma lista com as cartas do jogador com sua pontuação associada e o número limite, que possuem pontuação menor que número limite.
;; Caso o jogador não tenha nenhuma carta com pontuação menor que número limite, a função deve retornar
;; a mensagem "Não possui cartas com pontuação menor que limite.", onde limite é valor passado para a função. 
;; =========================================================================

;; -----------------
;; TIPO ResultadoCartasPontosLimite
;; -----------------

;; Um elemento do conjunto ResultadoCartasPontosLimite é:
;; i) uma ListaCartaPtLm ou
;; ii) uma String

(define PONTUAÇÃO_CARTA_ESPECIAL 20)
(define PONTUAÇÃO_CARTA_CURINGA 50)

;; retorna-pontuação-carta: Carta -> Número
;; obj: Dada uma estrutura do tipo Carta, devolve a pontuação conforme o tipo da carta,
;; numérica ou especial, auxiliando a função pontuação

;; Exemplos:
;; (retorna-pontuação-carta (make-carta "azul" 3)) = 3
;; (retorna-pontuação-carta (make-carta "vermelho" -2)) = 20

(define (retorna-pontuação-carta carta)
  (cond
    [(and (>= (carta-valor carta) 0) (<= (carta-valor carta) 9)) (carta-valor carta)]
    [(or (= (carta-valor carta) PULA_VEZ)
         (= (carta-valor carta) COMPRA2)
         (= (carta-valor carta) INVERTE))
     PONTUAÇÃO_CARTA_ESPECIAL]
    [else PONTUAÇÃO_CARTA_CURINGA]))

;; Testes:

(check-expect (retorna-pontuação-carta AZUL3) 3)
(check-expect (retorna-pontuação-carta VERMELHO-COMPRA2) 20)

(define (valida-cartas-limite-jogador jogador limite)
  (cond
    ;; Se a lista estiver vazia, então devolve um valor vazio
    [(empty? (jogador-cartas jogador)) empty]
    ;; Se a pontuação da carta
    [(< (retorna-pontuação-carta (first (jogador-cartas jogador))) limite)
     (cons
      (make-cartaptlm (first (jogador-cartas jogador)) (retorna-pontuação-carta (first (jogador-cartas jogador))) limite)
      (valida-cartas-limite-jogador (monta-jogador-resto-cartas jogador) limite))]
    [else (valida-cartas-limite-jogador (monta-jogador-resto-cartas jogador) limite)]
  )
)

;; cartas-pontos-limite : Jogador Número -> ResultadoCartasPontosLimite
;; Obj: dado um jogador e um número de limite, retorna as cartas do jogador
;; que possuem limite maior que o passado ou uma mensagem caso não existam
;; Exemplos:
;; (cartas-pontos-limite JOGADOR_1 10)

(define (cartas-pontos-limite jogador limite)
  (cond
    [(not (empty? (valida-cartas-limite-jogador jogador limite))) (valida-cartas-limite-jogador jogador limite)]
    [else (string-append "Não possui cartas com pontuação menor que limite " (number->string limite))]
  )
)

(cartas-pontos-limite JOGADOR_1 10)