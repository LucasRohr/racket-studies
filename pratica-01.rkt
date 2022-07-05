;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pratica-01) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))

;; Este arquivo contém um modelo para a solução dos exercícios da
;; lista a ser resolvida no laboratório 1 de INF05008.
;; Este exercício não será entregue, é um exercício de fixação.

;; Em cada questão, há um início da solução. Deixamos essas linhas
;; sempre comentadas para não causar erros de compilação, pois
;; não estão completas (em muitos pontos há '...' que deve ser
;; completado por você).

;;===============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;;===============================================================

(define PULA_VEZ 10)
(define INVERTE 11)
(define COMPRA2 12)
(define CURINGA 40)
(define CURINGA_COMPRA4 42)

;; Questão: Por que não é necessário definir contratos, objetivos
;; ou exemplos aqui?

;; Resposta: pois constantes são valores imutáveis e objetivos, sendo claros seu propósito como valor fixo e sem diferentes retornos para testes.

;;===============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;;===============================================================

(define CIRCULO_BRANCO
  (circle 40 "solid" "white")
)

(define QUADRADOS_COLORIDOS
  (above
     (beside
        (rectangle 50 75 "solid" "red")
        (rectangle 50 75 "solid" "green")
      )
      (beside
        (rectangle 50 75 "solid" "yellow")
        (rectangle 50 75 "solid" "blue")
      )
   )
)

(define CONTORNO_PRETO
  (rectangle 110 160 "outline" "black")
)

; ...

;;===============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
;;===============================================================

;; ==> TRADUZ-COR

;; traduz-cor : String -> String
;; Objetivo: Dada uma cor, que pode ser  amarelo, verde, vermelho, azul ou preto,
;; retorna a respectiva cor em ingles, ou seja, "blue", "green", "yellow", "red" ou "black".

;; Exemplos:
;; (traduz-cor "azul") = "blue"
;; (traduz-cor "verde") = "green"

(define (traduz-cor nome-cor) ;; Dada uma cor nome-cor em português
  (cond
    ;; se nome-cor for "azul, devolver "blue"
    [(string=? nome-cor "azul") "blue"]
    ;; se nome-cor for "verde", devolver "green"
    [(string=? nome-cor "verde") "green"]
    ;; se nome-cor for "vermelho, devolver "red"
    [(string=? nome-cor "vermelho") "red"]
    ;; se nome-cor for "amarelol, devolver "yellow"
    [(string=? nome-cor "amarelo") "yellow"]
    ;; senão, devolver "black"
    [else "black"]))

;; Testes:
(check-expect (traduz-cor "vermelho") "red")
(check-expect (traduz-cor "verde") "green")
(check-expect (traduz-cor "preto") "black")

;;===============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;;===============================================================

;; ========================
;; ESCOLHE-FUNDO - versão 1
;; ========================

;; escolhe-fundo: String -> Imagem

;; Objetivo: Dada uma cor, que pode ser amarelo, verde, vermelho, azul ou preto
;; gera a imagem de fundo para uma carta de UNO desta cor.

;; Exemplos:
;; (escolhe-fundo "vermelho")  desenha o fundo de uma carta vermelha
;; (escolhe-fundo "preto")     desenha o fundo de uma carta curinga

(define (escolhe-fundo nome-cor) ;; dada uma cor ...
    (overlay 
        CIRCULO_BRANCO ;; circulo branco
        (cond
           ;; se a cor nome-cor for preto, desenhar os quadrados coloridos no fundo
           [(string=? nome-cor "preto")   QUADRADOS_COLORIDOS]
           ;; senão, desenhar um retângulo da cor desejada
           [else (rectangle 100 150 "solid" (traduz-cor nome-cor))]
        ) ;; retangulo da cor da carta
        CONTORNO_PRETO ;; contorno preto
     )
)

;; ========================
;; ESCOLHE-FUNDO - versão 2
;; ========================

;; escolhe-fundo-v2: String -> Imagem

;; Dada uma cor, que pode ser amarelo, verde, vermelho, azul ou preto
;; gera a imagem de fundo para uma carta de UNO desta cor.

;; Exemplos:
;; (escolhe-fundo "vermelho")  desenha o fundo de uma carta vermelha
;; (escolhe-fundo "preto")     desenha o fundo de uma carta curinga

(define (escolhe-fundo-v2 nome-cor) ;; Dada uma cor...
   (cond
        ;; se a cor COR for preto, desenhar os quadrados coloridos no fundo
        [(string=? nome-cor "preto") (overlay CIRCULO_BRANCO QUADRADOS_COLORIDOS CONTORNO_PRETO)]
        ;; senão, desenhar um retângulo da cor desejada
       [else (overlay CIRCULO_BRANCO (rectangle 100 150 "solid" (traduz-cor nome-cor)) CONTORNO_PRETO)]
    )
)

;; Testes:

(check-expect (escolhe-fundo-v2 "vermelho") (overlay  CIRCULO_BRANCO (rectangle 100 150 "solid" "red") CONTORNO_PRETO))
(check-expect (escolhe-fundo-v2 "preto") (overlay  CIRCULO_BRANCO QUADRADOS_COLORIDOS CONTORNO_PRETO))

;; Qual a diferença entre as versões desta função?

;; A primeira função evita repetir o código de overlay e encapsula a condicional dentro da própria chamada da função,
;; já a V2 possui condicional no escopo externo, fazendo haver repetição de código.

;;===============================================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
;;===============================================================

;; ========================
;; DESENHA-CARTAS
;; ========================

;; escolhe-simbolo: Número -> Imagem

;; Dado um número, que pode ser de 0 a 9 ou as constantes referentes às
;; cartas especiais de UNO, devolve uma imagem que representa este número na carta.

;; Exemplos:
;; (escolhe-simbolo 8) devolve a imagem do número oito
;; (escolhe-simbolo COMPRA2) devolve a imagem +2

(define (escolhe-simbolo numero-carta)
  (cond
      ;; se a carta for numérica, desenha a carta numérica
      [(and (>= numero-carta 0) (<= numero-carta 9)) (text (number->string numero-carta) 38 "black")]
      ;; se a carta for um COMPRA2, desenha +2
      [(= numero-carta COMPRA2) (text "+2" 38 "black")]
      ;; se a carta for um INVERTE, desenha «
      [(= numero-carta INVERTE) (text "«" 38 "black")]
      ;; se a carta for um PULA_VEZ, desenha Ø
      [(= numero-carta PULA_VEZ) (text "Ø" 38 "black")]
      ;; se a carta for um CURINGA, não desenha nada (devolve uma imagem vazia)
      [(= numero-carta CURINGA) (rectangle 0 0 "solid" "black")]
      ;; se a carta for um CURINGA_COMPRA4, devolve +4
      [(= numero-carta CURINGA_COMPRA4) (text "+4" 38 "black")]
   )
)

;; desenha-carta : Número String -> Imagem

;; Objetivo: Dados um número e uma cor, representando uma carta de UNO,
;; gera uma imagem para esta carta,

;; Exemplos:
;; (desenha-carta 4 "vermelha")  desenha a carta número 4 vermelha
;; (desenha-carta COMPRA4 "preta")    desenha a carta curinga compra 4

(define (desenha-carta numero-carta nome-cor)
  (overlay ;; sobrepor
      (escolhe-simbolo numero-carta) ;; o desenho do símbolo da carta
      (escolhe-fundo nome-cor)  ;; com o fundo da carta
  )
)

(desenha-carta 3 "azul")
(desenha-carta 11 "vermelho")
(desenha-carta 40 "preto")
(desenha-carta 42 "preto")

;;===============================================================
;; 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
;;===============================================================

;; jogada-válida? : Numero String Numero String -> Booleano

;; Objetivo: A função analiza duas cartas (representadas por 4 argumentos:um
;; número e uma string, representando a carta da mão e um número e uma string,
;; representando a carta da mesa, nesta ordem) e verifica se é possível
;; jogar uma sobre a outra, de acordo com as regras do UNO

;; Exemplos:
;;    (jogada-válida? 2 "vermelho" 4 "vermelho")=  #t
;;    (jogada-válida? 2 "vermelho" CURINGA "preto") = #t 
;;    (jogada-válida? CURINGA "preto" 2 "vermelho") = #t
;;    (jogada-válida? 2 "vermelho" 2 "verde") = #t
;;    (jogada-válida? 2 "vermelho" 3 "verde") = #f

(define (jogada-válida? numero-carta-mao cor-carta-mao numero-carta-mesa cor-carta-mesa)
   (or
    (or (string=? cor-carta-mao "preto") (string=? cor-carta-mesa "preto"))
    (or (= numero-carta-mao numero-carta-mesa) (string=? cor-carta-mao cor-carta-mesa))
   )
)

;; Testes:
   (check-expect (jogada-válida? 3 "azul" 3 "verde")  #t)
   (check-expect (jogada-válida? 8 "verde" CURINGA "preto")  #t)
   (check-expect (jogada-válida? CURINGA_COMPRA4 "preto" PULA_VEZ "amarelo")  #t)
   (check-expect (jogada-válida? INVERTE "azul" INVERTE "verde")  #t)
   (check-expect (jogada-válida? 8 "verde" PULA_VEZ "amarelo")  #f)
   (check-expect (jogada-válida? PULA_VEZ "amarelo" 5 "vermelho")  #f)
   (check-expect (jogada-válida? 8 "verde" 5 "vermelho")  #f)

;;===============================================================
;; 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
;;===============================================================

;; mostra-jogada: Numero String Numero String -> Imagem

;; Objetivo: A função analiza duas cartas (representadas por 4 argumentos:um
;; número e uma string, representando a carta da mesa e um número e uma string,
;; representando a carta da mão, nesta ordem) e verifica se é possível
;; jogar uma sobre a outra, de acordo com as regras do UNO, desenhando uma
;; imagem mostrando as cartas e se é possível fazer a jogada ou não.

(define (mostra-jogada numero-carta-mao cor-carta-mao numero-carta-mesa cor-carta-mesa)
  (above
     (cond
        [(boolean=? (jogada-válida? numero-carta-mao cor-carta-mao numero-carta-mesa cor-carta-mesa) #true) (text "Jogada válida! É possível jogar a carta!" 16 "darkgreen")]
        [else (text "Jogada inválida! Não é possível jogar a carta!" 16 "darkred")]
      )
     (rectangle 10 15 "solid" "white")
     (beside
       (above
         (text "Carta na mão:" 16 "black")
         (rectangle 10 15 "solid" "white")
         (desenha-carta numero-carta-mao cor-carta-mao)
       )

       (rectangle 20 10 "solid" "white")

       (above
         (text "Carta na mesa:" 16 "black")
         (rectangle 10 15 "solid" "white")
         (desenha-carta numero-carta-mesa cor-carta-mesa)
       )
     )
     (rectangle 10 20 "solid" "white")
   )
)

(mostra-jogada 3 "azul" 3 "verde")
(mostra-jogada 8 "verde" CURINGA "preto")
(mostra-jogada 8 "verde" PULA_VEZ "amarelo")

