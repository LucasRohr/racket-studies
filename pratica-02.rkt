;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pratica-02) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; =======================================================================
;;  FUNÇÕES DEFINIDAS NO LABORATÓRIO 1:
;; =======================================================================

;; ==> DESENHA-CARTAS
;; desenha-carta : Número String -> Imagem
;; Objetivo: Dados um número e uma cor, representando uma carta de UNO,
;; gera uma imagem para esta carta,
;; Exemplos:
;; (desenha-carta 4 "vermelha")  desenha a carta número 4 vermelha
;; (desenha-carta COMPRA4 "preta")    desenha a carta curinga compra 4
(define (desenha-carta *num *cor)
  (overlay ;; sobrepor
          (escolhe-simbolo *num)  ;; o desenho do símbolo da carta
          (escolhe-fundo *cor)))  ;; com o fundo da carta

;; ==> ESCOLHE-SIMBOLO:
;; escolhe-simbolo: Número -> Imagem
;; Dado um número, que pode ser de 0 a 9 ou as constantes referentes às
;; cartas especiais de UNO, devolve uma imagem que representa este número na carta.
;; Exemplos/testes:
      (check-expect (escolhe-simbolo 8) (text (number->string 8) 70 "black"))
      (check-expect (escolhe-simbolo COMPRA2) (text "+2" 60 "black"))

(define (escolhe-simbolo *num)
  (cond
      ;; se a carta for numérica,     desenha a carta numérica
      [(and (>= *num  0) (<= *num 9))  (text (number->string *num) 70 "black")]
      ;; se a carta for um COMPRA2, desenha +2
      [(= *num COMPRA2)   (text "+2" 60 "black")]
      ;; se a carta for um INVERTE, desenha «
      [(= *num INVERTE)   (text "«" 60 "black")]    
      ;; se a carta for um PULA_VEZ, desenha Ø
      [(= *num PULA_VEZ)   (text "Ø" 60 "black")] 
      ;; se a carta for um CURINGA, não desenha nada (devolve uma imagem vazia)
      [(= *num CURINGA)   empty-image] 
      ;; se a carta for um CURINGA_COMPRA4, devolve +4
      [(= *num CURINGA_COMPRA4)  (text "+4" 60 "black")]))

;; ==> ESCOLHE-FUNDO:
;; escolhe-fundo: String -> Imagem
;; Dada uma cor, que pode ser amarelo, verde, vermelho, azul ou preto
;; gera a imagem de fundo para uma carta de UNO desta cor.
;; Exemplos:
;;    (escolhe-fundo "vermelho")  desenha o fundo de uma carta vermelha
;;    (escolhe-fundo "preto")     desenha o fundo de uma carta curinga

(define (escolhe-fundo *cor);; Dada uma cor *cor
    (overlay 
        CIRCULO_BRANCO ;; circulo branco
        (cond
           ;; se a cor COR for preto, desenhar os quadrados coloridos no fundo
           [(string=? *cor "preto") QUADRADOS_COLORIDOS]
           ;; senão, desenhar um retângulo da cor desejada
           [else (rectangle 100 150 "solid" (traduz-cor *cor))])  ;; retangulo da cor da carta
        CONTORNO_PRETO));; contorno preto

;; ==> TRADUZ-COR
;; traduz-cor : String -> String
;; Objetivo: a função recebe uma cor de carta UNO, e retorna a respectiva cor
;; em ingles, ou seja, "blue", "green", "yellow", "red" ou "black".
;; Exemplos/testes:
     (check-expect (traduz-cor "azul") "blue")
     (check-expect (traduz-cor "verde") "green")

(define (traduz-cor uma-cor) ;; Dada uma cor uma-cor
  (cond
    ;; se uma-cor for "azul, devolver "blue"
    [(string=? "azul" uma-cor) "blue"]
    ;; se C for "verdel, devolver "green"
    [(string=? "verde" uma-cor) "green"]
    ;; se C for "vermelho, devolver "red"
    [(string=? "vermelho" uma-cor) "red"]
    ;; se C for "amarelol, devolver "yellow"
    [(string=? "amarelo" uma-cor) "yellow"]
    ;; senão, devolver "black"
    [else "black"]))




;; ===============================================================
;;                   DEFINIÇÕES DE DADOS
;; ===============================================================

;; Definição de constantes:

;; Constantes para as cartas especiais:
(define PULA_VEZ -1)
(define COMPRA2 -2)
(define INVERTE -3)
(define CURINGA -5)
(define CURINGA_COMPRA4 -4)

;; Constantes do tipo Imagem:
(define CIRCULO_BRANCO (circle 40 "solid""white"))
(define QUADRADOS_COLORIDOS
         (above                                  
            (beside (rectangle 50 75 "solid" "red")
                    (rectangle 50 75 "solid" "green"))
            (beside (rectangle 50 75 "solid" "yellow")
                    (rectangle 50 75 "solid" "blue"))))
(define CONTORNO_PRETO (rectangle 110 160 "outline" "black"))

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================

;; -----------------
;; TIPO Carta:
;; -----------------

(define-struct carta (cor valor))

;; Um elemento do conjunto Carta é
;;   (make-carta cor valor)     onde
;;   cor : String, é a cor da carta, que pode ser "azul", "verde", "amarelo" ou "vermelho" 
;;   valor : Número, é o valor numérico que representa a carta

;; Constantes do tipo Carta:
(define AZUL3  (make-carta "azul" 3))
(define VERMELHO5 (make-carta "vermelho" 5))
(define VERMELHO-COMPRA2 (make-carta "vermelho" -2))
(define VERDE3 (make-carta "verde" 3))

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================

;; -----------------
;; TIPO CartaUNO:
;; -----------------

;; Um elemento do conjunto CartaUNO é:
;; i) um Carta ou
;; ii) um Número, sendo este -5 ou -4

;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ==============================================================

;; jogada-válida? : CartaUNO, CartaUNO  -> Booleano

;; Objetivo: A função analiza duas cartas e verifica se é possível
;; jogar uma sobre a outra, de acordo com as regras do UNO

;; Exemplos:
;;    (check-expect (jogada-válida?  AZUL3 VERDE3)   #t)
;;    (check-expect (jogada-válida?  AZUL3 VERMELHO5) #f)
;;    (check-expect (jogada-válida?  VERMELHO5 VERMELHO-COMPRA2) #t)
;;    (check-expect (jogada-válida?  VERMELHO-COMPRA2 CURINGA)   #t)

(define (jogada-válida? @carta-mesa @carta-mao)
     (cond
          ;; se a carta da mão for curinga, a jogada é válida
          [(number? @carta-mao) #true]
          ;; se a carta da mesa for curinga, a jogada é válida
          [(number? @carta-mesa) #true]
          ;; se as duas cartas forem da mesma cor ou do mesmo tipo/valor, a jogada é válida
          [
            (and
               (and (carta? @carta-mesa) (carta? @carta-mao))
               (or (= (carta-valor @carta-mao) (carta-valor @carta-mesa)) (string=? (carta-cor @carta-mao) (carta-cor @carta-mesa)))
            )

            #true
          ]
          ;; senão, a jogada é inválida
          [else #false]
     )
)

;; Testes:

(check-expect (jogada-válida?  AZUL3 VERDE3)   #t)
(check-expect (jogada-válida?  AZUL3 VERMELHO5) #f)
(check-expect (jogada-válida?  VERMELHO5 VERMELHO-COMPRA2) #t)
(check-expect (jogada-válida?  VERMELHO-COMPRA2 CURINGA)   #t)


;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;; ==============================================================

;; Uma  CartaUNOouString é
;; 1. CartaUNO, ou
;; 2. String

;; próxima-carta: CartaUNO -> CartaUNOouString
;; obj: Dado uma estrutura de carta, retorna a próxima de mesma cor caso o valor for de 0 até 8, retornando 0 caso 8, e retorna invalidez caso seja especial
;; Exemplos/testes:
    ;; (check-expect (próxima-carta AZUL3)  (make-carta "azul" 4))
    ;; (check-expect (próxima-carta (make-carta "verde" 9))  (make-carta "verde" 0) )
    ;; (check-expect (próxima-carta VERMELHO-COMPRA2)  "Não é possível definir a próxima carta." )
    ;; (check-expect (próxima-carta CURINGA)  "Não é possível definir a próxima carta.")

(define (próxima-carta carta)
        (cond
          ;; se a carta for curinga, devolver....
          [(number? carta) "Não é possível definir a próxima carta."]

          ;; se a carta for especial de alguma cor, devolver ....
          [(or (= (carta-valor carta) PULA_VEZ) (= (carta-valor carta) COMPRA2) (= (carta-valor carta) INVERTE)) "Não é possível definir a próxima carta."]
          
          ;; senão, retorna a próxima estrutura de carta
          [else
            (make-carta
              (carta-cor carta)
              (cond
                 [(and (>= (carta-valor carta) 0) (<= (carta-valor carta) 8)) (+ (carta-valor carta) 1)]
                 [else 0]
              )
            )
          ]
        )
)

;; Testes:

(check-expect (próxima-carta AZUL3)  (make-carta "azul" 4))
(check-expect (próxima-carta (make-carta "verde" 9))  (make-carta "verde" 0) )
(check-expect (próxima-carta VERMELHO-COMPRA2)  "Não é possível definir a próxima carta." )
(check-expect (próxima-carta CURINGA)  "Não é possível definir a próxima carta.")

;; ==============================================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 
;; ==============================================================

;; Constante LIVRE:
(define LIVRE "Livre")

;; -----------------
;; TIPO CartaMão:
;; -----------------

;; Um elemento do conjunto CartaUNO é:
;; i) um CartaUNO ou
;; ii) um String, sendo esta a constante LIVRE

;; -----------------
;; TIPO Mão:
;; -----------------

(define-struct mão (carta-1 carta-2 carta-3 carta-4))

;; Um elemento do conjunto Mão é
;;  (make-mão carta-1 carta-2 carta-3 carta-4) onde
;;  carta-1 : CartaMão, representa a primeira carta da mão
;;  carta-2 : CartaMão, representa a segunda carta da mão
;;  carta-3 : CartaMão, representa a terceira carta da mão
;;  carta-4 : CartaMão, representa a quarta carta da mão
  
;; Constantes do tipo Mão

(define MAO_1(make-mão
     (make-carta "verde" 0)
     (make-carta "amarelo" INVERTE)
     -5
     -4
  )
)

(define MAO_2(make-mão
     (make-carta "verde" 0)
     (make-carta "amarelo" PULA_VEZ)
     (make-carta "vermelho" 2)
     (make-carta "amarelo" 7)
  )
)

(define MAO_3 (make-mão
     (make-carta "verde" 0)
     (make-carta "amarelo" PULA_VEZ)
     LIVRE
     LIVRE
  )
)

(define MAO_4 (make-mão
     (make-carta "vermelho" 9)
     (make-carta "amarelo" PULA_VEZ)
     LIVRE
     (make-carta "verde" INVERTE)
  )
)

;; ==============================================================
;; 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
;; ==============================================================

;; desenha-mão: Mão -> Image

;; Obj: Dada uma mão, gera uma imagem com as cartas da mão, lado a lado

;; Exemplos:
;;   desenha-mão(
;;     (make-mão
;;         (make-carta "vermelho" 9)
;;         (make-carta "amarelo" PULA_VEZ)
;;         LIVRE
;;         (make-carta "verde" INVERTE)
;;     )
;;   )

;;   desenha-mão(
;;     (make-mão
;;         (make-carta "vermelho" 9)
;;         -4
;;         LIVRE
;;         (make-carta "verde" PULA_VEZ)
;;     )
;;   )

(define (desenha-mão mão) ;; Dada uma mão 
  (beside
    ;; a imagem da carta da posição 1 da mao
    (cond
      [(string? (mão-carta-1 mão)) (text "\tLivre\t" 20 "black")]
      [else
        (desenha-carta
            (cond
              [(number? (mão-carta-1 mão)) (mão-carta-1 mão)]
              [else (carta-valor (mão-carta-1 mão))]
            )
            (cond
              [(number? (mão-carta-1 mão)) "preto"]
              [else (carta-cor (mão-carta-1 mão))]
            )
        )
      ]
    )
    ;; a imagem da carta da posição 2 da mao
    (cond
      [(string? (mão-carta-2 mão)) (text "\tLivre\t" 20 "black")]
      [else
        (desenha-carta
            (cond
              [(number? (mão-carta-2 mão)) (mão-carta-2 mão)]
              [else (carta-valor (mão-carta-2 mão))]
            )
            (cond
              [(number? (mão-carta-2 mão)) "preto"]
              [else (carta-cor (mão-carta-2 mão))]
            )
        )
      ]
    )
    ;; a imagem da carta da posição 3 da mao
    (cond
      [(string? (mão-carta-3 mão)) (text "\tLivre\t" 20 "black")]
      [else
        (desenha-carta
            (cond
              [(number? (mão-carta-3 mão)) (mão-carta-3 mão)]
              [else (carta-valor (mão-carta-3 mão))]
            )
            (cond
              [(number? (mão-carta-3 mão)) "preto"]
              [else (carta-cor (mão-carta-3 mão))]
            )
        )
      ]
    )
    ;; a imagem da carta da posição 4 da mao
    (cond
      [(string? (mão-carta-4 mão)) (text "\tLivre\t" 20 "black")]
      [else
        (desenha-carta
            (cond
              [(number? (mão-carta-4 mão)) (mão-carta-4 mão)]
              [else (carta-valor (mão-carta-4 mão))]
            )
            (cond
              [(number? (mão-carta-4 mão)) "preto"]
              [else (carta-cor (mão-carta-4 mão))]
            )
        )
      ]
    )
  ) ;; coloca lado a lado
)

(desenha-mão MAO_1)
(desenha-mão MAO_2)
(desenha-mão MAO_3)
(desenha-mão MAO_4)
