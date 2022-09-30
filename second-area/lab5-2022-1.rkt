;; Nome: Lucas Rohr Carreño

;; Funções úteis do pacote de imagens (para maiores informações e exemplos, consultar o manual):

;; rotate: Número Imagem -> Imagem
;; Dado um ângulo (em graus) e uma imagem, gera uma nova imagem rotacionando a imagem original
;; no ângulo dado.

;; line: Número Número StringOuPen -> Imagem
;; Dados as as coordenadas x e y de um ponto e uma cor, desenha uma reta desta cor
;; ligando este ponto ao ponto (0,0). O terceiro argumento pode ser também uma caneta (estrutura pen), neste caso se
;; pode fazer linhas diferentes (ver decrição de pen a seguir).

;; Um elemento da estrutura pen é composto por 5 campos: cor (String), largura da linha da caneta (Numero),
;; estilo (String, pode ser "solid", "dash", "dot", ...), o estilo do início/fim da linha ("round", "butt" ou "projecting") e
;; o estilo para unir linhas ("round", "bevel", "miter").

;; image-width: Imagem -> Número
;; Dada uma imagem, devolve a sua largura (em número de pixels)

;; overlay/align/offset: String String Imagem Número Imagem -> Imagem
;; Dados os tipos de alinhameno das imagens na vertical ("right", "left" ou "center") e na horizontal ("bottom", "top" ou "center"),
;; a primeira imagem, o valor do descolamento e a segunda imagem, sobrepõe as imagens considerando os alinhamnentos e descolcamento dados.

;; ========================================================================================
;;                              DEFINIÇÕES DE DADOS
;; ========================================================================================

;; Definição de constantes:

(define LARGURA 400) ;; largura da cena
(define ALTURA 400)  ;; altura da cena
(define ANGULO 45)
(define CENA-VAZIA (empty-scene 400 400))

;; Definição de tipos de dados:
;; ------------ 
;; TIPO FIGURA:
;; ------------
(define-struct figura (coord-x coord-y altura cor))
;; Um elemento do conjunto Figura é
;;     (make-figura x y a c), onde
;;   x: Número, é a coordenada x do centro da figura
;;   y: Número, é a coordenada y do centro da figura
;;   a : Número, é a altura da figura
;;   c : Número, número que representa a cor da figura, de acordo com a função gera-cor 

;; ========================================================================================
;;                                  FUNÇÕES DO LAB 5 
;; ========================================================================================
;; ==> Insira aqui as definições de funções do Lab. 5 que você irá utilizar (não inclua exemplos/testes)


;; sierpinski: Numero String -> Imagem
;; Obj: Dados o tamanho do lado e uma cor, desenha um triângulo de Sierpinski
;; desta cor cujo lado do triângulo externo é o lado passado como argumento. 
(define (sierpinski lado cor)
       (cond
         ;; se o lado for muito pequeno, desenhar um triângulo com o lado e cor dados, sem preenchimento
         [(< lado 5) (triangle lado "outline" cor)]
         ;; senão
         [else
          ;; desenha um triângulo de sierpinksi com a metade do tamanho do lado
          ;; e dá o nome de TRIANGULO para este desenho (definição local):
          (local (
                  (define TRIANGULO (sierpinski (/ lado 2) cor))
                  )
            ;; e monta a imagem do triângulo de sierpinski colocando um TRIANGULO
            ;; acima de dois outros TRIANGULOs:
            (above
             TRIANGULO
             (beside TRIANGULO TRIANGULO)))]))

;; tapete-sierpinski: Numero String -> Imagem
;; Obj: Dados o tamanho do lado e uma cor, desenha um tapete de Sierpinski
;; desta cor cujo lado do quadrado externo é o lado passado como argumento.
(define (tapete-sierpinski lado cor)
  (cond
    ;; se o lado for muito pequeno, desenhar um triângulo com o lado e cor dados, sem preenchimento
    [(< lado 5) (rectangle lado lado "solid" cor)]
    ;; senão
    [else
     ;; desenha um tapete de sierpinksi com a metade do tamanho do lado
     ;; e dá o nome de TAPETE para este desenho (definição local):
     (local (
             (define TAPETE (tapete-sierpinski (/ lado 3) cor))
             (define TAPETE_BRANCO (tapete-sierpinski (/ lado 3) "white"))
             )
       ;; e monta a imagem do tapete de sierpinski:
       (above
        (beside TAPETE TAPETE TAPETE)
        (beside TAPETE TAPETE_BRANCO TAPETE)
        (beside TAPETE TAPETE TAPETE)))]))

;; seleciona-cor : Numero -> String
;; Dado um número natural, devolve uma de 5 cores: "red", "blue", "green", "yellow" ou "cyan".
;; As cores devem ser devolvidas nesta ordem
(define (seleciona-cor numero)
  (cond
    [(= numero 0) "red"]
    [(= numero 1) "blue"]
    [(= numero 2) "green"]
    [(= numero 3) "yellow"]
    [(= numero 4) "cyan"]
    [else "red"]))

;; desenha-triangulos: ListaDeNúmeros -> Cena
;; Dada uma lista de números, pega os números 3 a 3, interpretando o
;; primeiro como o tamanho do lado do triângulo e o segundo e o terceiro como as coordenadas x e y,
;; respectivamente, de um ponto onde o triangulo deve ser poscionado em uma cena, monta a cena
;; com um triângulo de Sierpinski com o tamanho correspondente em cada uma das posições da lista.
;; Se sobrarem números na lista, eles devem ser ignorados. Os triangulos devel ser desenhados em
;; vermelho em uma cena de 400 por 200.
(define (tamanho-lista lista)
  (cond
    [(empty? lista) 0]
    [(cons? lista) (+ 1 (length (rest lista)))]))

(define (desenha-triangulos lista-numeros)
  (cond
    [(< (tamanho-lista lista-numeros) 3) (empty-scene 400 200)]
    [else
     (local (
       (define PRIMEIRO_NUM (first lista-numeros))
       (define SEGUNDO_NUM (first (rest lista-numeros)))
       (define TERCEIRO_NUM (first (rest (rest lista-numeros))))
       )
     (place-image (sierpinski PRIMEIRO_NUM "red") SEGUNDO_NUM TERCEIRO_NUM (desenha-triangulos (rest (rest (rest lista-numeros))))))]))

;; desenha-triangulos-coloridos: ListaDeNúmeros -> Cena
;; Dada uma lista de números, pega os números 3 a 3, interpretando o
;; primeiro como o tamanho do lado do triângulo e o segundo e o terceiro como as coordenadas x e y,
;; respectivamente, de um ponto onde o triangulo deve ser poscionado em uma cena, monta a cena
;; com um triângulo de Sierpinski com o tamanho correspondente em cada uma das posições da lista.
;; Se sobrarem números na lista, eles devem ser ignorados. Os triângulos serão desenhados em vermelho, azul, verde,
;; amarelo e anil, nesta ordem, em uma cena de 400 por 200.
(define (desenha-tri-aux lista-numeros n)
  (cond
    [(< (tamanho-lista lista-numeros) 3) (empty-scene 400 200)]
    [else
     (local (
       (define PRIMEIRO_NUM (first lista-numeros))
       (define SEGUNDO_NUM (first (rest lista-numeros)))
       (define TERCEIRO_NUM (first (rest (rest lista-numeros))))
       )
     (place-image (sierpinski PRIMEIRO_NUM (seleciona-cor n)) SEGUNDO_NUM TERCEIRO_NUM (desenha-tri-aux (rest (rest (rest lista-numeros))) (+ n 1))))]))

(define (desenha-triangulos-coloridos lista-numero)
  (desenha-tri-aux lista-numero 0))


;; desenha-figuras-coloridas: ListaDeNumeros Funcao -> Imagem
;; Dada uma lista de númerose uma funçao que gera uma imagem a partir de um tamanho de lado e uma cor,
;; pega os números 3 a 3, interpretando o primeiro como o tamanho do lado da figura e o segundo e o terceiro como as coordenadas x e y,
;; respectivamente, de um ponto onde a imagem gerada pela função deve ser poscionada em uma cena, monta a cena
;; com as iagens com o tamanho correspondente em cada uma das posições da lista.
;; Se sobrarem números na lista, eles devem ser ignorados. As imagens serão desenhadas em vermelho, azul, verde,
(define (desenha-figura-aux lista-numeros gera-imagem n)
  (cond
    [(< (tamanho-lista lista-numeros) 3) (empty-scene 400 200)]
    [else
     (local (
       (define PRIMEIRO_NUM (first lista-numeros))
       (define SEGUNDO_NUM (first (rest lista-numeros)))
       (define TERCEIRO_NUM (first (rest (rest lista-numeros))))
       )
     (place-image (gera-imagem PRIMEIRO_NUM (seleciona-cor n)) SEGUNDO_NUM TERCEIRO_NUM (desenha-figura-aux (rest (rest (rest lista-numeros))) gera-imagem (+ n 1))))]))

(define (desenha-figuras-coloridas lista-numero gera-imagem)
  (desenha-figura-aux lista-numero gera-imagem 0))


;; ========================================================================================
;;                             SOLUÇÕES DA LISTA 4 
;; ========================================================================================

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================

;; Definição de constantes:
(define TAM_MIN_CAULE 5)

;; árvore : Número String -> Imagem
;; Objetivo : dado um tamanho de caule e uma cor, desenha uma árvore com N galhos contendo folhas nas pontas de cada um
;; a posição dos galhos é calculada na árvore e sua rotação também

;; Exemplos:
;; (árvore 50 "pink") = desenha árvore com folhas rosas e caule base com 50 de altura
;; (árvore 100 "red") = desenha árvore com folhas vermelhas e caule base com 100 de altura
(define (árvore tam-caule cor)
  (cond
    [(< tam-caule TAM_MIN_CAULE) (circle 3 "solid" cor)]
    [else
     (local ((define GALHO (árvore (/ tam-caule 3) cor)))
       (beside
        GALHO
        (rotate 45
         (line 0 (/ tam-caule 3) (make-pen "brown" 3 "solid" "round" "round")))
        (line 0 tam-caule (make-pen "brown" 3 "solid" "round" "round"))
       (rotate 135
         (line 0 (* tam-caule (/ 2 3)) (make-pen "brown" 3 "solid" "round" "round")))
       GALHO))]))

;; Chamadas da função árvore (pele menos 3, colocadas lado a lado):
(beside (árvore 50 "pink")
        (árvore 100 "red")
        (árvore 100 "green"))

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================

;; Definição de constante:
(define FIGURA1 (make-figura 0 200 50 1))
(define FIGURA2 (make-figura 100 100 100 3))
(define FIGURA3 (make-figura 100 300 25 2))

;; desenha-sierpinski : Figura -> Imagem
;; Objetivo : dada uma figura, usa de seus atributos para montar um triângulo de sierpinski equilátero

;; Exemplos:
;; (desenha-sierpinski FIGURA1) = desenha triângulo equilátero de sierpinski azul com lado segundo altura da figura
;; (desenha-sierpinski FIGURA2) = desenha triângulo equilátero de sierpinski amarelo com lado segundo altura da figura
(define (desenha-sierpinski figura)
  (sierpinski (/ (* 2 (figura-altura figura)) (sqrt 3)) (seleciona-cor (figura-cor figura))))

;; Chamadas da função desenha-sierpinski (pele menos 3, colocadas lado a lado):
(beside (desenha-sierpinski FIGURA1)
        (desenha-sierpinski FIGURA2)
        (desenha-sierpinski FIGURA3))

;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
;; ==============================================================

;; desenha-árvore : Figura -> Imagem
;; Objetivo : dada uma figura, usa de seus atributos para montar uma árvore com caule conforme altura da figura e folhas

;; Exemplos:
;; (desenha-árvore FIGURA1) = desenha árvore com folhas azuis com altura segundo a figura
;; (desenha-árvore FIGURA2) = desenha árvore com folhas amarelas com altura segundo a figura
(define (desenha-árvore figura)
  (árvore (figura-altura figura) (seleciona-cor (figura-cor figura))))

;; Chamadas da função desenha-árvore (pele menos 3, colocadas lado a lado):
(beside (desenha-árvore FIGURA1)
        (desenha-árvore FIGURA2)
        (desenha-árvore FIGURA3))

;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;; ==============================================================

;; desenha-figuras : (Número String -> Imagem) Figura -> Cena
;; Objetivo : dada uma função que desenha figura e uma figura, usa dos atributos da figura
;; e vai modificando eles usando da função recebida para ir desenhando figura em uma cena

;; Exemplos:
;; (desenha-figuras sierpinski FIGURA2)
;; (desenha-figuras árvore FIGURA1)
;; (desenha-figuras tapete-sierpinski FIGURA3)
(define (desenha-figuras desenha-figura figura)
  (cond
    [(< (figura-altura figura) 10) (empty-scene 400 400)]
    [else
     (place-image (desenha-figura (figura-altura figura) (seleciona-cor (figura-cor figura)))
                  (figura-coord-x figura) (figura-coord-y figura)
                  (desenha-figuras desenha-figura (make-figura
                                        (+ (figura-coord-x figura) 20)
                                        (- (figura-coord-y figura) 10)
                                        (/ (figura-altura figura) 2)
                                        (+ (figura-cor figura) 1))))]))

;; Terminação: Quando a altura da figura for menor que 10, retornar a cena vazia desenhando todas as figuras

;; Chamadas da função desenha-figuras (pele menos 3, colocadas lado a lado):
(beside (desenha-figuras sierpinski FIGURA2)
        (desenha-figuras árvore FIGURA1)
        (desenha-figuras tapete-sierpinski FIGURA3))

;; ==============================================================
;; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 
;; ==============================================================


;; (A)
;; ========================
;; Critérios de terminação:
;; ========================
;; ==> coloque aqui as funções que definem cirtérios de terminação diferentes (pelo menos 3)

;; criterio-terminacao-altura : Figura -> Booleano
;; Objetivo : dada figura, retorna se a altura da mesma é menor que 10

;; Exemplos/Testes:
   (check-expect (criterio-terminacao-altura FIGURA1) #false)
   (check-expect (criterio-terminacao-altura FIGURA2) #false)

(define (criterio-terminacao-altura figura)
  (< (figura-altura figura) 10))

;; criterio-terminacao-coord-x : Figura -> Booleano
;; Objetivo : dada figura, retorna se a coord-x da mesma é menor ou igual que 0

;; Exemplos/Testes:
   (check-expect (criterio-terminacao-coord-x FIGURA1) #true)
   (check-expect (criterio-terminacao-coord-x FIGURA2) #false)

(define (criterio-terminacao-coord-x figura)
  (<= (figura-coord-x figura) 0))

;; criterio-terminacao-coord-y-altura : Figura -> Booleano
;; Objetivo : dada figura, retorna se a coord-y da mesma é menor ou igual que a altura e se a altura é menor que 50

;; Exemplos/Testes:
   (check-expect (criterio-terminacao-coord-y-altura FIGURA1) #false)
   (check-expect (criterio-terminacao-coord-y-altura FIGURA3) #false)

(define (criterio-terminacao-coord-y-altura figura)
  (and (< (figura-coord-y figura) (figura-altura figura)) (< (figura-altura figura) 50)))

;; (B)
;; ==================================
;; Funções para modificação da figura:
;; ==================================
;; ==> coloque aqui as funções que modificam as figuras (pelo menos 3)



;; (C)
;; ==================================
;; Funções para desenhar uma figura:
;; ==================================
;; ==> coloque aqui funções que desenham uma figura (pelo menos 2)
  
;; (D)

;; Chamadas da função desenha-figuras-gen (pele menos 4, colocadas lado a lado):

;; (E)
;; Para cada chamada listada anteriormente, argumentar sobre a terminação.

