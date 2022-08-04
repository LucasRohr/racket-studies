;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lista2-LucasRohrCarreno-C) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Nome: Lucas Rohr Carreño

;; ========================================================================
;;                                 QUESTÃO 1
;; Defina o tipo de dado Estado para 8-Puzzle e o tipo de dado ListasDeEstados. Defina três constantes 
;; do tipo Estado, e duas constantes do tipo ListasDeEstados. Defina o tipo ListaDeNúmeros, e quatro constantes do tipo ListaDeNúmeros com os nomes
;; LISTA-MOVE-DIR, LISTA-MOVE-ESQ, LISTA-MOVE-CIMA, LISTA-MOVE-BAIXO com as posições do tile blank que permitem movimentos em cada sentido.
;; =========================================================================
;; -----------------
;; TIPO ESTADO
;; -----------------
(define-struct estado (zero um dois tres quatro cinco seis sete oito))  
;; Um elemento do conjunto Estado é
;;   (make-estado zero um dois tres quatro cinco seis sete oito), onde:
;;   zero   : Número, tile na pocisão zero.
;;   um     : Número, tile na pocisão um.
;;   dois   : Número, tile na pocisão dois.
;;   tres   : Número, tile na pocisão tres.
;;   quatro : Número, tile na pocisão quatro.
;;   cinco  : Número, tile na pocisão cinco.
;;   seis   : Número, tile na pocisão seis.
;;   sete   : Número, tile na pocisão sete.
;;   oito   : Número, tile na pocisão oito.


;; Definição de constantes do tipo Estado:

(define ESTADO-1 (make-estado 0 1 2 3 4 5 6 7 8))
(define ESTADO-2 (make-estado 1 4 2 3 0 5 6 7 8))
(define ESTADO-3 (make-estado 4 5 8 1 1 2 0 7 1))

;; --------------------
;; TIPO LISTA DE ESTADOS
;; --------------------

;; Uma ListasDeEstados pode ser
;; 1. vazia (empty) ou
;; 2. (cons e l), onde
;;    e : Estado
;;    l : ListasDeEstados

;; Definição de constantes do tipo ListasDeEstados:

(define LISTA-ESTADOS-1 (cons ESTADO-1 (cons ESTADO-2 empty)))
(define LISTA-ESTADOS-2 (cons ESTADO-3 (cons ESTADO-2 empty)))

;; --------------------
;; TIPO LISTA DE NÚMEROS
;; --------------------

;; Uma ListaDeNúmeros pode ser
;; 1. vazia (empty) ou
;; 2. (cons n l), onde
;;    n : Número
;;    l : ListaDeNúmeros

;; Definição de constantes do tipo ListaDeNúmeros:

(define LISTA-MOVE-DIR (cons 0 (cons 1 (cons 3 (cons 4 (cons 6 (cons 7 empty)))))))
(define LISTA-MOVE-ESQ (cons 1 (cons 2 (cons 4 (cons 5 (cons 7 (cons 8 empty)))))))
(define LISTA-MOVE-CIMA (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))
(define LISTA-MOVE-BAIXO (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty)))))))

;; ========================================================================
;;                                 QUESTÃO 2
;; Desenvolva a função desenha-lista-estados que, dado uma ListasDeEstados, devolve as imagens dos
;; estados desta lista lado a lado.
;; ========================================================================

;; desenha-tile : Número -> Imagem
;; Objetivo : dado um número, desenha o mesmo em um quadrado, auxiliando desenha-estado

;; Exemplos:
;; (desenha-tile 2) = desenha quadrado com 2 dentro
;; (desenha-tile 0) = desenha quadrado com 0 dentro
(define (desenha-tile numero)
  (overlay
   (rectangle 20 20 "outline" "black")
   (text (number->string numero) 14 "black")))

;; desenha-estado : Estado -> Imagem
;; Objetivo : dado um Estado, desenha seus valores em um grid usando
;; a função desenha-tile, auxiliando desenha-lista-estados

;; Exemplos:
;; (desenha-estado ESTADO-1) = desenha grid de quadrados com ESTADO-1
;; (desenha-estado ESTADO-2) = desenha grid de quadrados com ESTADO-2
(define (desenha-estado estado)
  (cond
    [(empty? estado) empty-image]
    [else
     (above
      (beside
       (desenha-tile (estado-zero estado))
       (desenha-tile (estado-um estado))
       (desenha-tile (estado-dois estado)))
      (beside
       (desenha-tile (estado-tres estado))
       (desenha-tile (estado-quatro estado))
       (desenha-tile (estado-cinco estado)))
      (beside
       (desenha-tile (estado-seis estado))
       (desenha-tile (estado-sete estado))
       (desenha-tile (estado-oito estado))))]))


;; desenha-lista-estados : ListasDeEstados -> Imagem
;; Objetivo : dada uma ListasDeEstados, retorna um grid de estado um ao lado do outro

;; Exemplos:
;; (desenha-lista-estados LISTA-ESTADOS-1) = desenha grids de estados da LISTA-ESTADOS-1 um ao lado do outro
;; (desenha-lista-estados LISTA-ESTADOS-2) = desenha grids de estados da LISTA-ESTADOS-2 um ao lado do outro
(define (desenha-lista-estados lista-estados)
  (cond
    ;; Se a lista lista-estados estiver vazia
    ;; então devolver uma imagem vazia
    [(empty? lista-estados) empty-image]
    ;; Senão, retornar o desenho do primeiro estado ao lado do desenho do restante dos estados
    ;; separados por um espaço em branco
    [else (beside
           (desenha-estado (first lista-estados))
           (rectangle 15 0 "outline" "white")
           (desenha-lista-estados (rest lista-estados)))]))

;; ========================================================================
;;                                 QUESTÃO 3
;; Desenvolva a função pos-blank que, dado um dado um estado, devolve o número da posição do tile blank
;; (valor zero) no estado.
;; ========================================================================

;; Definição de constantes:

(define POSICAO-ZERO 0)
(define POSICAO-UM 1)
(define POSICAO-DOIS 2)
(define POSICAO-TRES 3)
(define POSICAO-QUATRO 4)
(define POSICAO-CINCO 5)
(define POSICAO-SEIS 6)
(define POSICAO-SETE 7)
(define POSICAO-OITO 8)

;; pos-blank : Estado -> Número
;; Objetivo : dado um Estado, retorna em qual posição do estado está o valor zero (blank)

;; Exemplos:
;; (pos-blank ESTADO-1) = 0
;; (pos-blank ESTADO-2) = 4
(define (pos-blank estado)
  (cond
    [(= (estado-zero estado) 0) POSICAO-ZERO]
    [(= (estado-um estado) 0) POSICAO-UM]
    [(= (estado-dois estado) 0) POSICAO-DOIS]
    [(= (estado-tres estado) 0) POSICAO-TRES]
    [(= (estado-quatro estado) 0) POSICAO-QUATRO]
    [(= (estado-cinco estado) 0) POSICAO-CINCO]
    [(= (estado-seis estado) 0) POSICAO-SEIS]
    [(= (estado-sete estado) 0) POSICAO-SETE]
    [(= (estado-oito estado) 0) POSICAO-OITO]))

;; Testes:

(check-expect (pos-blank ESTADO-1) 0)
(check-expect (pos-blank ESTADO-2) 4)

;; ========================================================================
;;                                 QUESTÃO 4
;; Desenvolva a função está-em? que, dado um número e uma lista de números, diz se este número está na lista.
;; Desenvolva a função estado-está-em? que, dados um estado e uma lista de estados, diz se este estado está na lista.
;; ========================================================================

;; está-em? : Número ListaDeNúmeros -> Booleano
;; Objetivo : dado um número e uma ListaDeNúmeros, retorna se o número está na lista ou não

;; Exemplos:
;; (está-em? 4 LISTA-MOVE-DIR) = #true
;; (está-em? 0 LISTA-MOVE-ESQ) = #false
(define (está-em? numero lista-numeros)
  (cond
    ;; Se a lista lista-numeros estiver vazia,
    ;; então devolver falso
    [(empty? lista-numeros) #false]
    ;; Se o primeiro valor da lista for igual ao número informado,
    ;; então retornar true
    [(= numero (first lista-numeros)) #true]
    ;; Senão, seguir retornando o restante da lista
    [else (está-em? numero (rest lista-numeros))]))

;; Testes:

(check-expect (está-em? 4 LISTA-MOVE-DIR) #true)
(check-expect (está-em? 0 LISTA-MOVE-ESQ) #false)

;; estado-é-igual? : Estado Estado -> Booleano
;; Objetivo : dados dois estados, verifica se são iguais, auxiliando estado-está-em?

;; Exemplos:
;; (estado-é-igual? ESTADO-1 ESTADO-1) = #true
;; (estado-é-igual? ESTADO-1 ESTADO-2) = #false
(define (estado-é-igual? estado-1 estado-2)
  (and
   (= (estado-zero estado-1) (estado-zero estado-2))
   (= (estado-um estado-1) (estado-um estado-2))
   (= (estado-dois estado-1) (estado-dois estado-2))
   (= (estado-tres estado-1) (estado-tres estado-2))
   (= (estado-quatro estado-1) (estado-quatro estado-2))
   (= (estado-cinco estado-1) (estado-cinco estado-2))
   (= (estado-seis estado-1) (estado-seis estado-2))
   (= (estado-sete estado-1) (estado-sete estado-2))
   (= (estado-oito estado-1) (estado-oito estado-2))))

;; Testes:

(check-expect (estado-é-igual? ESTADO-1 ESTADO-1) #true)
(check-expect (estado-é-igual? ESTADO-1 ESTADO-2) #false)

;; estado-está-em? : Estado ListasDeEstados -> Booleano
;; Objetivo : dado um estado e uma ListasDeEstados, retorna se o estado está na lista
;; usando estado-é-igual? para auxiliar

;; Exemplos:
;; (estado-está-em? ESTADO-1 LISTA-ESTADOS-1) = #true
;; (estado-está-em? ESTADO-1 LISTA-ESTADOS-2) = #false
(define (estado-está-em? estado lista-estados)
  (cond
    ;; Se a lista lista-estados estiver vazia,
    ;; então devolver falso
    [(empty? lista-estados) #false]
    ;; Se o primeiro estado for igual ao estado informado,
    ;; então retornar true
    [(estado-é-igual? estado (first lista-estados)) #true]
    ;; Senão, seguir retornando o restante da lista
    [else (estado-está-em? estado (rest lista-estados))]))

;; Testes:

(check-expect (estado-está-em? ESTADO-1 LISTA-ESTADOS-1) #true)
(check-expect (estado-está-em? ESTADO-1 LISTA-ESTADOS-2) #false)

;; ========================================================================
;;                                 QUESTÃO 5
;; Desenvolva a função gera-sucessores que, dado um estado, devolve uma ListasDeEstados com todos
;; os estados que podem ser gerados com um movimento do blank a partir do estado recebido.
;; A lista retorna deve conter na ordem (se gerado) o estado gerado por mover o blank para: esquerda, baixo, direita e cima.
;; ========================================================================

;; --------------------
;; TIPO ResultadoTroca:
;; --------------------

;; Um ResultadoTroca: pode ser
;; 1. vazio (empty) ou
;; 2. Estado

;; verifica-troca-tile-esq : Estado Número -> ResultadoTroca
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tile
;; ou o novo estado com tile movida caso possível, contando movimento da tile blank para esquerda

;; Exemplos:
;; (verifica-troca-tile-esq ESTADO-1 0) = empty
;; (verifica-troca-tile-esq ESTADO-2 4) = (make-estado 1 4 2 0 3 5 6 7 8)
(define (verifica-troca-tile-esq estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-ESQ)
     (make-estado
       ;; Checagem de movimento na primeira fileira
       (cond [(= posicao-blank 1) 0] [else (estado-zero estado)])
       (cond [(= posicao-blank 2) 0] [(= posicao-blank 1) (estado-zero estado)] [else (estado-um estado)])
       (cond [(= posicao-blank 2) (estado-um estado)] [else (estado-dois estado)])
       ;; Checagem de movimento na segunda fileira
       (cond [(= posicao-blank 4) 0] [else (estado-tres estado)])
       (cond [(= posicao-blank 5) 0] [(= posicao-blank 4) (estado-tres estado)] [else (estado-quatro estado)])
       (cond [(= posicao-blank 5) (estado-quatro estado)] [else (estado-cinco estado)])
       ;; Checagem de movimento na terceira fileira
       (cond [(= posicao-blank 7) 0] [else (estado-seis estado)])
       (cond [(= posicao-blank 8) 0] [(= posicao-blank 7) (estado-seis estado)] [else (estado-sete estado)])
       (cond [(= posicao-blank 8) (estado-sete estado)] [else (estado-oito estado)])
      )
     ]
    [else empty]
  )
)

;; Testes:

(check-expect (verifica-troca-tile-esq ESTADO-1 0) empty)
(check-expect (verifica-troca-tile-esq ESTADO-2 4) (make-estado 1 4 2 0 3 5 6 7 8))

;; verifica-troca-tile-dir : Estado Número -> ResultadoTroca
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tile
;; ou o novo estado com tile movida caso possível, contando movimento da tile blank para direita

;; Exemplos:
;; (verifica-troca-tile-dir ESTADO-1 0) = (make-estado 1 0 2 3 4 5 6 7 8)
;; (verifica-troca-tile-dir ESTADO-2 4) = (make-estado 1 4 2 3 5 0 6 7 8)
(define (verifica-troca-tile-dir estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-DIR)
     (make-estado
       ;; Checagem de movimento na primeira fileira
       (cond [(= posicao-blank 0) (estado-um estado)] [else (estado-zero estado)])
       (cond [(= posicao-blank 1) (estado-dois estado)] [(= posicao-blank 0) 0] [else (estado-um estado)])
       (cond [(= posicao-blank 1) 0] [else (estado-dois estado)])
       ;; Checagem de movimento na segunda fileira
       (cond [(= posicao-blank 3) (estado-quatro estado)] [else (estado-tres estado)])
       (cond [(= posicao-blank 4) (estado-cinco estado)] [(= posicao-blank 3) 0] [else (estado-quatro estado)])
       (cond [(= posicao-blank 4) 0] [else (estado-cinco estado)])
       ;; Checagem de movimento na terceira fileira
       (cond [(= posicao-blank 6) (estado-sete estado)] [else (estado-seis estado)])
       (cond [(= posicao-blank 7) (estado-oito estado)] [(= posicao-blank 6) 0] [else (estado-sete estado)])
       (cond [(= posicao-blank 7) 0] [else (estado-oito estado)])
      )
     ]
    [else empty]
  )
)

;; Testes:

(check-expect (verifica-troca-tile-dir ESTADO-1 0) (make-estado 1 0 2 3 4 5 6 7 8))
(check-expect (verifica-troca-tile-dir ESTADO-2 4) (make-estado 1 4 2 3 5 0 6 7 8))

;; verifica-troca-tile-cima : Estado Número -> ResultadoTroca
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tile
;; ou o novo estado com tile movida caso possível, contando movimento da tile blank para cima

;; Exemplos:
;; (verifica-troca-tile-cima ESTADO-1 0) = empty
;; (verifica-troca-tile-cima ESTADO-2 4) = (make-estado 1 0 2 3 4 5 6 7 8)
(define (verifica-troca-tile-cima estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-CIMA)
     (make-estado
       ;; Checagem de movimento na primeira fileira
       (cond [(= posicao-blank 3) 0] [else (estado-zero estado)])
       (cond [(= posicao-blank 4) 0] [else (estado-um estado)])
       (cond [(= posicao-blank 5) 0] [else (estado-dois estado)])
       ;; Checagem de movimento na segunda fileira
       (cond [(= posicao-blank 3) (estado-zero estado)] [(= posicao-blank 6) 0] [else (estado-tres estado)])
       (cond [(= posicao-blank 4) (estado-um estado)] [(= posicao-blank 7) 0] [else (estado-quatro estado)])
       (cond [(= posicao-blank 5) (estado-dois estado)] [(= posicao-blank 8) 0] [else (estado-cinco estado)])
       ;; Checagem de movimento na terceira fileira
       (cond [(= posicao-blank 6) (estado-tres estado)] [else (estado-seis estado)])
       (cond [(= posicao-blank 7) (estado-quatro estado)] [else (estado-sete estado)])
       (cond [(= posicao-blank 8) (estado-cinco estado)] [else (estado-oito estado)])
      )
     ]
    [else empty]
  )
)

;; Testes:

(check-expect (verifica-troca-tile-cima ESTADO-1 0) empty)
(check-expect (verifica-troca-tile-cima ESTADO-2 4) (make-estado 1 0 2 3 4 5 6 7 8))

;; verifica-troca-tile-baixo : Estado Número -> ResultadoTroca
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tile
;; ou o novo estado com tile movida caso possível, contando movimento da tile blank para baixo

;; Exemplos:
;; (verifica-troca-tile-baixo ESTADO-1 0) = (make-estado 3 1 2 0 4 5 6 7 8)
;; (verifica-troca-tile-baixo ESTADO-2 4) = (make-estado 1 4 2 3 7 5 6 0 8)
(define (verifica-troca-tile-baixo estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-BAIXO)
     (make-estado
       ;; Checagem de movimento na primeira fileira
       (cond [(= posicao-blank 0) (estado-tres estado)] [else (estado-zero estado)])
       (cond [(= posicao-blank 1) (estado-quatro estado)] [else (estado-um estado)])
       (cond [(= posicao-blank 2) (estado-cinco estado)] [else (estado-dois estado)])
       ;; Checagem de movimento na segunda fileira
       (cond [(= posicao-blank 0) 0] [(= posicao-blank 3) (estado-seis estado)] [else (estado-tres estado)])
       (cond [(= posicao-blank 1) 0] [(= posicao-blank 4) (estado-sete estado)] [else (estado-quatro estado)])
       (cond [(= posicao-blank 2) 0] [(= posicao-blank 5) (estado-oito estado)] [else (estado-cinco estado)])
       ;; Checagem de movimento na terceira fileira
       (cond [(= posicao-blank 3) 0] [else (estado-seis estado)])
       (cond [(= posicao-blank 4) 0] [else (estado-sete estado)])
       (cond [(= posicao-blank 5) 0] [else (estado-oito estado)])
      )
     ]
    [else empty]
  )
)

;; Testes:

(check-expect (verifica-troca-tile-baixo ESTADO-1 0) (make-estado 3 1 2 0 4 5 6 7 8))
(check-expect (verifica-troca-tile-baixo ESTADO-2 4) (make-estado 1 4 2 3 7 5 6 0 8))

;; Definição de constantes de direção

(define ESQUERDA "esq")
(define DIREITA "dir")
(define CIMA "cima")
(define BAIXO "baixo")

;; verifica-sucessor : Estado String -> ResultadoTroca
;; Objetivo : dado um estado e a direção de movimento da tile blank, decide qual auxiliar de troca usar
;; verificando e retornando um resultado de troca conforme direção

;; Exemplos:
;; (verifica-sucessor ESTADO-1 ESQUERDA) = empty
;; (verifica-sucessor ESTADO-2 BAIXO) = (make-estado 1 4 2 3 7 5 6 0 8)
(define (verifica-sucessor estado direcao)
  (cond
    [(string=? direcao ESQUERDA) (verifica-troca-tile-esq estado (pos-blank estado))]
    [(string=? direcao DIREITA) (verifica-troca-tile-dir estado (pos-blank estado))]
    [(string=? direcao CIMA) (verifica-troca-tile-cima estado (pos-blank estado))]
    [(string=? direcao BAIXO) (verifica-troca-tile-baixo estado (pos-blank estado))]
  )
)

;; Testes:

(check-expect (verifica-sucessor ESTADO-1 ESQUERDA) empty)
(check-expect (verifica-sucessor ESTADO-2 BAIXO) (make-estado 1 4 2 3 7 5 6 0 8))

;; gera-lista-tiles : Estado -> ListaDeNúmeros
;; Objetivo : dado um estado, retorna seus atributos numéricos em lista

;; Exemplos:
;; (gera-lista-tiles ESTADO-1) = (list 0 1 2 3 4 5 6 7 8)
;; (gera-lista-tiles ESTADO-2) = (list 1 4 2 3 0 5 6 7 8)
(define (gera-lista-tiles estado)
  (cons (estado-zero estado)
        (cons (estado-um estado)
        (cons (estado-dois estado)
        (cons (estado-tres estado)
        (cons (estado-quatro estado)
        (cons (estado-cinco estado)
        (cons (estado-seis estado)
        (cons (estado-sete estado)
        (cons (estado-oito estado)
        empty))))))))))

;; Testes:

(check-expect (gera-lista-tiles ESTADO-1) (list 0 1 2 3 4 5 6 7 8))
(check-expect (gera-lista-tiles ESTADO-2) (list 1 4 2 3 0 5 6 7 8))

;; retorna-sucessores-nao-vazios : ListasDeEstados -> ListasDeEstados
;; Objetivo : dada uma ListasDeEstados, sendo estes sucessores, retorna a lista
;; dos sucessores existentes, não vazios

;; Exemplos:
;; (retorna-sucessores-nao-vazios (list '() (make-estado 3 1 2 0 4 5 6 7 8) '() (make-estado 1 0 2 3 4 5 6 7 8))) =
;;  (list (make-estado 3 1 2 0 4 5 6 7 8) (make-estado 1 0 2 3 4 5 6 7 8))
;; (retorna-sucessores-nao-vazios
;;  (list
;;    (make-estado 1 4 2 0 3 5 6 7 8)
;;    (make-estado 1 4 2 3 7 5 6 0 8)
;;    (make-estado 1 4 2 3 5 0 6 7 8)
;;    (make-estado 1 0 2 3 4 5 6 7 8))) =
;; (list
;;   (make-estado 1 4 2 0 3 5 6 7 8)
;;   (make-estado 1 4 2 3 7 5 6 0 8)
;;   (make-estado 1 4 2 3 5 0 6 7 8)
;;   (make-estado 1 0 2 3 4 5 6 7 8))
(define (retorna-sucessores-nao-vazios lista-sucessores)
  (cond
    ;; Se a lista lista-sucessores estiver vazia,
    ;; então retornar vazio
    [(empty? lista-sucessores) empty]
    ;; Se o primeiro sucessor da lista não for vazio,
    ;; então retornar ele montado em uma lista junto do resto dos sucessores não vazios
    [(not (empty? (first lista-sucessores))) (cons (first lista-sucessores) (retorna-sucessores-nao-vazios (rest lista-sucessores)))]
    ;; Senão, seguir retornando o resto da lista
    [else (retorna-sucessores-nao-vazios (rest lista-sucessores))]
  )
)

;; Testes:

(check-expect (retorna-sucessores-nao-vazios (list '() (make-estado 3 1 2 0 4 5 6 7 8) '() (make-estado 1 0 2 3 4 5 6 7 8)))
              (list (make-estado 3 1 2 0 4 5 6 7 8) (make-estado 1 0 2 3 4 5 6 7 8)))
(check-expect (retorna-sucessores-nao-vazios
               (list (make-estado 1 4 2 0 3 5 6 7 8)
                     (make-estado 1 4 2 3 7 5 6 0 8)
                     (make-estado 1 4 2 3 5 0 6 7 8)
                     (make-estado 1 0 2 3 4 5 6 7 8)))
              (list (make-estado 1 4 2 0 3 5 6 7 8)
                    (make-estado 1 4 2 3 7 5 6 0 8)
                    (make-estado 1 4 2 3 5 0 6 7 8)
                    (make-estado 1 0 2 3 4 5 6 7 8)))

;; gera-sucessores : Estado -> ListasDeEstados
;; Objetivo : dado um estado, retorna sua lista de sucessores possíveis
;; usando verifica-sucessor com diferentes direções de movimento do tile blank, além de
;; retorna-sucessores-nao-vazios como auxiliar

;; Exemplos:
;; (gera-sucessores ESTADO-1) = (list (make-estado 3 1 2 0 4 5 6 7 8) (make-estado 1 0 2 3 4 5 6 7 8))
;; (gera-sucessores ESTADO-2) =
;; (list
;;   (make-estado 1 4 2 0 3 5 6 7 8)
;;   (make-estado 1 4 2 3 7 5 6 0 8)
;;   (make-estado 1 4 2 3 5 0 6 7 8)
;;   (make-estado 1 0 2 3 4 5 6 7 8))
(define (gera-sucessores estado)
  (cond
    ;; Se o estado não possui um tile blank,
    ;; então retornar vazio
    [(not (está-em? 0 (gera-lista-tiles estado))) empty]
    ;; Senão, verificar os sucessores do estado para cada direção de movimento do tile blank
    ;; e retornar apenas os não vazios (sucessores existentes)
    [else (retorna-sucessores-nao-vazios
           (cons (verifica-sucessor estado ESQUERDA)
                 (cons (verifica-sucessor estado BAIXO)
                       (cons (verifica-sucessor estado DIREITA)
                             (cons (verifica-sucessor estado CIMA) empty)))))]))

;; Testes:

(check-expect (gera-sucessores ESTADO-1) (list (make-estado 3 1 2 0 4 5 6 7 8) (make-estado 1 0 2 3 4 5 6 7 8)))
(check-expect (gera-sucessores ESTADO-2) (list (make-estado 1 4 2 0 3 5 6 7 8)
                                               (make-estado 1 4 2 3 7 5 6 0 8)
                                               (make-estado 1 4 2 3 5 0 6 7 8)
                                               (make-estado 1 0 2 3 4 5 6 7 8)))

;; ========================================================================
;;                                 QUESTÃO 6
;; Desenvolva a função gera-sucessores-lista que, dado uma ListasDeEstados, devolve uma única 
;; ListasDeEstados com todos os estados que podem ser gerados com um movimento do blank a partir
;; de cada um dos estados recebidos. A lista retornada não pode conter estados repetidos.
;; ========================================================================

;; retorna-sucessores-lista-estados : ListasDeEstados -> ListasDeEstados
;; Objetivo : dado uma ListasDeEstados, retorna uma nova ListasDeEstados montada recursivamente
;; contendo os sucessores de cada estado iterado na lista

;; Exemplos:
;; (retorna-sucessores-lista-estados LISTA-ESTADOS-1) =
;; (list
;;  '()
;;  '()
;;  (make-estado 4 5 8 1 1 2 7 0 1)
;;  (make-estado 4 5 8 0 1 2 1 7 1)
;;  (make-estado 1 4 2 0 3 5 6 7 8)
;;  (make-estado 1 4 2 3 7 5 6 0 8)
;;  (make-estado 1 4 2 3 5 0 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8))

;; (retorna-sucessores-lista-estados LISTA-ESTADOS-2) =
;; (list
;;   '()
;;   '()
;;   (make-estado 4 5 8 1 1 2 7 0 1)
;;   (make-estado 4 5 8 1 1 2 1 7 1)
;;   (make-estado 1 4 2 0 3 5 6 7 8)
;;   (make-estado 1 4 2 3 7 5 6 0 8)
;;   (make-estado 1 4 2 3 5 0 6 7 8)
;;   (make-estado 1 0 2 3 4 5 6 7 8))

(define (retorna-sucessores-lista-estados lista-estados)
  (cond
    ;; Se a lista-estados estiver vazia,
    ;; então retornar vazio
    [(empty? lista-estados) empty]
    ;; Se o primeiro estado da lista não for vazio,
    ;; então verificar e retornar seus sucessores e montar em uma
    ;; lista juntamente com os sucessores do restante da lista de estados
    [(not (empty? (first lista-estados)))
     (cons (verifica-sucessor (first lista-estados) ESQUERDA)
           (cons (verifica-sucessor (first lista-estados) BAIXO)
           (cons (verifica-sucessor (first lista-estados) DIREITA)
           (cons (verifica-sucessor (first lista-estados) CIMA)
           (retorna-sucessores-lista-estados (rest lista-estados))))))]
    ;; Senão, retornar a lista de sucessores do resto dos estados
    [else (retorna-sucessores-lista-estados (rest lista-estados))]))

;; Testes:

(check-expect (retorna-sucessores-lista-estados LISTA-ESTADOS-1)
              (list
               '()
               (make-estado 3 1 2 0 4 5 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)
               '()
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))

(check-expect (retorna-sucessores-lista-estados LISTA-ESTADOS-2)
              (list
               '()
               '()
               (make-estado 4 5 8 1 1 2 7 0 1)
               (make-estado 4 5 8 0 1 2 1 7 1)
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))

;; retorna-sucessores-estados-nao-vazios : ListasDeEstados -> ListasDeEstados
;; Objetivo : dado uma ListasDeEstados, retorna uma nova ListasDeEstados montada recursivamente
;; removendo os valores que forem vazios, para complementar o retorno da função retorna-sucessores-lista-estados
;; de forma separada e mais limpa

;; Exemplos:
;; (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados LISTA-ESTADOS-1)) =
;; (list
;;  (make-estado 3 1 2 0 4 5 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8)
;;  (make-estado 1 4 2 0 3 5 6 7 8)
;;  (make-estado 1 4 2 3 7 5 6 0 8)
;;  (make-estado 1 4 2 3 5 0 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8))

;; (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados LISTA-ESTADOS-2)) =
;; (list
;;  (make-estado 4 5 8 1 1 2 7 0 1)
;;  (make-estado 4 5 8 0 1 2 1 7 1)
;;  (make-estado 1 4 2 0 3 5 6 7 8)
;;  (make-estado 1 4 2 3 7 5 6 0 8)
;;  (make-estado 1 4 2 3 5 0 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8))

(define (retorna-sucessores-estados-nao-vazios lista-estados)
  (cond
    ;; Se a lista-estados estiver vazia,
    ;; então retornar vazio
    [(empty? lista-estados) empty]
    ;; Se o primeiro elemento da lista não for vazio,
    ;; então retornar ele montado junto da lista do resto de elementos não vazios
    [(not (empty? (first lista-estados)))
     (cons (first lista-estados) (retorna-sucessores-estados-nao-vazios (rest lista-estados)))]
    ;; Senão, retornar o resto dos elementos não vazios
    [else (retorna-sucessores-estados-nao-vazios (rest lista-estados))]))

;; Testes:

(check-expect (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados LISTA-ESTADOS-1))
              (list
               (make-estado 3 1 2 0 4 5 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))

(check-expect (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados LISTA-ESTADOS-2))
              (list
               (make-estado 4 5 8 1 1 2 7 0 1)
               (make-estado 4 5 8 0 1 2 1 7 1)
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))

;; gera-sucessores-lista-repetidos : ListasDeEstados -> ListasDeEstados
;; Objetivo : dada uma ListasDeEstados, deve devolver uma ListasDeEstados contendo os
;; sucessores possíveis de serem gerados com um movimento do blank de cada estado.
;; A função utiliza as funções retorna-sucessores-lista-estados bem como
;; retorna-sucessores-estados-nao-vazios como auxiliares

;; Exemplos:
;; (gera-sucessores-lista LISTA-ESTADOS-1) =
;; (list
;;  (make-estado 3 1 2 0 4 5 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8)
;;  (make-estado 1 4 2 0 3 5 6 7 8)
;;  (make-estado 1 4 2 3 7 5 6 0 8)
;;  (make-estado 1 4 2 3 5 0 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8))

;; (gera-sucessores-lista LISTA-ESTADOS-2) =
;; (list
;;  (make-estado 4 5 8 1 1 2 7 0 1)
;;  (make-estado 4 5 8 0 1 2 1 7 1)
;;  (make-estado 1 4 2 0 3 5 6 7 8)
;;  (make-estado 1 4 2 3 7 5 6 0 8)
;;  (make-estado 1 4 2 3 5 0 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8))

(define (gera-sucessores-lista-repetidos lista-estados)
  (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados lista-estados)))

;; Testes:

(check-expect (gera-sucessores-lista-repetidos LISTA-ESTADOS-1)
              (list
               (make-estado 3 1 2 0 4 5 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))

(check-expect (gera-sucessores-lista-repetidos LISTA-ESTADOS-2)
              (list
               (make-estado 4 5 8 1 1 2 7 0 1)
               (make-estado 4 5 8 0 1 2 1 7 1)
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))

;; gera-sucessores-lista : ListasDeEstados -> ListasDeEstados
;; Objetivo : dada uma ListasDeEstados, deve devolver uma ListasDeEstados contendo os
;; sucessores possíveis de serem gerados com um movimento do blank de cada estado,
;; sem contar sucessores repetidos. A função utiliza a função gera-sucessores-lista-repetidos
;; como auxiliar na chamada para remover os sucessores repetidos

;; Exemplos:
;; (gera-sucessores-lista (gera-sucessores-lista-repetidos LISTA-ESTADOS-1)) =
;; (list
;;  (make-estado 3 1 2 0 4 5 6 7 8)
;;  (make-estado 1 4 2 0 3 5 6 7 8)
;;  (make-estado 1 4 2 3 7 5 6 0 8)
;;  (make-estado 1 4 2 3 5 0 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8))

;; (gera-sucessores-lista (gera-sucessores-lista-repetidos LISTA-ESTADOS-2)) =
;; (list
;;  (make-estado 4 5 8 1 1 2 7 0 1)
;;  (make-estado 4 5 8 0 1 2 1 7 1)
;;  (make-estado 1 4 2 0 3 5 6 7 8)
;;  (make-estado 1 4 2 3 7 5 6 0 8)
;;  (make-estado 1 4 2 3 5 0 6 7 8)
;;  (make-estado 1 0 2 3 4 5 6 7 8))

(define (gera-sucessores-lista lista-estados)
  (cond
    ;; Se a lista lista-estados estiver vazia, então devolver vazio
    [(empty? lista-estados) empty]
    ;; Se o primeiro elemento da lista não se encontra no restante dela,
    ;; então retornar o mesmo e montar junto do resto da lista
    [(not (estado-está-em? (first lista-estados) (rest lista-estados)))
     (cons (first lista-estados) (gera-sucessores-lista (rest lista-estados)))]
    ;; Senão, retornar o restante da lista
    [else (gera-sucessores-lista (rest lista-estados))]))

;; Testes:

(check-expect (gera-sucessores-lista (gera-sucessores-lista-repetidos LISTA-ESTADOS-1))
              (list
               (make-estado 3 1 2 0 4 5 6 7 8)
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))

(check-expect (gera-sucessores-lista (gera-sucessores-lista-repetidos LISTA-ESTADOS-2))
              (list
               (make-estado 4 5 8 1 1 2 7 0 1)
               (make-estado 4 5 8 0 1 2 1 7 1)
               (make-estado 1 4 2 0 3 5 6 7 8)
               (make-estado 1 4 2 3 7 5 6 0 8)
               (make-estado 1 4 2 3 5 0 6 7 8)
               (make-estado 1 0 2 3 4 5 6 7 8)))


;; ========================================================================
;;                                 QUESTÃO 7
;; (Bônus-Opcional: 1 Ponto)
;; Dado um estado determina quantos estados podem ser alcançados a partir
;; do estado recebido usando um número arbitrário de movimentos.
;; ========================================================================

;; conta-estados-alc : ...
