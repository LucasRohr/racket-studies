;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lista2-LucasRohrCarreno-C) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #t ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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
;;   (make-estado zero um dois tres quatro cinco seis sete oito)
;;   zero   : Número, tile na pocisão zero.
;;   um     : Número, tile na pocisão um.
;;   dois   : Número, tile na pocisão dois.
;;   tres   : Número, tile na pocisão tres.
;;   quatro   : Número, tile na pocisão quatro.
;;   cinco   : Número, tile na pocisão cinco.
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

;; Definição de constantes do tipo ListaDeNúmeros

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
;; (desenha-tile 0)desenha quadrado com 0 dentro
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


;; desenha-lista-estados : ListaDeEstados -> Imagem
;; Objetivo : dada uma ListaDeEstados, retorna um grid de estado um
;; ao lado do outro

;; Exemplos:
;; (desenha-lista-estados LISTA-ESTADOS-1) = desenha grids da LISTA-ESTADOS-1 um ao lado do outro
;; (desenha-lista-estados LISTA-ESTADOS-2) = desenha grids da LISTA-ESTADOS-2 um ao lado do outro
(define (desenha-lista-estados lista-estados)
  (cond
    ;; se a lista lista-estados estiver vazia, então devolver uma imagem vazia
    [(empty? lista-estados) empty-image]
    ;; senão, retorna o desenho do primeiro estado ao lado do desenho do restantes dos estados
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
;; Objetivo : dado um número de uma ListaDeNúmeros, retorna se o número está na lista ou não

;; Exemplos:
;; (está-em? 4 LISTA-MOVE-DIR) = #true
;; (está-em? 0 LISTA-MOVE-ESQ) = #false
(define (está-em? numero lista-numeros)
  (cond
    ;; se a lista lista-numeros estiver vazia, então devolver falso
    [(empty? lista-numeros) #false]
    ;; se o primeiro valor da lista for igual ao número informado, retornar true
    [(= numero (first lista-numeros)) #true]
    ;; senão, retornar a execução da função com o restante da lista
    [else (está-em? numero (rest lista-numeros))]))

;; Testes:

(check-expect (está-em? 4 LISTA-MOVE-DIR) #true)
(check-expect (está-em? 0 LISTA-MOVE-ESQ) #false)

;; estado-é-igual? : Estado ListaEstadoDeNúmeros -> Booleano
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

;; estado-está-em? : Estado ListaDeEstados -> Booleano
;; Objetivo : dado um estado e uma ListaDeEstados, retorna se o estado está na lista
;; usando estado-está-em? para auxiliar

;; Exemplos:
;; (estado-está-em? ESTADO-1 LISTA-ESTADOS-1) = #true
;; (estado-está-em? ESTADO-1 LISTA-ESTADOS-2) = #false
(define (estado-está-em? estado lista-estados)
  (cond
    ;; se a lista lista-estados estiver vazia, então devolver falso
    [(empty? lista-estados) #false]
    ;; se o primeiro estado for igual ao estado informado, retornar true
    [(estado-é-igual? estado (first lista-estados)) #true]
    ;; senão, retornar a execução da função passando o resto da lista
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
;; 1. vazia (empty) ou
;; 2. Estado

;; verifica-troca-tile-esq : Estado Número -> ResultadoTroca
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tila
;; ou o novo estado com tile movida caso possível, com posições para esquerda

;; Exemplos:
;; (verifica-troca-tile-esq ESTADO-1 0) = empty
;; (verifica-troca-tile-esq ESTADO-2 4) = (make-estado 1 4 2 0 3 5 6 7 8)
(define (verifica-troca-tile-esq estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-ESQ)
     (make-estado
       (cond [(= posicao-blank 1) 0] [else (estado-zero estado)])
       (cond [(= posicao-blank 2) 0] [(= posicao-blank 1) (estado-zero estado)] [else (estado-um estado)])
       (cond [(= posicao-blank 2) (estado-um estado)] [else (estado-dois estado)])
       
       (cond [(= posicao-blank 4) 0] [else (estado-tres estado)])
       (cond [(= posicao-blank 5) 0] [(= posicao-blank 4) (estado-tres estado)] [else (estado-quatro estado)])
       (cond [(= posicao-blank 5) (estado-quatro estado)] [else (estado-cinco estado)])
       
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
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tila
;; ou o novo estado com tile movida caso possível, com posições para direita

;; Exemplos:
;; (verifica-troca-tile-dir ESTADO-1 0) = (make-estado 1 0 2 3 4 5 6 7 8)
;; (verifica-troca-tile-dir ESTADO-2 4) = (make-estado 1 4 2 3 5 0 6 7 8)
(define (verifica-troca-tile-dir estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-DIR)
     (make-estado
       (cond [(= posicao-blank 0) (estado-um estado)] [else (estado-zero estado)])
       (cond [(= posicao-blank 1) (estado-dois estado)] [(= posicao-blank 0) 0] [else (estado-um estado)])
       (cond [(= posicao-blank 1) 0] [else (estado-dois estado)])

       (cond [(= posicao-blank 3) (estado-quatro estado)] [else (estado-tres estado)])
       (cond [(= posicao-blank 4) (estado-cinco estado)] [(= posicao-blank 3) 0] [else (estado-quatro estado)])
       (cond [(= posicao-blank 4) 0] [else (estado-cinco estado)])

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
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tila
;; ou o novo estado com tile movida caso possível, com posições para cima

;; Exemplos:
;; (verifica-troca-tile-cima ESTADO-1 0) = empty
;; (verifica-troca-tile-cima ESTADO-2 4) = (make-estado 1 0 2 3 4 5 6 7 8)
(define (verifica-troca-tile-cima estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-CIMA)
     (make-estado
       (cond [(= posicao-blank 3) 0] [else (estado-zero estado)])
       (cond [(= posicao-blank 4) 0] [else (estado-um estado)])
       (cond [(= posicao-blank 5) 0] [else (estado-dois estado)])

       (cond [(= posicao-blank 3) (estado-zero estado)] [(= posicao-blank (estado-seis estado)) 0] [else (estado-tres estado)])
       (cond [(= posicao-blank 4) (estado-um estado)] [(= posicao-blank (estado-sete estado)) 0] [else (estado-quatro estado)])
       (cond [(= posicao-blank 5) (estado-dois estado)] [(= posicao-blank (estado-oito estado)) 0] [else (estado-cinco estado)])

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
;; Objetivo : dado um estado e a posição do seu tile blank, retorna vazio se não for possível trocar a tila
;; ou o novo estado com tile movida caso possível, com posições para baixo

;; Exemplos:
;; (verifica-troca-tile-baixo ESTADO-1 0) = (make-estado 3 1 2 0 4 5 6 7 8)
;; (verifica-troca-tile-baixo ESTADO-2 4) = (make-estado 1 4 2 3 7 5 6 0 8)
(define (verifica-troca-tile-baixo estado posicao-blank)
  (cond
    [(está-em? posicao-blank LISTA-MOVE-BAIXO)
     (make-estado
       (cond [(= posicao-blank 0) (estado-tres estado)] [else (estado-zero estado)])
       (cond [(= posicao-blank 1) (estado-quatro estado)] [else (estado-um estado)])
       (cond [(= posicao-blank 2) (estado-cinco estado)] [else (estado-dois estado)])

       (cond [(= posicao-blank 0) 0] [(= posicao-blank 3) (estado-seis estado)] [else (estado-tres estado)])
       (cond [(= posicao-blank 1) 0] [(= posicao-blank 4) (estado-sete estado)] [else (estado-quatro estado)])
       (cond [(= posicao-blank 2) 0] [(= posicao-blank 5) (estado-oito estado)] [else (estado-cinco estado)])

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

;; verifica-sucessor : Estado String -> ResultadoTroca
;; Objetivo : dado um estado e a direção de movimento da tile blank, decide qual auxiliar de troca usar
;; retornando um resultado de troca conforme direção

;; Exemplos:
;; (verifica-sucessor ESTADO-1 "esq") = empty
;; (verifica-sucessor ESTADO-2 "baixo") = (make-estado 1 4 2 3 7 5 6 0 8)
(define (verifica-sucessor estado direcao)
  (cond
    [(string=? direcao "esq") (verifica-troca-tile-esq estado (pos-blank estado))]
    [(string=? direcao "dir") (verifica-troca-tile-dir estado (pos-blank estado))]
    [(string=? direcao "cima") (verifica-troca-tile-cima estado (pos-blank estado))]
    [(string=? direcao "baixo") (verifica-troca-tile-baixo estado (pos-blank estado))]
  )
)

;; Testes:

(check-expect (verifica-sucessor ESTADO-1 "esq") empty)
(check-expect (verifica-sucessor ESTADO-2 "baixo") (make-estado 1 4 2 3 7 5 6 0 8))

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

;; gera-sucessores : Estado -> Imagem
;; Objetivo : dado um estado, desenha uma imagem de seus sucessores lado a lado

;; Exemplos:
;; (gera-sucessores ESTADO-1) = desenho de sucessores do ESTADO-1 lado a lado
;; (gera-sucessores ESTADO-2) = desenho de sucessores do ESTADO-2 lado a lado
(define (gera-sucessores estado)
  (cond
    [(not (está-em? 0 (gera-lista-tiles estado))) empty-image]
    [else (desenha-lista-estados
           (cons (verifica-sucessor estado "esq")
                 (cons (verifica-sucessor estado "dir")
                 (cons (verifica-sucessor estado "cima")
                 (cons (verifica-sucessor estado "baixo") empty)))))]))

;; ========================================================================
;;                                 QUESTÃO 6
;; Desenvolva a função gera-sucessores-lista que, dado uma ListasDeEstados, devolve uma única 
;; ListasDeEstados com todos os estados que podem ser gerados com um movimento do blank a partir
;; de cada um dos estados recebidos. A lista retornada não pode conter estados repetidos.
;; ========================================================================

(define (retorna-sucessores-lista-estados lista-estados)
  (cond
    [(empty? lista-estados) empty]
    [(not (empty? (first lista-estados)))
     (cons (verifica-sucessor (first lista-estados) "dir")
           (cons (verifica-sucessor (first lista-estados) "esq")
           (cons (verifica-sucessor (first lista-estados) "cima")
           (cons (verifica-sucessor (first lista-estados) "baixo")
           (retorna-sucessores-lista-estados (rest lista-estados))))))]
    [else (retorna-sucessores-lista-estados (rest lista-estados))]))

(define (retorna-sucessores-estados-nao-vazios lista-estados)
  (cond
    [(empty? lista-estados) empty]
    [(not (empty? (first lista-estados)))
     (cons (first lista-estados) (retorna-sucessores-estados-nao-vazios (rest lista-estados)))]
    [else (retorna-sucessores-estados-nao-vazios (rest lista-estados))]))

;; gera-sucessores-lista : ListasDeEstados -> ListasDeEstados
;; Obj : dada uma ListasDeEstados, deve devolver uma ListasDeEstados contendo os
;; sucessores possíveis de serem gerados com um movimento do blank de cada estado,
;; sem contar sucessores repetidos
(define (gera-sucessores-lista lista-estados)
  (cond
    ;; se a lista lista-estados estiver vazia, então devolver uma imagem vazia
    [(empty? lista-estados) empty]
    ;; se o primeiro elemento da lista não se encontra no restante dela,
    ;; então desenhar os sucessores dele (usando a função gera-sucessores) ao lado
    ;; do restante dos sucessores únicos da lista
    [(not
      (estado-está-em?
       (first (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados lista-estados)))
       (rest (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados lista-estados)))))
     (cons
      (first (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados lista-estados)))
      (gera-sucessores-lista (rest (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados lista-estados)))))]
    ;; senão, retornar o restante da lista de sucessores
    [else (gera-sucessores-lista (rest (retorna-sucessores-estados-nao-vazios (retorna-sucessores-lista-estados lista-estados))))]))


;; ========================================================================
;;                                 QUESTÃO 7
;; (Bônus-Opcional: 1 Ponto)
;; Dado um estado determina quantos estados podem ser alcançados a partir
;; do estado recebido usando um número arbitrário de movimentos.
;; ========================================================================

;; conta-estados-alc : ...
;; ...