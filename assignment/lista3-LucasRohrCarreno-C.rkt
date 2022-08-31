;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista3-LucasRohrCarreno-C) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Nome: Lucas Rohr Carreño

;; ==========================================================
;; DEFINIÇÕES DE DADOS:
;; ==========================================================

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
;;   quatro : Número, tile na pocisão quatro.
;;   cinco  : Número, tile na pocisão cinco.
;;   seis   : Número, tile na pocisão seis.
;;   sete   : Número, tile na pocisão sete.
;;   oito   : Número, tile na pocisão oito.

;; --------------------
;; TIPO LISTA DE ESTADOS
;; --------------------
;; Uma ListaDeEstados é
;; 1. vazia (empty), ou
;; 2. (cons s ls), onde 
;;        s: estado;
;;       ls: ListaDeEstados

;; -----------------
;; TIPO NODO
;; -----------------
(define-struct nodo (estado valor-h valor-g ls-nodos))  
;; Um elemento do conjunto Nodo é
;;   (make-nodo s h g)
;;   s           : Estado, é um estado.
;;   h           : Número, valor-h.
;;   g           : Número, valor-g.
;;   ls-nodos : ListaDeNodos, é uma lista de nodos.

;; --------------------
;; TIPO LISTA DE NODOS
;; --------------------
;; Uma ListaDeNodos é
;; 1. vazia (empty), ou
;; 2. (cons s ls), onde 
;;        s: nodo;
;;       ls: ListaDeNodos.

(define goal (make-estado 0 1 2 3 4 5 6 7 8))
(define s1 (make-estado 1 0 2 3 4 5 6 7 8))
(define s2 (make-estado 1 4 2 3 0 5 6 7 8))
(define s3 (make-estado 1 2 3 4 5 6 7 8 0))
(define s4 (make-estado 0 6 1 7 4 2 3 8 5))
(define s5 (make-estado 5 0 2 6 4 8 1 7 3))
(define s6 (make-estado 2 4 7 0 3 6 8 1 5))

;; ==========================================================
;; FUNÇÔES AUXILIARES:
;; ==========================================================

;; ==========================================================
;; DESENHA LISTA DE ESTADOS:
;; ==========================================================

;; desenha-lista-estados: ListaDeEstados -> Imagem
;; Objetivo: Dada uma lista de estados, devolve as imagens dos estados desta lista, lado a lado.

(define (desenha-lista-estados _ls)
  (cond
    [(empty? _ls) empty-image]
    [else (beside (desenha-estado (first _ls)) (rectangle 30 0 "outline" "white") (desenha-lista-estados (rest _ls)))]))

;;(check-expect (desenha-lista-estados (list goal)) .)
;;(check-expect (desenha-lista-estados (list s1 s2 s3 s4 s5 s6)) .)


;; desenha-estado: Estado -> Imagem
;; Objetivo: Dada um estado, devolve a imagem do estado.

(define (desenha-estado _estado)
  (above 
   (beside
    (desenha-tile (estado-zero _estado)) (desenha-tile (estado-um _estado)) (desenha-tile (estado-dois _estado)))
   (beside
    (desenha-tile (estado-tres _estado)) (desenha-tile (estado-quatro _estado)) (desenha-tile (estado-cinco _estado)))
   (beside
    (desenha-tile (estado-seis _estado)) (desenha-tile (estado-sete _estado)) (desenha-tile (estado-oito _estado)))))

;;(check-expect (desenha-estado goal) .)
;;(check-expect (desenha-estado s3) .)



;; desenha-tile Número -> Imagem
;; Objetivo: Dada um número, devolve a imagem de um tile com o número.

(define (desenha-tile tile)
  (overlay
   (text (number->string tile) 20 "black")
   (rectangle 30 30 "outline" "black")))

;;(check-expect (desenha-tile 2) (overlay (text (number->string 2) 20 "black") (rectangle 30 30 "outline" "black")))

;; (check-expect (desenha-tile 0) (overlay (text (number->string 0) 20 "black") (rectangle 30 30 "outline" "black")))

;; ==========================================================
;; GERA SUCESSORES:
;; ==========================================================

;; --------------------
;; TIPO LISTA DE NÚMEROS
;; --------------------
;; Uma ListaDeNúmeros é
;; 1. vazia (empty), ou
;; 2. (cons s ls), onde 
;;        s: Número;
;;       ls: ListaDeNumeros.

;; Constantes do tipo lista de números para guardar as posições do blank que permitem movimentos em cada sentido.

(define LISTA-MOVE-DIR (list 0 1 3 4 6 7))
(define LISTA-MOVE-ESQ (list 1 2 4 5 7 8))
(define LISTA-MOVE-CIMA (list 3 4 5 6 7 8))
(define LISTA-MOVE-BAIXO (list 0 1 2 3 4 5))

;; está-em? Estado -> ListaDeNúmeros
;; Objetivo: Dado um número e uma lista de números, diz se este número pertence a lista.

(define (está-em? _pos _lista-pos)
  (cond
    [(empty? _lista-pos) false]
    [(= _pos (first _lista-pos)) true]
    [else (está-em? _pos (rest _lista-pos))]))

(check-expect (está-em? 0 (list 0 1 3 4 6 7)) true)

(check-expect (está-em? 10 (list 0 1 3 4 6 7)) false)

;; pos-blank:  Estado -> Número
;; Objetivo: Dado um estado retorna a posição do tile blank.

(define (pos-blank _estado)
  (cond
    [(= 0 (estado-zero _estado)) 0]
    [(= 0 (estado-um _estado)) 1]
    [(= 0 (estado-dois _estado)) 2]
    [(= 0 (estado-tres _estado)) 3]
    [(= 0 (estado-quatro _estado)) 4]
    [(= 0 (estado-cinco _estado)) 5]
    [(= 0 (estado-seis _estado)) 6] 
    [(= 0 (estado-sete _estado)) 7]
    [(= 0 (estado-oito _estado)) 8]))

(check-expect (pos-blank goal) 0)

(check-expect (pos-blank s3) 8)

;; valor-pos-estado: Número Estado -> Número
;; Objetivo: Dados uma posição e um estado, devolve o valor guardado nesta posição do estado.

(define (valor-pos-estado _pos _estado)
  (cond
    [(= 0 _pos) (estado-zero _estado)]
    [(= 1 _pos) (estado-um _estado)]
    [(= 2 _pos) (estado-dois _estado)]
    [(= 3 _pos) (estado-tres _estado)]
    [(= 4 _pos) (estado-quatro _estado)]
    [(= 5 _pos) (estado-cinco _estado)]
    [(= 6 _pos) (estado-seis _estado)] 
    [(= 7 _pos) (estado-sete _estado)]
    [(= 8 _pos) (estado-oito _estado)]))

(check-expect (valor-pos-estado 0 goal) 0)

(check-expect (valor-pos-estado 8 s3) 0)

;; seleciona-lista: String -> ListaDeNúmeros
;; Objetivo: Dado um sentido, devolve a lista de posições de movimento possíveis neste sentido.

(define (seleciona-lista _sentido)
  (cond
    [(string=? "DIR" _sentido) LISTA-MOVE-DIR]
    [(string=? "ESQ" _sentido) LISTA-MOVE-ESQ]
    [(string=? "CIMA" _sentido) LISTA-MOVE-CIMA]
    [(string=? "BAIXO" _sentido) LISTA-MOVE-BAIXO]))

(check-expect (seleciona-lista "DIR") LISTA-MOVE-DIR)

(check-expect (seleciona-lista "BAIXO") LISTA-MOVE-BAIXO)

;; seleciona-troca: Número String -> Número
;; Objetivo: Dados a posição do blank e um sentido, devolve a posição que deverá trocar com o blank.

(define (seleciona-troca _blank _sentido)
  (cond
    [(string=? "DIR" _sentido)   (+ _blank 1)]
    [(string=? "ESQ" _sentido)   (- _blank 1)]
    [(string=? "CIMA" _sentido)  (- _blank 3)]
    [(string=? "BAIXO" _sentido) (+ _blank 3)]))

(check-expect (seleciona-troca (pos-blank goal) "DIR") 1)

(check-expect (seleciona-troca (pos-blank s3) "ESQ") 7)

;; define-pos: Número Número Número Estado -> Número
;; Objetivo: Dados a posição atual e duas outras posições p1 e p2, se a atual for p1 ou p2, retorna o valor da outra posição (se for p1, retorna p2 e vice versa, para trocar os valores),
;; senão retorna o valor da posição atual.

(define (define-pos _pos _pos1 _pos2 _estado)
  (cond
         [(= _pos _pos1) (valor-pos-estado _pos2 _estado)]
         [(= _pos _pos2) (valor-pos-estado _pos1 _estado)]
         [else (valor-pos-estado _pos _estado)]))

(check-expect (define-pos 0 (pos-blank goal) (seleciona-troca (pos-blank goal) "DIR") goal) 1)

(check-expect (define-pos 8 (pos-blank s3) (seleciona-troca (pos-blank s3) "ESQ") s3) 8)

;; gera-movimento: Estado Número Número -> Estado
;; Objetivo: Dados o estado atual, e as posições que devem ser trocadas, gera o próximo estado.

(define (gera-movimento _estado _pos1 _pos2)
  (make-estado (define-pos 0 _pos1 _pos2 _estado)
               (define-pos 1 _pos1 _pos2 _estado)
               (define-pos 2 _pos1 _pos2 _estado)
               (define-pos 3 _pos1 _pos2 _estado)
               (define-pos 4 _pos1 _pos2 _estado)
               (define-pos 5 _pos1 _pos2 _estado)
               (define-pos 6 _pos1 _pos2 _estado)
               (define-pos 7 _pos1 _pos2 _estado)
               (define-pos 8 _pos1 _pos2 _estado)))

(check-expect (gera-movimento goal (pos-blank goal) (seleciona-troca (pos-blank goal) "DIR")) (make-estado 1 0 2 3 4 5 6 7 8))

(check-expect (gera-movimento s3 (pos-blank s3) (seleciona-troca (pos-blank s3) "ESQ")) (make-estado 1 2 3 4 5 6 7 0 8))

;; gera-sucessor: String Estado -> ListaDeEstados
;; Objetivo: Dado um sentdo e um estado, gera uma lista contendo o estado sucessor deste estado, se houver,
;; ou uma lista vazia, caso não seja possível um movimento neste sentido.

(define (gera-sucessor _sentido _estado)
  (cond
    ;; Se o movimento não for possível, devolver a lista vazia.
    [(not (está-em? (pos-blank _estado) (seleciona-lista _sentido))) empty]
    ;; Senão, fazer o movimento, gerando uma lista com um elemento apenas, o novo estado.
    [else (list (gera-movimento _estado (pos-blank _estado) (seleciona-troca (pos-blank _estado) _sentido)))]))

(check-expect (gera-sucessor "DIR" goal) (list (make-estado 1 0 2 3 4 5 6 7 8)))

(check-expect (gera-sucessor "DIR" s3) empty)

;; gera-sucessores: Estado -> ListaDeEstados
;; Objetivo: Dado um estado retorna uma lista de estados.

(define (gera-sucessores _estado)
  (append
   (gera-sucessor "ESQ" _estado)
   (gera-sucessor "BAIXO" _estado)
   (gera-sucessor "DIR" _estado)
   (gera-sucessor "CIMA" _estado)))

(check-expect (gera-sucessores goal) (list
 (make-estado 3 1 2 0 4 5 6 7 8)
 (make-estado 1 0 2 3 4 5 6 7 8)))

(check-expect (gera-sucessores s2) (list
 (make-estado 1 4 2 0 3 5 6 7 8)
 (make-estado 1 4 2 3 7 5 6 0 8)
 (make-estado 1 4 2 3 5 0 6 7 8)
 (make-estado 1 0 2 3 4 5 6 7 8)))

(check-expect (gera-sucessores s3) (list (make-estado 1 2 3 4 5 6 7 0 8) (make-estado 1 2 3 4 5 0 7 8 6)))

;; =========================================
;; NÃO MODIFIQUE O CÓDIGO ACIMA DESTA LINHA. 
;; =========================================

;; Definição de constantes do tipo Estado:

(define ESTADO-1 (make-estado 0 1 2 3 4 5 6 7 8))
(define ESTADO-2 (make-estado 1 4 2 3 0 5 6 7 8))
(define ESTADO-3 (make-estado 4 5 8 1 1 2 0 7 1))

;; Definição de constantes de Nodo:

(define NODO-1 (make-nodo ESTADO-1 0 5 empty))
(define NODO-2 (make-nodo ESTADO-2 2 0 empty))

(define NODO-1-EXPANDIDO (make-nodo ESTADO-1 0 5 (list (make-nodo (make-estado 3 1 2 0 4 5 6 7 8) 1 6 '()) (make-nodo (make-estado 1 0 2 3 4 5 6 7 8) 1 6 '()))))
(define NODO-2-EXPANDIDO (make-nodo ESTADO-2 2 0 (list
  (make-nodo (make-estado 1 4 2 0 3 5 6 7 8) 3 1 '())
  (make-nodo (make-estado 1 4 2 3 7 5 6 0 8) 3 1 '())
  (make-nodo (make-estado 1 4 2 3 5 0 6 7 8) 3 1 '())
  (make-nodo (make-estado 1 0 2 3 4 5 6 7 8) 1 1 '()))))

(define LISTA-NODOS-1 (list NODO-1 NODO-2))
(define LISTA-NODOS-2 (list NODO-2))

;; =========================================
;; 11111111111111111111111111111111111111111 
;; =========================================

;; retorna-valor-soma-tile-objetivo : Número Número Número -> Número
;; Objetivo : dado um valor de tile, posição da tile no estado e a posição blank no estado,
;; retorna uma unidade para soma heurística caso a tile não seja o valor objetivo ou blank

;; Exemplos:
;; (retorna-valor-soma-tile-objetivo 4 0 8) = 1
;; (retorna-valor-soma-tile-objetivo 1 1 0) = 0
(define (retorna-valor-soma-tile-objetivo tile posicao posicao-blank)
   (cond
      [(and (not (= tile posicao)) (not (= tile posicao-blank))) 1]
      [else 0]))

;; Testes:

(check-expect (retorna-valor-soma-tile-objetivo 4 0 8) 1)
(check-expect (retorna-valor-soma-tile-objetivo 1 1 0) 0)


;; função-heurística : Estado -> Número
;; Objetivo : dado um estado, retorna a soma do número de tiles que não estão na posição objetivo, excluindo o tile blank

;; Exemplos:
;; (função-heurística ESTADO-1) = 0
;; (função-heurística ESTADO-2) = 0
(define (função-heurística estado)
  (+
   (retorna-valor-soma-tile-objetivo (estado-zero estado) 0 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-um estado) 1 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-dois estado) 2 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-tres estado) 3 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-quatro estado) 4 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-cinco estado) 5 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-seis estado) 6 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-sete estado) 7 (pos-blank estado))
   (retorna-valor-soma-tile-objetivo (estado-oito estado) 8 (pos-blank estado))))

;; Testes:

(check-expect (função-heurística ESTADO-1) 0)
(check-expect (função-heurística ESTADO-2) 2)

;; cria-nodo : Estado Número -> Nodo
;; Objetivo : dado um estado e um valor G, cria um nodo com o estado e valor G, chamando
;; função-heurística para valor H, com uma lista vazia de nodos no nodo

;; Exemplos:
;; (cria-nodo ESTADO-1 5) = NODO-1
;; (cria-nodo ESTADO-2 8) = NODO-2
(define (cria-nodo estado valor-g)
  (make-nodo estado (função-heurística estado) valor-g empty))

;; Testes:

(check-expect (cria-nodo ESTADO-1 5) NODO-1)
(check-expect (cria-nodo ESTADO-2 0) NODO-2)

;; =========================================
;; 22222222222222222222222222222222222222222 
;; =========================================

;; retorna-nodos-sucessores : ListaDeEstados Número -> ListaDeNodos
;; Objetivo : dada uma lista de estados e um valor G, retorna uma lista de nodos com o estado
;; de cada um sendo um dos estados da lista de estados, sendo estes sucessores do estado de um nodo

;; Exemplos:
;; (retorna-nodos-sucessores (list (make-estado 3 1 2 0 4 5 6 7 8) (make-nodo (make-estado 1 0 2 3 4 5 6 7 8)) 5) =
;; (list (make-nodo (make-estado 3 1 2 0 4 5 6 7 8) 1 5 '()) (make-nodo (make-estado 1 0 2 3 4 5 6 7 8) 1 5 '()))

;; (retorna-nodos-sucessores (list (make-estado 1 4 2 0 3 5 6 7 8) (make-estado 1 4 2 3 7 5 6 0 8) (make-estado 1 4 2 3 5 0 6 7 8) (make-estado 1 0 2 3 4 5 6 7 8)) 2) =
;; (list
;;   (make-nodo (make-estado 1 4 2 0 3 5 6 7 8) 3 8 '())
;;   (make-nodo (make-estado 1 4 2 3 7 5 6 0 8) 3 8 '())
;;   (make-nodo (make-estado 1 4 2 3 5 0 6 7 8) 3 8 '())
;;   (make-nodo (make-estado 1 0 2 3 4 5 6 7 8) 1 8 '()))
(define (retorna-nodos-sucessores estados-sucessores valor-g-nodo)
  (cond
    ;; Se a lista de estados sucessores estiver vazia, retornar vazio
    [(empty? estados-sucessores) empty]
    ;; Senão, retornar um nodo sucessor com o primeiro estado sucessor como estado e valor G + 1
    ;; juntamente do restante da lista de nodos sucessores
    [else (cons (cria-nodo (first estados-sucessores) (+ valor-g-nodo 1))
                (retorna-nodos-sucessores (rest estados-sucessores) valor-g-nodo))]))

;; Testes:

(check-expect (retorna-nodos-sucessores (list (make-estado 1 4 2 0 3 5 6 7 8) (make-estado 1 4 2 3 7 5 6 0 8) (make-estado 1 4 2 3 5 0 6 7 8) (make-estado 1 0 2 3 4 5 6 7 8)) 2)
              (list
               (make-nodo (make-estado 1 4 2 0 3 5 6 7 8) 3 3 '())
               (make-nodo (make-estado 1 4 2 3 7 5 6 0 8) 3 3 '())
               (make-nodo (make-estado 1 4 2 3 5 0 6 7 8) 3 3 '())
               (make-nodo (make-estado 1 0 2 3 4 5 6 7 8) 1 3 '())))

(check-expect (retorna-nodos-sucessores (list (make-estado 3 1 2 0 4 5 6 7 8) (make-estado 1 0 2 3 4 5 6 7 8)) 5)
              (list (make-nodo (make-estado 3 1 2 0 4 5 6 7 8) 1 6 '()) (make-nodo (make-estado 1 0 2 3 4 5 6 7 8) 1 6 '())))

;; expande-nodo : Nodo -> Nodo
;; Objetivo : dado um nodo, adiciona os sucessores do estado do nodo como os nodos do original passado para a função

;; Exemplos:
;; (expande-nodo NODO-1) = NODO-1-EXPANDIDO
;; (expande-nodo NODO-2) = NODO-2-EXPANDIDO
(define (expande-nodo nodo)
  (make-nodo
   (nodo-estado nodo)
   (nodo-valor-h nodo)
   (nodo-valor-g nodo)
   (retorna-nodos-sucessores (gera-sucessores (nodo-estado nodo)) (nodo-valor-g nodo))))

;; Testes:

(check-expect (expande-nodo NODO-1) NODO-1-EXPANDIDO)
(check-expect (expande-nodo NODO-2) NODO-2-EXPANDIDO)

;; =========================================
;; 33333333333333333333333333333333333333333
;; =========================================

;; desenha-info-nodo : Nodo -> Imagem
;; Objetivo : dado um nodo, retorna seu estado desenhado, junto do valor g, h e f (sendo este g + h)

;; Exemplos:
;; (desenha-info-nodo NODO-1) = desenha estado e valores do nodo 1
;; (desenha-info-nodo NODO-2) = desenha estado e valores do nodo 2
(define (desenha-info-nodo nodo)
  (cond
    [(empty? nodo) empty-image]
    [else
     (above
      (desenha-estado (nodo-estado nodo))
      (text (string-append "valor-h:" (number->string (nodo-valor-h nodo))) 15 "black")
      (text (string-append "valor-g:" (number->string (nodo-valor-g nodo))) 15 "black")
      (text (string-append "valor-f:" (number->string (+ (nodo-valor-h nodo) (nodo-valor-g nodo)))) 15 "black"))]))

;; desenha-nodos-sucessores : ListaDeNodos -> Imagem
;; Objetivo : dado uma lista de nodos sucessores, retorna o primeiro nodo sucessor, com seus respectivos sucessores,
;; ao lado dos outros nodos sucessores, chamando de volta a função desenha-nodo

;; Exemplos:
;; (desenha-nodos-sucessores NODO-1) = desenha nodos sucessores do nodo 1
;; (desenha-nodos-sucessores NODO-2) = desenha nodos sucessores do nodo 2
(define (desenha-nodos-sucessores nodos-sucessores)
  (cond
    ;; Se a lista de nodos sucessores estiver vazia, retornar vazio
    [(empty? nodos-sucessores) empty-image]
    ;; Senão, retornar o desenho do primeiro nodo sucessor junto de sua própria árvore de sucessores,
    ;; ao lado dos desenhos dos outros nodos sucessores e suas árvores
    [else
     (beside
      (desenha-nodo (first nodos-sucessores))
      (rectangle 30 20 "solid" "white")
      (desenha-nodos-sucessores (rest nodos-sucessores)))]))

;; desenha-nodo : Nodo -> Imagem
;; Objetivo : dado um nodo, desenha o mesmo e todos os outros nodos existentes na árvore
;; onde o nodo é raiz

;; Exemplos:
;; (desenha-nodo NODO-2) = desenha árvore do nodo 2
;; (desenha-nodo NODO-2-EXPANDIDO) = desenha árvore do nodo 2 expandido
(define (desenha-nodo nodo)
  (cond
    ;; Se o nodo for vazio, retornar uma imagem vazia
    [(empty? nodo) empty-image]
    ;; Senão, retornar a info do nodo junto das de seus sucessores da árvore
    [else
     (above
      (desenha-info-nodo nodo)
      (rectangle 30 30 "solid" "white")
      (desenha-nodos-sucessores (nodo-ls-nodos nodo)))]))

(desenha-nodo NODO-2)
(desenha-nodo NODO-2-EXPANDIDO)

;; =========================================
;; 44444444444444444444444444444444444444444
;; =========================================

;; Definições de constantes para testes:

(define ARVORE-NODO-2-G2
  (make-nodo
   (make-estado 1 4 2 3 0 5 6 7 8)
   2
   0
   (list
    (make-nodo
     (make-estado 1 4 2 0 3 5 6 7 8)
     3
     1
     (list
      (make-nodo (make-estado 1 4 2 6 3 5 0 7 8) 4 2 (list '() '()))
      (make-nodo (make-estado 1 4 2 3 0 5 6 7 8) 2 2 (list '() '() '() '()))
      (make-nodo (make-estado 0 4 2 1 3 5 6 7 8) 3 2 (list '() '()))))
    (make-nodo
     (make-estado 1 4 2 3 7 5 6 0 8)
     3
     1
     (list
      (make-nodo (make-estado 1 4 2 3 7 5 0 6 8) 4 2 (list '() '()))
      (make-nodo (make-estado 1 4 2 3 7 5 6 8 0) 4 2 (list '() '()))
      (make-nodo (make-estado 1 4 2 3 0 5 6 7 8) 2 2 (list '() '() '() '()))))
    (make-nodo
     (make-estado 1 4 2 3 5 0 6 7 8)
     3
     1
     (list
      (make-nodo (make-estado 1 4 2 3 0 5 6 7 8) 2 2 (list '() '() '() '()))
      (make-nodo (make-estado 1 4 2 3 5 8 6 7 0) 4 2 (list '() '()))
      (make-nodo (make-estado 1 4 0 3 5 2 6 7 8) 4 2 (list '() '()))))
    (make-nodo
     (make-estado 1 0 2 3 4 5 6 7 8)
     1
     1
     (list
      (make-nodo (make-estado 0 1 2 3 4 5 6 7 8) 0 2 (list '() '()))
      (make-nodo (make-estado 1 4 2 3 0 5 6 7 8) 2 2 (list '() '() '() '()))
      (make-nodo (make-estado 1 2 0 3 4 5 6 7 8) 2 2 (list '() '())))))))

(define ARVORE-NODO-1-G3 '())

(define LISTA-NODOS-GERA-ARVORE-SUCESSORES-TESTE-1
  (list
 '()
 (make-nodo
  (make-estado 1 4 2 3 0 5 6 7 8)
  2
  0
  (list
   (make-nodo
    (make-estado 1 4 2 0 3 5 6 7 8)
    3
    1
    (list
     (make-nodo
      (make-estado 1 4 2 6 3 5 0 7 8)
      4
      2
      (list
       (make-nodo
        (make-estado 1 4 2 6 3 5 7 0 8)
        5
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 0 3 5 6 7 8)
        3
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list
       (make-nodo
        (make-estado 1 4 2 0 3 5 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 7 5 6 0 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 5 0 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 0 2 3 4 5 6 7 8)
        1
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 0 4 2 1 3 5 6 7 8)
      3
      2
      (list
       (make-nodo
        (make-estado 1 4 2 0 3 5 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 4 0 2 1 3 5 6 7 8)
        3
        3
        (list '() '() '()))))))
   (make-nodo
    (make-estado 1 4 2 3 7 5 6 0 8)
    3
    1
    (list
     (make-nodo
      (make-estado 1 4 2 3 7 5 0 6 8)
      4
      2
      (list
       (make-nodo
        (make-estado 1 4 2 3 7 5 6 0 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 0 7 5 3 6 8)
        5
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 1 4 2 3 7 5 6 8 0)
      4
      2
      (list
       (make-nodo
        (make-estado 1 4 2 3 7 5 6 0 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 7 0 6 8 5)
        5
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list
       (make-nodo
        (make-estado 1 4 2 0 3 5 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 7 5 6 0 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 5 0 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 0 2 3 4 5 6 7 8)
        1
        3
        (list '() '() '()))))))
   (make-nodo
    (make-estado 1 4 2 3 5 0 6 7 8)
    3
    1
    (list
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list
       (make-nodo
        (make-estado 1 4 2 0 3 5 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 7 5 6 0 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 5 0 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 0 2 3 4 5 6 7 8)
        1
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 1 4 2 3 5 8 6 7 0)
      4
      2
      (list
       (make-nodo
        (make-estado 1 4 2 3 5 8 6 0 7)
        5
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 5 0 6 7 8)
        3
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 1 4 0 3 5 2 6 7 8)
      4
      2
      (list
       (make-nodo
        (make-estado 1 0 4 3 5 2 6 7 8)
        4
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 5 0 6 7 8)
        3
        3
        (list '() '() '()))))))
   (make-nodo
    (make-estado 1 0 2 3 4 5 6 7 8)
    1
    1
    (list
     (make-nodo
      (make-estado 0 1 2 3 4 5 6 7 8)
      0
      2
      (list
       (make-nodo
        (make-estado 3 1 2 0 4 5 6 7 8)
        1
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 0 2 3 4 5 6 7 8)
        1
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list
       (make-nodo
        (make-estado 1 4 2 0 3 5 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 7 5 6 0 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 4 2 3 5 0 6 7 8)
        3
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 0 2 3 4 5 6 7 8)
        1
        3
        (list '() '() '()))))
     (make-nodo
      (make-estado 1 2 0 3 4 5 6 7 8)
      2
      2
      (list
       (make-nodo
        (make-estado 1 0 2 3 4 5 6 7 8)
        1
        3
        (list '() '() '()))
       (make-nodo
        (make-estado 1 2 5 3 4 0 6 7 8)
        3
        3
        (list '() '() '()))))))))))

(define LISTA-NODOS-GERA-ARVORE-SUCESSORES-TESTE-2
  (list
 (make-nodo
  (make-estado 1 4 2 3 0 5 6 7 8)
  2
  0
  (list
   (make-nodo
    (make-estado 1 4 2 0 3 5 6 7 8)
    3
    1
    (list
     (make-nodo
      (make-estado 1 4 2 6 3 5 0 7 8)
      4
      2
      (list '() '()))
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list '() '() '() '()))
     (make-nodo
      (make-estado 0 4 2 1 3 5 6 7 8)
      3
      2
      (list '() '()))))
   (make-nodo
    (make-estado 1 4 2 3 7 5 6 0 8)
    3
    1
    (list
     (make-nodo
      (make-estado 1 4 2 3 7 5 0 6 8)
      4
      2
      (list '() '()))
     (make-nodo
      (make-estado 1 4 2 3 7 5 6 8 0)
      4
      2
      (list '() '()))
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list '() '() '() '()))))
   (make-nodo
    (make-estado 1 4 2 3 5 0 6 7 8)
    3
    1
    (list
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list '() '() '() '()))
     (make-nodo
      (make-estado 1 4 2 3 5 8 6 7 0)
      4
      2
      (list '() '()))
     (make-nodo
      (make-estado 1 4 0 3 5 2 6 7 8)
      4
      2
      (list '() '()))))
   (make-nodo
    (make-estado 1 0 2 3 4 5 6 7 8)
    1
    1
    (list
     (make-nodo
      (make-estado 0 1 2 3 4 5 6 7 8)
      0
      2
      (list '() '()))
     (make-nodo
      (make-estado 1 4 2 3 0 5 6 7 8)
      2
      2
      (list '() '() '() '()))
     (make-nodo
      (make-estado 1 2 0 3 4 5 6 7 8)
      2
      2
      (list '() '()))))))))

;; gera-árvore-sucessores : ListaDeNodos Número -> ListaDeNodos
;; Objetivo : dada uma lista de nodos e um número k, retorna uma lista de nodos sucessores,
;; usando gera-árvore recursivamente

;; Exemplos:
;; (gera-árvore-sucessores LISTA-NODOS-1 3) = LISTA-NODOS-GERA-ARVORE-SUCESSORES-TESTE-1
;; (gera-árvore-sucessores LISTA-NODOS-2 2) = LISTA-NODOS-GERA-ARVORE-SUCESSORES-TESTE-2
(define (gera-árvore-sucessores lista-nodos numero-k)
  (cond
    ;; Se a lista de nodos estiver vazia, retornar vazio
    [(empty? lista-nodos) empty]
    ;; Senão, retornar a árvore gerada a partir do primeiro nodo junto
    ;; do restante das árvores geradas a partir do resto de nodos
    [else
     (append
      (list (gera-árvore (first lista-nodos) numero-k))
      (gera-árvore-sucessores (rest lista-nodos) numero-k))]))

;; Testes:

(check-expect (gera-árvore-sucessores LISTA-NODOS-1 3) LISTA-NODOS-GERA-ARVORE-SUCESSORES-TESTE-1)
(check-expect (gera-árvore-sucessores LISTA-NODOS-2 2) LISTA-NODOS-GERA-ARVORE-SUCESSORES-TESTE-2)
 
;; gera-árvore : Nodo Número -> Nodo
;; Objetivo : dado um nodo e um número k, retorna um nodo (árvore)
;; com todos os sucessores do nodo (incluindo o nodo) que tem valor-g menor ou igual a k

;; Exemplos:
;; (gera-árvore NODO-1 3) = ARVORE-NODO-1-G3
;; (gera-árvore NODO-2 2) = ARVORE-NODO-2-G2
(define (gera-árvore nodo numero-k)
  (cond
    ;; Se o valor g do nodo for menor que o valor k, retornar o nodo com seus sucessores
    ;; a partir do nodo expandido
    [(<= (nodo-valor-g nodo) numero-k)
     (make-nodo
      (nodo-estado nodo)
      (nodo-valor-h nodo)
      (nodo-valor-g nodo)
      (gera-árvore-sucessores (nodo-ls-nodos (expande-nodo nodo)) numero-k))]
    ;; Senão, retornar os nodos inalterados sem expandir o nodo
    [else (gera-árvore-sucessores (nodo-ls-nodos nodo) numero-k)]))

;; Testes:

(check-expect (gera-árvore NODO-1 3) ARVORE-NODO-1-G3)
(check-expect (gera-árvore NODO-2 2) ARVORE-NODO-2-G2)

(desenha-nodo (gera-árvore NODO-2 2))

;; =========================================
;; 55555555555555555555555555555555555555555
;; =========================================

;; estado-é-igual? : Estado Estado -> Booleano
;; Objetivo : dados dois estados, verifica se são iguais

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


;; retorna-nodos-com-estado-lista : ListaDeNodos Estado -> ListaNodos
;; Objetivo : dada uma lista de nodos e um estado, retorna uma lista com nodos contendo o estado usando
;; retorna-nodos-com-estado recursivamente

;; Exemplos:
;; (retorna-nodos-com-estado-lista LISTA-NODOS-1 ESTADO-1) = (list (make-nodo (make-estado 0 1 2 3 4 5 6 7 8) 0 5 '()))
;; (retorna-nodos-com-estado-lista LISTA-NODOS-2 ESTADO-1) = '()
(define (retorna-nodos-com-estado-lista lista-nodos estado)
  (cond
    ;; Se a lista de nodos estiver vazia, retornar vazio
    [(empty? lista-nodos) empty]
    ;; Senão, combina em uma lista o primeiro nodo com o estado em questão junto do
    ;; restante dos outros nodos contendo o estado
    [else
     (append
      (retorna-nodos-com-estado (first lista-nodos) estado)
      (retorna-nodos-com-estado-lista (rest lista-nodos) estado))]))

;; Testes:

(check-expect (retorna-nodos-com-estado-lista LISTA-NODOS-1 ESTADO-1) (list (make-nodo (make-estado 0 1 2 3 4 5 6 7 8) 0 5 '())))
(check-expect (retorna-nodos-com-estado-lista LISTA-NODOS-2 ESTADO-1) '())


;; retorna-nodos-com-estado : Nodo Estado -> ListaNodos
;; Objetivo : dado um nodo e um estado, retorna uma lista com nodos contendo o estado, comparando se os estados são iguais
;; usando retorna-nodos-com-estado-lista recursivamente

;; Exemplos:
;; (retorna-nodos-com-estado NODO-1-EXPANDIDO ESTADO-1) = (list (make-nodo (make-estado 0 1 2 3 4 5 6 7 8) 0 5 '()))
;; (retorna-nodos-com-estado NODO-2-EXPANDIDO ESTADO-1) = '()
(define (retorna-nodos-com-estado nodo estado)
  (cond
    ;; Se o estado for igual, retornar uma lista com o nodo e os outros nodos com o estado
    [(estado-é-igual? (nodo-estado nodo) estado) (cons nodo (retorna-nodos-com-estado-lista (nodo-ls-nodos nodo) estado))]
    ;; Senão, retorna apenas os outros nodos com o estado
    [else (retorna-nodos-com-estado-lista (nodo-ls-nodos nodo) estado)]))

;; Testes:

(check-expect (retorna-nodos-com-estado NODO-1-EXPANDIDO ESTADO-1)
              (list
               (make-nodo
                (make-estado 0 1 2 3 4 5 6 7 8)
                0
                5
                (list
                 (make-nodo
                  (make-estado 3 1 2 0 4 5 6 7 8)
                  1
                  6
                  '())
                 (make-nodo
                  (make-estado 1 0 2 3 4 5 6 7 8)
                  1
                  6
                  '())))))
(check-expect (retorna-nodos-com-estado NODO-2-EXPANDIDO ESTADO-1) '())


;; retorna-menor-valor-g-nodos : ListaDeNodos Número -> Número
;; Objetivo : dada uma lista de nodos e um menor valor g, percorre recursivamente a lista atualizando o menor valor
;; com o menor valor g dentre os nodos

;; Exemplos:
;; (retorna-menor-valor-g-nodos LISTA-NODOS-1 1) = 1
;; (retorna-menor-valor-g-nodos LISTA-NODOS-2 1) = 1
(define (retorna-menor-valor-g-nodos lista-nodos menor-valor)
  (cond
    ;; Se a lista de nodos estiver vazia, retornar o menor valor (pois percorreu toda lista)
    [(empty? lista-nodos) menor-valor]
    ;; Se o valor g do primeiro nodo for menor que o menor valor atual,
    ;; atualizar o menor valor g avançando no restante da lista
    [(< (nodo-valor-g (first lista-nodos)) menor-valor) (retorna-menor-valor-g-nodos (rest lista-nodos) (nodo-valor-g (first lista-nodos)))]
    ;; Senão, avança no restante da lista
    [else (retorna-menor-valor-g-nodos (rest lista-nodos) menor-valor)]))

;; Testes:

(check-expect (retorna-menor-valor-g-nodos LISTA-NODOS-1 1) 0)
(check-expect (retorna-menor-valor-g-nodos LISTA-NODOS-2 1) 0)

;; ---------------------
;; TIPO RetornoBuscaEstado
;; ---------------------
;; Uma  RetornoBuscaEstado é
;; 1. Uma Número, ou
;; 2. Uma String

;; Constante para mensagem:

(define MENSAGEM-ESTADO-NAO-ENCONTRADO "Estado não está contido na árvore.")

;; busca-estado : Nodo Estado -> RetornoBuscaEstado
;; Objetivo : dado um nodo e um estado, retorna o menor valor g dentro dos nodos que possuem o estado
;; ou uma mensagem string caso nenhum nodo tenha o estado

;; Exemplos:
;; (busca-estado NODO-2-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) = 1
;; (busca-estado NODO-2-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) = MENSAGEM-ESTADO-NAO-ENCONTRADO
(define (busca-estado nodo estado)
  (cond
    ;; Se a lista de nodos com o estado estiver vazia, retornar mensagem de não encontrado
    [(empty? (retorna-nodos-com-estado nodo estado)) MENSAGEM-ESTADO-NAO-ENCONTRADO]
    ;; Senão, retornar o menor valor g dentro os nodos que contém o estado
    [else (retorna-menor-valor-g-nodos (retorna-nodos-com-estado nodo estado) (nodo-valor-g (first (retorna-nodos-com-estado nodo estado))))]))

;; Testes:

(check-expect (busca-estado NODO-2-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) 1)
(check-expect (busca-estado NODO-1-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) MENSAGEM-ESTADO-NAO-ENCONTRADO)


;; conta-exp-f-lista : ListaDeNodos Número -> Número
;; Objetivo : dada uma lista de nodos e um número k, retorna a soma total dos nodos expandidos com
;; valor f (g + h) maiores do que k, usando conta-exp-f recursivamente

;; Exemplos:
;; (conta-exp-f-lista (nodo-ls-nodos NODO-2-EXPANDIDO) 10) = 1
;; (conta-exp-f-lista (nodo-ls-nodos NODO-2-EXPANDIDO) 1) = 0
(define (conta-exp-f-lista lista-nodos numero-k)
  (cond
    ;; Se a lista de nodos estiver vazia, retornar 0
    [(empty? lista-nodos) 0]
    ;; Senão, retornar a soma de 1, caso o primeiro nodo for expandido e
    ;; com f menor que k, junto à soma total do resto de nodos que cumpre os requisitos
    [else
     (+
      (conta-exp-f (first lista-nodos) numero-k)
      (conta-exp-f-lista (rest lista-nodos) numero-k))]))

;; Testes:

(check-expect (conta-exp-f-lista (nodo-ls-nodos NODO-2-EXPANDIDO) 10) 0)
(check-expect (conta-exp-f-lista (nodo-ls-nodos NODO-2-EXPANDIDO) 1) 0)


;; conta-exp-f : Nodo Número -> Número
;; Objetivo : dado um nodo e um número k, retorna o total de nodos que não são folhas que possuem
;; o valor f (g + h) menores do que o número k

;; Exemplos:
;; (conta-exp-f NODO-2-EXPANDIDO 10) = 1
;; (conta-exp-f NODO-2-EXPANDIDO 1) = 0
(define (conta-exp-f nodo numero-k)
  (cond
    ;; Se o nodo tiver nodos sucessores (for expandido) e seu f for maior do que o número k,
    ;; retornar o total de nodos expandidos de mesma condição somando 1
    [(and (not (empty? (nodo-ls-nodos nodo))) (< (+ (nodo-valor-g nodo) (nodo-valor-h nodo)) numero-k))
     (+ 1 (conta-exp-f-lista (nodo-ls-nodos nodo) numero-k))]
    ;; Senão, retornar o total de nodos expandidos de mesma condição
    [else (conta-exp-f-lista (nodo-ls-nodos nodo) numero-k)]))

;; Testes:

(check-expect (conta-exp-f NODO-2-EXPANDIDO 10) 1)
(check-expect (conta-exp-f NODO-2-EXPANDIDO 1) 0)

;; =========================================
;; 66666666666666666666666666666666666666666
;; =========================================