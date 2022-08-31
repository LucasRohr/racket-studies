;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista3-LucasRohrCarreno-C) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
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
;; Objetivo : dado um valor de tile, posição da tila no estado e a posição blank no estado
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
;; função-heurística para valor H e uma lista vazia de nodos

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
;; Objetivo : dado um estado e um valor G, cria um nodo com o estado e valor G, chamando
;; função-heurística para valor H e uma lista vazia de nodos

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
    [(empty? estados-sucessores) empty]
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
;; Objetivo : dado um nodo, retorna seu estado desenhado, junto do valor g, h e f (sendo este f + g)

;; Exemplos:
;; (desenha-info-nodo NODO-1) = desenha estado e valores do nodo 1
;; (desenha-info-nodo NODO-2) = desenha estado e valores do nodo 2
(define (desenha-info-nodo nodo)
  (above
   (desenha-estado (nodo-estado nodo))
   (text (string-append "valor-h:" (number->string (nodo-valor-h nodo))) 15 "black")
   (text (string-append "valor-g:" (number->string (nodo-valor-g nodo))) 15 "black")
   (text (string-append "valor-f:" (number->string (+ (nodo-valor-h nodo) (nodo-valor-g nodo)))) 15 "black")))

;; desenha-nodos-sucessores : ListaDeNodos -> Imagem
;; Objetivo : dado uma lista de nodos sucessores, retorna o primeiro nodo sucessor, com seus respectivos sucessores,
;; ao lado dos outros nodos sucessores, chamando de volta a função desenha-nodo

;; Exemplos:
;; (desenha-nodos-sucessores NODO-1) = desenha nodos sucessores do nodo 1
;; (desenha-nodos-sucessores NODO-2) = desenha nodos sucessores do nodo 2
(define (desenha-nodos-sucessores nodos-sucessores)
  (cond
    [(empty? nodos-sucessores) empty-image]
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
  (above
   (desenha-info-nodo nodo)
   (rectangle 30 30 "solid" "white")
   (desenha-nodos-sucessores (nodo-ls-nodos nodo))))

(desenha-nodo NODO-2)
(desenha-nodo NODO-2-EXPANDIDO)

;; =========================================
;; 44444444444444444444444444444444444444444
;; =========================================

(define (gera-arvore-sucessores nodos-sucessores numero-k)
  (cond
    [(empty? nodos-sucessores) empty]
    [else (gera-árvore (expande-nodo (first nodos-sucessores)) numero-k)]))

(define (gera-árvore nodo numero-k)
  (cond
    [(<= (nodo-valor-g nodo) numero-k)
     (make-nodo
      (nodo-estado nodo)
      (nodo-valor-h nodo)
      (nodo-valor-g nodo)
      (nodo-ls-nodos (gera-arvore-sucessores (nodo-ls-nodos (expande-nodo nodo)) numero-k)))]
    [else nodo]))

;; =========================================
;; 55555555555555555555555555555555555555555
;; =========================================

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


;; retorna-nodos-com-estado-lista : ListaDeNodos Estado -> ListaNodos
;; Objetivo : dada uma lista de nodos e um estado, retorna uma lista com nodos contendo o estado usando
;; retorna-nodos-com-estado recursivamente

;; Exemplos:
;; (retorna-nodos-com-estado-lista LISTA-NODOS-1 ESTADO-1) = (list (make-nodo (make-estado 0 1 2 3 4 5 6 7 8) 0 5 '()))
;; (retorna-nodos-com-estado-lista LISTA-NODOS-2 ESTADO-1) = '()
(define (retorna-nodos-com-estado-lista lista-nodos estado)
  (cond
    [(empty? lista-nodos) empty]
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
    [(estado-é-igual? (nodo-estado nodo) estado) (cons nodo (retorna-nodos-com-estado-lista (nodo-ls-nodos nodo) estado))]
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
;; Objetivo : dada uma lista de nodos um menor valor, percorre recursivamente a lista atualizando o menor valor
;; com o menor valor g dentre os nodos

;; Exemplos:
;; (retorna-menor-valor-g-nodos LISTA-NODOS-1 1) = 1
;; (retorna-menor-valor-g-nodos LISTA-NODOS-2 1) = 1
(define (retorna-menor-valor-g-nodos lista-nodos menor-valor)
  (cond
    [(empty? lista-nodos) menor-valor]
    [(< (nodo-valor-g (first lista-nodos)) menor-valor) (retorna-menor-valor-g-nodos (rest lista-nodos) (nodo-valor-g (first lista-nodos)))]
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
;; Objetivo : dado um nodo e um estado, retorna o menor valor g dentro os nodos que possuem o estado
;; ou uma mensagem string caso nenhum nodo tenha o estado

;; Exemplos:
;; (busca-estado NODO-2-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) = 1
;; (busca-estado NODO-2-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) = MENSAGEM-ESTADO-NAO-ENCONTRADO
(define (busca-estado nodo estado)
  (cond
    [(empty? (retorna-nodos-com-estado nodo estado)) MENSAGEM-ESTADO-NAO-ENCONTRADO]
    [else (retorna-menor-valor-g-nodos (retorna-nodos-com-estado nodo estado) (nodo-valor-g (first (retorna-nodos-com-estado nodo estado))))]))

;; Testes:

(check-expect (busca-estado NODO-2-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) 1)
(check-expect (busca-estado NODO-1-EXPANDIDO (make-estado 1 4 2 0 3 5 6 7 8)) MENSAGEM-ESTADO-NAO-ENCONTRADO)

;; =========================================
;; 66666666666666666666666666666666666666666
;; =========================================