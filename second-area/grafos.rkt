;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname grafos) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct nodo (nome vizinhos))
;; Um nodo é um par
;; (make-nodo n v), onde
;; n : String, representa o nome do nodo
;; v : ListaDeString, representa os nodos vizinhos

;; Um Grafo é
;; 1. empty, ou
;; 2. (cons n g), onde
;; n : Nodo
;; g : Grafo

(define GrafoCidades
  (list
   (make-nodo "Duluth" (list "Toronto"))
   (make-nodo "Toronto" (list "Chicago" "Duluth" "Pittsburgh"))
   (make-nodo "Chicago" (list "Toronto"))
   (make-nodo "Pittsburgh" (list "Toronto"))))

(define GrafoCidades2
  (list
   (make-nodo "Helena" (list "Winnipeg" "Deluth" "Omaha" "Denver"))
   (make-nodo "Winnipeg" (list "Helena" "Deluth" "Sault St Mary"))
   (make-nodo "Duluth" (list "Toronto" "Helena" "Winnipeg" "Sault St Mary" "Omaha" "Chicago"))
   (make-nodo "Toronto" (list "Sault St Mary" "Chicago" "Duluth" "Pittsburgh"))
   (make-nodo "Chicago" (list "Toronto" "Pittsburgh" "Duluth" "Omaha" "Saint Louis"))
   (make-nodo "Pittsburgh" (list "Toronto" "Chicago" "Saint Louis" "Nashville" "Radiator Springs"))
   (make-nodo "Omaha" (list "Helena" "Deluth" "Chicago" "Denver" "Kansas City"))
   (make-nodo "Kansas City" (list "Denver" "Omaha" "Saint Louis" "Oklahoma City"))))

;; vizinhos: String Grafo -> ListaDeString
;; Dados o nome de um nodo e um grafo, devolve os nomes de
;; todos os nodos vizinhos do nodo dado.
;; Obs.: O nodo dado deve fazer parte do grafo.

(define (vizinhos nome grafo nodos-ja-visitados)
  (cond
    [(empty? grafo) empty]
    [(and
      (string=? nome (nodo-nome (first grafo)))
      (not (está-na-lista? (nodo-nome (first grafo)) nodos-ja-visitados)))
     (nodo-vizinhos (first grafo))]
    [else (vizinhos nome (rest grafo) (appen (list (nodo-nome (first grafo))) nodos-ja-visitados))]))

(define (está-na-lista? nome lista)
  (cond
    [(empty? (filter (lambda (item) (string=? item nome)) lista)) false]
    [else true]))

(define (subtrai-lista lista1 lista2)
  (map (lambda (item-lista1) (cond [(not (está-na-lista? item-lista1 lista2)) item-lista1] [else ""])) lista1))

(subtrai-lista (list "Winnipeg" "Deluth" "Omaha" "Denver") (list "Helena" "Deluth" "Sault St Mary"))


(define (retorna-caminho nome-destino grafo lista-cidades)
  (cond
    [(not (está-na-lista? nome-destino lista-cidades)) (retorna-caminho nome-destino (rest grafo) (append (list (nodo-nome (first grafo))) lista-cidades))]
    [else lista-cidades]))

(define (testa-caminho caminho)
  (cond
    [(empty? caminho) #false]
    [else caminho]))

;; encontra-caminho: String String Grafo -> ListaDeStringOUFalse
;; Dados os nome das cidades origen e destino e um grafo (mapa), encontra um caminho entre a
;; origem e o destino. Se não existir caminho, devolve false.
;; Obs.: As cidades dadas devem fazer parte do grafo.
(define (encontra-caminhos origem destino grafo)
  (cond
    ;; Se a origem for igual ao destino, retornar a lista com o destino.
    [(string=? origem destino) grafo]
    ;; senão
    ;; definir um nome local que guarda um caminho entre um dos vizinhos do
    ;; nodo origem e o destino, se existir caminho (senão, false é armazenado):
    [else
     (local
       ((define caminho (testa-caminho (retorna-caminho destino grafo (list '())))))
       ;; e testar este nome local (caminho)
       ;; se o caminho for uma lista (foi encontrado um caminho), devolver este
       ;; caminho, colocando o nodo origem na frente
       (cond
         ;; se o nome for um booleano (não há caminho), devolver false
         [(boolean? caminho) #false]
         [else (append (list origem) caminho)]))]))




