(define-struct nó (id conteúdo esq dir))
;; Um elemento do conjunto AB (Árvore Binária) pode ser
;; 1. empty , representando a falta de informação, ou
;; 2. (make-nó id c e d)
;; onde:
;; id : Número, representa o identificador do nó
;; c : String, representa o conteúdo do nó
;; e : AB, representa a sub-árvore da esquerda
;; d : AB, representa a sub-árvore da direita

(define-struct par (id cont))
;; Um elemento do conjunto Par tem o formato
;; (make-par id c), onde:
;; id : Número, representa o id
;; c : String, representa um conteúdo

;; Uma ListaPar é
;; 1. empty, ou
;; 2. (cons p lp), onde p: Par e lp: ListaPar

;; constróiABP: ListaPar -> ABP
;; Dada uma lista de pares de identificador e conteúdo, gera uma ABP com estes pares..
;; Exemplo:
;; (constróiABP empty) = empty
;; (constrói ABP L1) =
(define L1 (list (make-par 11 “F”) (make-par 20 “E”) (make-pr 3 “B”)
(make-par 15 “D”) (make-par 17 “B”) (make-par 10 “A”)))

(define (constróiABP lp)
    (cond
        [(empty? lp) empty]
        [else
            (insere-nó ;; inserir um nó com
            (nó-id (first lp)) ;; o identificador do primeiro nó da lista
            (nó-conteudo (first lp)) ;; o conteúdo do primeiro nó da lista
            (constróiABP(rest lp)))])) ;; na árvore com o resto dos pares da lista

;; insere-nó: Número String ABP -> ABP
;; Dados um identificador, um conteúdo e uma ABP, insere um nó com este identificador
;; e conteúdo na ABP. Assume que o identificador ainda não está na ABP.

(define (insere-nó id cont abp)
    (cond
        [(empty? abp) (make-nó id cont empty empty)]
        [(> id (nó-id abp))
            (make-nó ;; ABP construída inserindo o nó com id, cont na subárvore da direita de abp
                (nó-id abp) ;; id da raiz da ABP
                (nó-conteudo abp) ;; conteúdo da raiz da ABP
                (nó-esq abp) ;; subárvore da esquerda da ABP
                (insere-nó id cont (nó-dir abp)))] ;; nova subárvore da direita da ABP
        [(< id (nó-id abp))
            (make-nó ;; ABP construída inserindo o nó com id, cont na subárvore da esquerda de abp
            (nó-id abp) ;; id da raiz da ABP
            (nó-conteudo abp) ;; conteúdo da raiz da ABP
            (insere-nó id cont (nó-esq abp)) ;; nova subárvore da esquerda da ABP
            (nó-dir abp))])) ;; subárvore da direita da ABP

(define (busca-conteudo-no id abp)
    (cond
      [(empty? abp) empty]
      [(= (nó-id abp) id) (nó-conteudo abp)]
      [(> id (nó-id abp)) (busca-conteudo-no id (nó-dir abp))]
      [else (busca-conteudo-no id (nó-esq abp))]))

