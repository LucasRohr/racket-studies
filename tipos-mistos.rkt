;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tipos-mistos) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercicios sobre Tipos Mistos


;; === VencimentoPerecivel ===

(define-struct VencimentoPerecivel (mes proximo-mes seguintes))

;; Um elemento do conjunto VencimentoPerecivel tem o formato
;; (make-vencimento-perecivel 5 9 10)

;; onde:
;; mes: Numero, representa produtos que vencem no mês atual
;; proximo-mes: Numero, representa produtos que vencem no próximo mês
;; seguintes: Numero, representa produtos que vencem nos meses seguintes


;; === QuantidadeVencimento ===

;; Um elemento do conjunto QuantidadeVencimento é:
;; i) um Número ou
;; ii) um VencimentoPerecivel


;; === Produto ===

(define-struct Produto (nome codigo preco quantidade-vencimento))

;; Um elemento do conjunto Produto tem o formato
;; (make-produto nome codigo preco quantidade-vencimento)

;; onde:
;; nome: String, representa o nome do produto
;; codigo: Número, representa o código do produto
;; preco: Número, representa o preço do produto
;; quantidade-vencimento: QuantidadeVencimento, representa a quantidade do produto, classificada em termos do prazo de validade, no caso de perecíveis


;; 1. Definir constantes com os elementos dos conjuntos VencPerecível, QuantVenc e Produto.

(define VENCIMENTO-AZEITE (make-VencimentoPerecivel 50 330 500))
(define VENCIMENTO-LEITE (make-VencimentoPerecivel 302 56 40))

(define CADERNO (make-Produto "Caderno" 142 4.50 105))
(define AZEITE (make-Produto "Azeite" 34 5.25 VENCIMENTO-AZEITE))
(define LEITE (make-Produto "Leite" 10 2.50 VENCIMENTO-LEITE))

;; 2. Construa as funções a seguir:

;; a) Dado um produto, devolve seu preço.

;;retorna-nome-produto Produto -> string
;; Objetivo: dado uma estrutura Produto, devolve seu nome string

;; Exemplo:
;; (retorna-nome-produto
;;   (make-Produto "Caderno" 142 4.50 105)
;; ) = "Caderno"

(define (retorna-nome-produto produto)
  (Produto-nome produto)
)

;; Testes:

(check-expect (retorna-nome-produto CADERNO) "Caderno")


;; b) Dado um elemento de QuantidadeVencimento, devolve a quantidade total de produtos.

;;retorna-quantidade-total QuantidadeVencimento -> Numero
;; Objetivo: dado uma estrutura QuantidadeVencimento, sendo VencimentoPerecivel ou Numero, devolve o total da quantidade

;; Exemplo:
;; (retorna-quantidade-total (make-VencimentoPerecivel 100 50 50)) = 200
;; (retorna-quantidade-total 42) = 42

(define (retorna-quantidade-total quantidade)
  (cond
      [(number? quantidade) quantidade]
      [else (+ (VencimentoPerecivel-mes quantidade) (VencimentoPerecivel-proximo-mes quantidade) (VencimentoPerecivel-seguintes quantidade))]
   )
)

;; Testes:

(check-expect (retorna-quantidade-total (make-VencimentoPerecivel 100 50 50)) 200)
(check-expect (retorna-quantidade-total 42) 42)


;; c) Dado um produto, devolve o valor do estoque deste produto.

;; retorna-valor-estoque Produto -> Numero
;; Objetivo: dado uma estrutura Produto, devolve o calculo do valor de estoque com base no preço e se o produto é perecível ou não

;; Exemplo:
;; (retorna-valor-estoque (make-Produto "Azeite" 34 5.25 (make-VencimentoPerecivel 50 330 500))) = 4620
;; (retorna-valor-estoque (make-Produto "Caderno" 142 4.50 105)) = 472.5

(define (retorna-valor-estoque produto)
  (*
     (retorna-quantidade-total (Produto-quantidade-vencimento produto))
     (Produto-preco produto)
  )
)

;; Testes:

(check-expect (retorna-valor-estoque AZEITE) 4620)
(check-expect (retorna-valor-estoque CADERNO) 472.5)


;; d) Dado um produto, baixa o preço em 30% se o produto for perecível e o estoque deste produto for maior que 30. O registro fica inalterado caso contrário.

;; calcula-desconto-30 Produto -> Produto
;; Objetivo: dado uma estrutura Produto, devolve o prodito com desconto ou não no preço caso ele for perecível ou com estoque acima de 30

;; Exemplo:
;; (calcula-desconto-30 (make-Produto "Azeite" 34 5.25 (make-VencimentoPerecivel 50 330 500))) = (make-Produto "Azeite" 34 3.675 (make-VencimentoPerecivel 50 330 500))
;; (calcula-desconto-30 (make-Produto "Caderno" 142 4.50 105)) = (make-Produto "Caderno" 142 4.50 105)

(define (calcula-desconto-30 produto)
   (cond
      [
       (and (VencimentoPerecivel? (Produto-quantidade-vencimento produto)) (> (retorna-quantidade-total (Produto-quantidade-vencimento produto)) 30))
       (make-Produto
          (Produto-nome produto)
          (Produto-codigo produto)
          (* (Produto-preco produto) 0.7)
          (Produto-quantidade-vencimento produto)
       )
      ]
      [else produto]
   )
)

;; Testes:

(check-expect (calcula-desconto-30 (make-Produto "Caderno" 142 4.50 105)) (make-Produto "Caderno" 142 4.50 105))
(check-expect (calcula-desconto-30 (make-Produto "Azeite" 34 5.25 (make-VencimentoPerecivel 50 330 500))) (make-Produto "Azeite" 34 3.675 (make-VencimentoPerecivel 50 330 500)))


;; e) Dado um produto, se ele for perecível, zera o estoque de produtos que vencem no mês.

;; zera-estoque-produto Produto -> Produto
;; Objetivo: dado uma estrutura Produto, zera o estoque de produtos perecíveis do mês caso ele for perecível

;; Exemplo:
;; (zera-estoque-produto (make-Produto "Azeite" 34 5.25 (make-VencimentoPerecivel 50 330 500))) = (make-Produto "Azeite" 34 3.675 (make-VencimentoPerecivel 0 330 500))
;; (zera-estoque-produto (make-Produto "Caderno" 142 4.50 105)) = (make-Produto "Caderno" 142 4.50 105)

(define (zera-estoque-produto produto)
   (cond
      [
       (VencimentoPerecivel? (Produto-quantidade-vencimento produto))
       (make-Produto
          (Produto-nome produto)
          (Produto-codigo produto)
          (Produto-preco produto)
          (make-VencimentoPerecivel
            0
            (VencimentoPerecivel-proximo-mes (Produto-quantidade-vencimento produto))
            (VencimentoPerecivel-seguintes (Produto-quantidade-vencimento produto))
          )
       )
      ]
      [else produto]
   )
)

;; Testes:

(check-expect (zera-estoque-produto (make-Produto "Caderno" 142 4.50 105)) (make-Produto "Caderno" 142 4.50 105))
(check-expect
   (zera-estoque-produto (make-Produto "Azeite" 34 5.25 (make-VencimentoPerecivel 50 330 500)))
   (make-Produto "Azeite" 34 5.25 (make-VencimentoPerecivel 0 330 500))
)


;; f) Dados dois produtos, se os dois forem perecíveis, devolve o numero de ítens que vencem neste mês de cada
;; produto, caso contrário, devolve a mensagem “Erro: produto não perecível”.

;; === VencimentosNoMes ===

(define-struct VencimentosNoMes (quantidade-produto-1 quantidade-produto-2))

;; Um elemento do conjunto VencimentosNoMes tem o formato
;; (make-VencimentosNoMes 5 9)

;; onde:
;; quantidade-produto-1: Numero, representa produtos que vencem no mês atual para o produto 1
;; quantidade-produto-2: Numero, representa produtos que vencem no mês atual para o produto 2

;; === RetornoVencimentosNoMes ===

;; Um elemento do conjunto RetornoVencimentosNoMes é:
;; i) uma String ou
;; ii) um VencimentosNoMes

;; ======

;; retorna-vencimentos-neste-mes Produto, Produto -> RetornoVencimentosNoMes
;; Objetivo: dado duas estruturas Produto, retorna uma estrutura VencimentosNoMes se ambos forem perecíveis ou uma mensagem String se não forem

;; Exemplos:

;; (retorna-vencimentos-neste-mes (make-Produto "Azeite" 34 5.25 (make-VencimentoPerecivel 50 330 500)) (make-Produto "Leite" 10 2.50 (make-VencimentoPerecivel 302 56 40)))
;; = (make-VencimentosNoMes 50 302)

;; (retorna-vencimentos-neste-mes (make-Produto "Caderno" 142 4.50 105) (make-Produto "Caderno" 142 4.50 105)) = (text "Erro: produto não perecível" 28 "white")

(define (retorna-vencimentos-neste-mes produto-1 produto-2)
   (cond
     [
       (and (VencimentoPerecivel? (Produto-quantidade-vencimento produto-1)) (VencimentoPerecivel? (Produto-quantidade-vencimento produto-2)))
       (make-VencimentosNoMes (VencimentoPerecivel-mes (Produto-quantidade-vencimento produto-1)) (VencimentoPerecivel-mes (Produto-quantidade-vencimento produto-2)))
     ]
     [else (text "Erro: produto não perecível" 22 "white")]
   )
)

;; Testes:

(check-expect (retorna-vencimentos-neste-mes CADERNO CADERNO) (text "Erro: produto não perecível" 22 "white"))
(check-expect (retorna-vencimentos-neste-mes AZEITE LEITE) (make-VencimentosNoMes 50 302))

(retorna-vencimentos-neste-mes CADERNO CADERNO)
(retorna-vencimentos-neste-mes AZEITE LEITE)