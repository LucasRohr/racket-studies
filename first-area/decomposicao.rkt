;; ============ DEFINIÇÕES DE DADOS ==============
;; -----------------
;; TIPO ALUNO:
;; -----------------
(define-struct aluno (nome prova1 prova2 exerc))
;; Um elemento de Aluno é
;;    (make-aluno n p1 p2 e), onde
;;    n : String, é o nome do aluno
;;    p1: Número, é a nota da prova 1 do aluno
;;    p2: Número, é a nota da prova 2 do aluno
;;    e : Número, é a nota dos exercícios do aluno

;; Exemplos de alunos:


;; -----------------
;; TIPO LISTAALUNOS:
;; -----------------
;; Uma ListaAlunos é
;; 1. empty, ou
;; 2. (cons a la), onde
;;     a : Aluno
;;     la: ListaAlunos
;; -----------------
;; TIPO LISTANÚMEROS:
;; -----------------
;; Uma ListaNúmeros é
;; 1. empty, ou
;; 2. (cons n ln), onde
;;     n : Número
;;     ln: ListaNúmeros

;; Exemplos de listas de alunos:

(define LISTA-ALUNOS1 (list (make-aluno "jao" 8 6 9) (make-aluno "jao" 2 4 1) (make-aluno "jao" 8 10 10) (make-aluno "jao" 6 6 6)))


;; ================================================
;;                 EXERCÍCIO 1
;; ================================================
;; ============ DEFINIÇÕES DE FUNÇÕES ==============

;; A seguir funções úteis para construir a solução:
;; =================================================
;;           FUNÇÕES DE CÁLCULO DAS NOTAS
;; =================================================

;; calculaNotas: ... -> ...
;; obj:  Dada a lista de alunos, devolver a lista de notas finais dos alunos

;; calculaNotaFinal: ... -> ...
;; obj:Dado um aluno, calcular sua nota final

;; calculaMedia: ... -> ...
;; Dada a lista de notas finais dos alunos, calcular a média da turma

;; somaNotas: ... -> ...
;; Devolve a soma dos números de uma lista de números

;; contaNotas: ... -> ...
;; Devolve o tamanho de uma lista de números

;; =================================================
;;               FUNÇÕES DE DESENHO 
;; =================================================

;; desenha-circulo: ... -> ...
;; Dado um número, desenhar este número dentro de um círculo verde se ele for
;; maior ou igual a 6, e dentro de um vermelho, caso contrário.

;; desenha-circulo-preto: ... -> ...
;; Desenha um círculo preto com o número dado dentro

;; mostraNotas: ... -> ...
;; Dada a lista de notas finais dos alunos e a média da turma, desenhar a imagem
;; com os círculos correspondentes

;; número->string : Número -> String
;; Dado um número, transforma este número em um string usando um formato com até 2 casas decimais 
;; Exemplos e testes:
        (check-expect (número->string 7.689)  "7.69")
        (check-expect(número->string 10/3)  "3.33")
        (check-expect (número->string 7)  "7.0")
(define (número->string @n)
  (number->string (exact->inexact (/ (round (* @n 100)) 100))))


(define (desenha-nota-circulo nota cor)
  (overlay
   (text (número->string nota) 14 "white")
   (circle 25 "solid" cor)))

(define (calcula-nota-aluno aluno)
  (/ (+ (* (aluno-prova1 aluno) 4) (* (aluno-prova2 aluno) 5) (aluno-exerc aluno)) 10))

(define (retorna-soma-notas-turma lista-alunos)
  (cond
    [(empty? lista-alunos) 0]
    [else (+ (calcula-nota-aluno (first lista-alunos)) (retorna-soma-notas-turma (rest lista-alunos)))]))

(define (retorna-total-alunos lista-alunos)
  (cond
    [(empty? lista-alunos) 0]
    [else (+ 1 (retorna-total-alunos (rest lista-alunos)))]))

(define (calcula-media-turma lista-alunos)
  (/ (retorna-soma-notas-turma lista-alunos) (retorna-total-alunos lista-alunos)))


(calcula-media-turma LISTA-ALUNOS1)

(define (mostra-media-turma lista-alunos)
  (desenha-nota-circulo (calcula-media-turma lista-alunos) "black"))

(define (mostra-media-aluno aluno)
  (desenha-nota-circulo
   (calcula-nota-aluno aluno)
   (cond
     [(>= (calcula-nota-aluno aluno) 6) "darkgreen"]
     [else "red"])))

;; mostraNotasTurma: ListaAlunos  -> Imagem
;; obj: Dada uma lista de alunos de uma turma, monta uma imagem com as notas finais
;; dos alunos dentro de círculos (verdes se a nota final for maior ou igual a 6
;; e vermelhos, caso contrário), colocando a média aritmética da turma dentro de um
;; círculo preto no final. A nota final de um aluno é uma média ponderada:
;; a prova 1 tem peso 4, a prova 2 tem peso 5 e os exercícios tem peso 1.
;; Exemplos:
;; (mostraNotasTurma ...) = 
;; (mostraNotasTurma ...) = 

(define (mostraNotasAlunos lista-alunos)
  (cond
    [(empty? lista-alunos) empty-image]
    [else (beside (mostra-media-aluno (first lista-alunos)) (mostraNotasAlunos (rest lista-alunos)))]))

(define (mostraNotasTurma lista-alunos)
  (beside (mostraNotasAlunos lista-alunos) (mostra-media-turma lista-alunos)))

;; Testes:
;; (check-expect (mostraNotasTurma ...) ...)
;; (check-expect (mostraNotasTurma ...) ...)

(mostraNotasTurma LISTA-ALUNOS1)

;; Observação: para os exemplos dos slides foram usados círculos com raio 25, nas cores
;; "darkgreen", "red" e "black", e texto na cor "white". Foi usada a função
;; número->string (descrição abaixo) para arredondar o resultado da média (deixando-a
;; com até 2 casas decimais) e transformá-lo em uma string.

;; ================================================
;;                 EXERCÍCIO 2
;; ================================================

;; ============ DEFINIÇÕES DE FUNÇÕES ==============
;; ordena: ListaNúmeros -> ListaNúmeros
;; Ordena uma lista de números em ordem crescente
;; Exemplos e testes:
;;        (check-expect (ordena empty) empty)
;;        (check-expect (ordena (list 5 2 10 3.5 8.6 4 9)) (list 2 3.5 4 5 8.6  9 10))
(define (ordena @L)
       (cond
           ;; se a lista @L estiver vazia, devolve a lista vazia
           [(empty? @L)  ...]
           ;; senão, insere  
           [else (...
                      (... @L)            ;; o primeiro elemento da lista @L
                      (... (... @L)))]));; na lista ordenada contendo os elementos do resto de @L

;; insere: Número ListaNúmeros -> ListaNúmeros
;; Dados um número e uma lista de números ordenada em ordem crescente,
;; coloca este número na lista na posição correta
;; Exemplos e testes:
;;        (check-expect (insere  2 empty) (list 2))
;;        (check-expect (insere  4 (list 2 3.5  5 8.6  9 10)) (list 2 3.5 4 5 8.6  9 10))
(define (insere @N @L)
       (cond
           ;; se a lista @L estiver vazia, devolve a lista contendo apenas @N
           [(empty? @L) ...]
           ;; se @N for menor ou igual ao primeiro elemento de @L, coloca @N no início de @L
           [(<= @N (first @L)) ...]
           ;; se @N for maior que o primeiro elemento de @L,
           [(>  @N (first @L))
                 (cons ;; monta uma lista com 
                       (... @L)               ;; o primeiro elemento de @L e
                       (... @N (... @L)))]));; a lista obtida inserindo @N na posição correta no resto de @L


;; mostraNotasOrdenadasTurma: ...  -> ...
;; obj: Dada uma lista de alunos de uma turma, monta uma imagem com as notas finais
;; dos alunos dentro de círculos (verdes se a nota final for maior ou igual a 6
;; e vermelhos, caso contrário), colocando a média aritmética da turma dentro de um
;; círculo preto no final. A nota final de um aluno é uma média ponderada:
;; a prova 1 tem peso 4, a prova 2 tem peso 5 e os exercícios tem peso 1.
;; A lista de notas finais deve ser mostrada em ordem crescente de nota.
