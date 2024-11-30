;; A u t o r e s : MARTINEZ RIVERA WILSON ANDRES 2266319 . JUAN DAVID CAMACHO CATAÑO 2266292 , PAREDES CHAVES JUAN GABRIEL 2266183
#lang racket

(require rackunit)

;; Error handling helper
(define (eopl:error msg val)
  (error (string-append msg ": ") val))

;; Representación de expresiones como estructuras
(struct num-exp (n) #:transparent)
(struct list-exp (head tail) #:transparent)
(struct list-empty-exp () #:transparent)
(struct length-exp (list-exp) #:transparent)
(struct first-exp (list-exp) #:transparent)
(struct rest-exp (list-exp) #:transparent)
(struct nth-exp (list-exp index) #:transparent)
(struct cond-exp (clauses else) #:transparent)

;; Evaluador
(define (evaluar-expresion expr amb)
  (match expr
    ;; Expresión numérica
    [(num-exp n) n]

    ;; Listas
    [(list-exp head tail)
     (let ([head-val (evaluar-expresion head amb)]
           [tail-val (evaluar-expresion tail amb)])
       (if (list? tail-val)
           (cons head-val tail-val)
           (eopl:error "El segundo argumento de cons debe ser una lista" tail-val)))]

    [(list-empty-exp) '()]

    [(length-exp list-exp)
     (let ([list-val (evaluar-expresion list-exp amb)])
       (if (list? list-val)
           (length list-val)
           (eopl:error "Argumento para length no es una lista" list-val)))]

    [(first-exp list-exp)
     (let ([list-val (evaluar-expresion list-exp amb)])
       (if (and (list? list-val) (not (null? list-val)))
           (car list-val)
           (eopl:error "No se puede obtener el primer elemento de una lista vacía o no lista" list-val)))]

    [(rest-exp list-exp)
     (let ([list-val (evaluar-expresion list-exp amb)])
       (if (and (list? list-val) (not (null? list-val)))
           (cdr list-val)
           (eopl:error "No se puede obtener el resto de una lista vacía o no lista" list-val)))]

    [(nth-exp list-exp index-exp)
     (let ([list-val (evaluar-expresion list-exp amb)]
           [index-val (evaluar-expresion index-exp amb)])
       (if (and (list? list-val) (integer? index-val) (>= index-val 0))
           (if (< index-val (length list-val))
               (list-ref list-val index-val)
               (eopl:error "Índice fuera de rango en nth"))
           (eopl:error "Argumentos inválidos para nth: lista y entero requeridos" list-val)))]

    ;; Condicional cond
    [(cond-exp clauses else)
     (define (evaluate-clause clause)
       (match clause
         [(list pred exp)
          (if (evaluar-expresion pred amb)  ;; Si la condición es verdadera
              (evaluar-expresion exp amb)
              #f)]))  ;; Si no, evaluamos el siguiente caso

     (define (loop clauses)
       (match clauses
         [(list) (evaluar-expresion else amb)]  ;; Si no hay más cláusulas, ejecutamos el else
         [(cons clause rest)
          (let ([result (evaluate-clause clause)])
            (if result
                result
                (loop rest)))]))
     (loop clauses)]  ;; Llamamos al bucle para evaluar las cláusulas

    ;; Error por defecto
    [_ (eopl:error "Expresión desconocida" expr)]))

;; Función de prueba para evaluar
(define (test-evaluar exp case-name)
  (displayln (string-append "Caso: " case-name))
  (displayln (string-append "Resultado: " (format "~a" (evaluar-expresion exp '())))))

;; Ejemplos de pruebas de listas

(displayln "Pruebas de listas:")

;; Caso 1: Lista vacía
(define caso-1 (list-empty-exp))
(test-evaluar caso-1 "Caso 1: Lista vacía")

;; Caso 2: Lista con un único elemento
(define caso-2 (list-exp (num-exp 42) (list-empty-exp)))
(test-evaluar caso-2 "Caso 2: Lista con un único elemento")

;; Caso 3: Lista con múltiples elementos
(define caso-3
  (list-exp (num-exp 1)
            (list-exp (num-exp 2)
                      (list-exp (num-exp 3) (list-empty-exp)))))
(test-evaluar caso-3 "Caso 3: Lista con múltiples elementos")

;; Caso 4: Lista anidada diferente
(define caso-4
  (list-exp
   (list-exp (num-exp 10) (list-exp (num-exp 20) (list-empty-exp)))  ;; Lista interna
   (list-exp (num-exp 30) (list-empty-exp))))  ;; Lista externa
(test-evaluar caso-4 "Caso 4: Lista anidada diferente")

;; Caso 5: Operación sobre listas - `nth`
(define caso-5
  (nth-exp
   (list-exp (num-exp 10)
             (list-exp (num-exp 20)
                       (list-exp (num-exp 30) (list-empty-exp))))
   (num-exp 1)))
(test-evaluar caso-5 "Caso 5: Operación sobre listas - `nth`")

(displayln "\nPruebas con cond:")

;; Ejemplos de pruebas con cond

;; Caso 6: Cond con expresión anidada
(define cond-test-1
  (cond-exp
   (list (list (num-exp 0) (num-exp 50)))
   (num-exp 42)))  ;; Siempre se toma el else, por lo que se retorna 42
(test-evaluar cond-test-1 "Caso 1: Cond con expresión anidada")

;; Condicional con solo else
(define cond-test-2
  (cond-exp
   (list)
   (num-exp 99)))  ;; Siempre se toma el else
(test-evaluar cond-test-2 "Caso 2: Condicional solo con else")

;; Condicional con una cláusula verdadera
(define cond-test-3
  (cond-exp
   (list (list (num-exp 1) (num-exp 42)))
   (num-exp 99)))  ;; Si 1 es verdadero, se retorna 42
(test-evaluar cond-test-3 "Caso 3: Condicional con una cláusula verdadera")

;; Condicional con una cláusula falsa
(define cond-test-4
  (cond-exp
   (list (list (num-exp 0) (num-exp 99)))
   (num-exp 88)))  ;; Si 0 es falso, se toma el else (88)
(test-evaluar cond-test-4 "Caso 4: Condicional con una cláusula falsa")

;; Condicional con múltiples condiciones
(define cond-test-5
  (cond-exp
   (list (list (num-exp 0) (num-exp 50))
         (list (num-exp 2) (num-exp 100)))
   (num-exp 100)))  ;; Se toma la segunda cláusula porque 2 es verdadero
(test-evaluar cond-test-5 "Caso 5: Condicional con múltiples condiciones")
