#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proyecto Final
;; Fundamentos de Lenguajes de Programación
;; MathFlow
;;
;; Integrantes:
;; Esteban Samuel Córdoba
;; Helkin Gabriel
;; Brigitte Vanessa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;funcion para probar el intérprete, se puede usar para evaluar cualquier programa del lenguaje.
;(provide scan&parse evaluar-programa valor-verdad?)
; =========================================
; ESPECIFICACIÓN LÉXICA
; =========================================
; Define:
; - números enteros y decimales
; - textos
; - identificadores
; - espacios y comentarios

; Tabla lexica que define los tokens existentes en el lenguaje.
; Entrada: texto fuente del programa.
; Salida: secuencia de tokens validos (numero, texto, identificador, etc.).
(define especificacion-lexica

  '(
    
    (white-space
      (whitespace)
      skip)

    (comment
      ("%" (arbno (not #\newline)))
      skip)

    (identificador
      ("@" letter (arbno (or letter digit "_" "?")))
      symbol)

    (numero
      (digit (arbno digit))
      number)

    (numero
      ("-" digit (arbno digit))
      number)

    (numero
      (digit (arbno digit) "." digit (arbno digit))
      number)

    (numero
      ("-" digit (arbno digit) "." digit (arbno digit))
      number)

    (texto
      ("\"" (arbno (not #\")) "\"")
      string)



   
    
))


; =========================================
; GRAMÁTICA
; =========================================
; Define la sintaxis del lenguaje.
; Incluye:
; - números
; - textos
; - variables
; - primitivas binarias y unarias


; Gramatica principal del lenguaje.
; Define la forma de expresiones, procedimientos, letrec, declarar y primitivas.
; Sirve para que el parser convierta texto en AST.
(define gramatica

  '(
    
    (programa
      (expresion)
      un-programa)

    (expresion
      (numero)
      numero-lit)

    (expresion
      (texto)
      texto-lit)

    (expresion
      (identificador)
      var-exp)    
    (expresion
      ("(" expresion primitiva-binaria expresion ")")
      primapp-bin-exp)
    (expresion
     (primitiva-unaria "(" expresion ")")
     primapp-un-exp)
    (expresion
     ("procedimiento"
      "(" (separated-list identificador ",") ")"
      "{" expresion "}")
     procedimiento-exp)
    (expresion
     ("evaluar"
      expresion
      "(" (separated-list expresion ",") ")"
      "finEval")
     app-exp)
    
    (expresion
      ("Si" expresion
        "{"
        expresion
        "}"
        "sino"
        "{"
        expresion
        "}")
        condicional-exp)

    (expresion
      ("declarar"
        "("
      (arbno identificador "=" expresion ";")
        ")"
        "{"
        expresion
        "}")
        variableLocal-exp)

    
    (expresion
     ("letrec"
      identificador
      "(" (separated-list identificador ",") ")"
       "{"
      expresion
      "}"
      "en"
      expresion)
     letrec-exp)

    

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multiplicacion)
    (primitiva-binaria ("/") primitiva-division)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion)
))

; =========================================
; GENERACIÓN AUTOMÁTICA DE DATATYPES
; =========================================
; Construye automáticamente:
; - scanner
; - parser
; - define-datatype

(sllgen:make-define-datatypes
  especificacion-lexica
  gramatica)

; Funcion auxiliar para parsear un programa en formato string.
; Recibe: un string con codigo fuente.
; Retorna: el arbol AST del programa.
(define scan&parse

  (sllgen:make-string-parser
    especificacion-lexica
    gramatica))

; =========================================
;DATATYPE: ambiente
;=========================================
;PROPÓSITO: Definir la estructura recursiva para los ambientes léxicos.
;VARIANTES:
;- (vacio): Representa un ambiente sinvariables ligadas.
;- (extendido ids vals old-env):Añade una lista de simbolos (ids)y sus
;correspondientes valores (vals) sobre un ambiente previo (old-env).

; Predicado general de valores.
; Recibe: cualquier valor.
; Retorna: #t porque en este interprete permito cualquier valor de Scheme.
(define scheme-value?
  (lambda (v) #t))


(define-datatype ambiente ambiente?

  (vacio)

  (extendido
    (ids (list-of symbol?))
    (vals (list-of scheme-value?))
    (old-env ambiente?))

  (extendido-rec
    (proc-id symbol?)
    (params (list-of symbol?))
    (proc-cuerpo expresion?)
    (old-env ambiente?)))


; =========================================
; DATATYPE: procVal
; =========================================
; Representa procedimientos (closures).
;
; Una cerradura almacena:
; - parámetros
; - cuerpo
; - ambiente léxico
; =========================================

(define-datatype procVal procVal?
  (cerradura
   (lista-ID
    (list-of symbol?))
   (cuerpo
    expresion?)
   (amb
    ambiente?)))



; Ambiente base para ejecutar pruebas rapidas.
; Incluye algunas variables globales de ejemplo (@a, @b, ...).
; Retorna: una estructura ambiente de tipo extendido.
(define ambiente-inicial

  (extendido
    '(@a @b @c @d @e)
    '(1 2 3 "hola" "FLP")
    (vacio)))

;; Busca la posición de un identificador
;; dentro de una lista de símbolos.

; Esta funcion busca la posicion de un simbolo en una lista de simbolos.
; Recibe: id (simbolo) y lista (lista de simbolos).
; Retorna: el indice si lo encuentra, o #f si no existe.
(define buscar-posicion

  (lambda (id lista)

    (let loop ((lst lista)
               (pos 0))

      (cond

        [(null? lst)
         #f]

        [(eqv? id (car lst))
         pos]

        [else
         (loop (cdr lst)
               (+ pos 1))]))))


; =========================================
;FUNCIÓN: buscar-variable
;=========================================
;PROPÓSITO:Buscar el valor asociado a un identificador (@id) en un ambiente dado

;ARGUMENTOS:
;- id: El símbolo que representa la variable a buscar
;- env: El ambiente actual en donde se realiza la búsqueda.

;RETORNA: El valor de la variable si existe, o produce un error si llega a (vacio).

; Lookup de variables en el ambiente lexico.
; Recibe: id (simbolo) y env (ambiente actual).
; Retorna: el valor asociado al id o lanza error si no existe.
(define buscar-variable

  (lambda (id env)

    (cases ambiente env

      (vacio ()

        (eopl:error
          'buscar-variable
          "Error, variable no existe ~s"
          id))

      (extendido (ids vals old-env)

        (let ((posicion
                (buscar-posicion id ids)))

          (if posicion
              (list-ref vals posicion)
              (buscar-variable id old-env))))

      (extendido-rec (proc-id params proc-cuerpo old-env)
        (if (eqv? id proc-id)
            (cerradura params proc-cuerpo env)
            (buscar-variable id old-env))))))

;; Evalúa primitivas binarias como:
;; +, ~, *, /, concat, etc.
; =========================================
; FUNCIÓN: evaluar-primitiva-binaria
; =========================================
; PROPÓSITO: Evaluar operaciones primitivas binarias sobre dos valores dados.
;
; ARGUMENTOS:
; - prim: La estructura de la primitiva binaria a evaluar (ej. +, ~, *, /, concat).
; - val1: El resultado evaluado del primer operando.
; - val2: El resultado evaluado del segundo operando.
;
; RETORNA: El resultado expresado de la operación matemática, relacional o de texto.
; =========================================

; Evaluador de primitivas binarias (+, ~, *, /, concat, comparadores, etc.).
; Recibe: prim (tipo de operacion), val1 y val2 (operandos ya evaluados).
; Retorna: el resultado de aplicar la primitiva.
(define evaluar-primitiva-binaria

  (lambda (prim val1 val2)

    (cases primitiva-binaria prim

      (primitiva-suma ()
        (+ val1 val2))

      (primitiva-resta ()
        (- val1 val2))

      (primitiva-multiplicacion ()
        (* val1 val2))

      (primitiva-division ()
        (/ val1 val2))

      (primitiva-concat ()
        (let ((s1 (cond ((string? val1) val1)
                        ((number? val1) (number->string val1))
                        (else (eopl:error 'concat "concat: expected string or number ~s" val1))))
              (s2 (cond ((string? val2) val2)
                        ((number? val2) (number->string val2))
                        (else (eopl:error 'concat "concat: expected string or number ~s" val2)))))
          (string-append s1 s2)))

      (primitiva-mayor ()
        (if (> val1 val2) 1 0))

      (primitiva-menor ()
        (if (< val1 val2) 1 0))

      (primitiva-mayor-igual ()
        (if (>= val1 val2) 1 0))

      (primitiva-menor-igual ()
        (if (<= val1 val2) 1 0))

      (primitiva-diferente ()
        (if (not (equal? val1 val2)) 1 0))

      (primitiva-comparador-igual ()
        (if (equal? val1 val2) 1 0))

      (else
        (eopl:error
          'primitiva-binaria
          "No implementada")))))

;; Evalúa primitivas unarias como:
;; add1, sub1, longitud y neg.

; =========================================
; FUNCIÓN: evaluar-primitiva-unaria
; =========================================
; PROPÓSITO: Evaluar operaciones primitivas unarias sobre un único valor.
;
; ARGUMENTOS:
; - prim: La estructura de la primitiva unaria a evaluar (ej. add1, sub1, longitud, neg).
; - val: El resultado evaluado del operando.
;
; RETORNA: El resultado expresado de aplicar la transformación unaria al valor.
; =========================================
; Evaluador de primitivas unarias (add1, sub1, longitud, neg).
; Recibe: prim (operacion) y val (operando evaluado).
; Retorna: el resultado de la operacion unaria.
(define evaluar-primitiva-unaria

  (lambda (prim val)

    (cases primitiva-unaria prim

      (primitiva-add1 ()
        (+ val 1))

      (primitiva-sub1 ()
        (- val 1))

      (primitiva-longitud ()
        (string-length val))
      
      (primitiva-negacion ()
        (if (zero? val) 1 0))
      (else
        (eopl:error
          'primitiva-unaria
          "No implementada")))))

; =========================================
; FUNCIÓN: aplicar-procedimiento
; =========================================
; PROPÓSITO: Ejecutar una cerradura (closure) ligando sus parámetros formales 
; a los argumentos pasados en la invocación.
;
; ARGUMENTOS:
; - proc: El valor de tipo cerradura (procVal) que contiene los identificadores, 
;         el cuerpo y el ambiente léxico guardado.
; - argumentos: La lista de valores ya evaluados que se pasarán a la función.
;
; RETORNA: El valor resultante de evaluar el cuerpo del procedimiento en 
; el nuevo ambiente extendido.
; =========================================

; Aplica una cerradura a una lista de argumentos.
; Recibe: proc (cerradura) y argumentos (lista de valores).
; Retorna: el resultado de evaluar el cuerpo del procedimiento en su nuevo ambiente.
(define aplicar-procedimiento

  (lambda (proc argumentos)

    (cases procVal proc

      (cerradura
        (ids cuerpo amb)

        (evaluar-expresion

          cuerpo

          (extendido
            ids
            argumentos
            amb))))))


; =========================================
;FUNCIÓN: evaluar-expresion
;=========================================
;PROPÓSITO:Evaluar de forma recursiva una estructura de Sintaxis Abstracta (AST)
;bajo un contexto de ambiente determinado.
;
;ARGUMENTOS:
;- exp: Árbol de sintaxis abstracta producido por el parser.
;- env: Ambiente en el que se resolverán los identificadores hallados.
;
;RETORNA:El resultado expresado de la evaluación (Número, Texto, Booleano, etc.)


; Funcion central del interprete.
; Recibe: exp (nodo AST) y env (ambiente lexico).
; Retorna: el valor final de la expresion segun las reglas del lenguaje.
(define evaluar-expresion

  (lambda (exp env)

    (cases expresion exp

      (numero-lit (num)
        num)

      (texto-lit (txt)

  (substring
    txt
    1
    (- (string-length txt) 1)))

      
      (var-exp (id)
        (buscar-variable id env))
      
      (primapp-bin-exp (exp1 prim exp2)

        (let (
              (val1 (evaluar-expresion exp1 env))
              (val2 (evaluar-expresion exp2 env)))

          (evaluar-primitiva-binaria
            prim
            val1
            val2)))

      (primapp-un-exp (prim exp)
                      (let (
                            (val (evaluar-expresion exp env)))    
         (evaluar-primitiva-unaria
          prim
          val)))

      (condicional-exp (test-exp true-exp false-exp)
        (if (not (zero? (evaluar-expresion
         test-exp
         env)))

      (evaluar-expresion
        true-exp
        env)

      (evaluar-expresion
        false-exp
        env)))

      (variableLocal-exp
        (ids exps cuerpo)

          (let loop
            ((ids-rest ids)
             (exps-rest exps)
             (env-actual env))

            (if (null? ids-rest)
                (evaluar-expresion cuerpo env-actual)
                (let ((valor (evaluar-expresion (car exps-rest) env-actual)))
                  (loop (cdr ids-rest)
                        (cdr exps-rest)
                        (extendido (list (car ids-rest))
                                   (list valor)
                                   env-actual))))))

  (letrec-exp (id params cuerpo body)
    (evaluar-expresion
     body
     (extendido-rec
      id
      params
      cuerpo
      env)))
; ===================================
; procedimiento-exp
; ===================================
      (procedimiento-exp
       (ids cuerpo)
       (cerradura
        ids
        cuerpo
        env))

; ===================================
; app-exp
; ===================================
      (app-exp
       (rator rands)
       (let (

             (proc
              (evaluar-expresion
               rator
               env))
             (args
              (map
               (lambda (x)
                 (evaluar-expresion
                  x
                  env))
               rands)))
         (aplicar-procedimiento
          proc
          args)))
          
         )))


;; En una expresión numérica, 0 es falso y cualquier otro valor es verdadero.
;; Devuelve 0 para falso y 1 para verdadero.
; Convierte un numero del lenguaje a verdad/falsedad numerica.
; Recibe: valor numerico.
; Retorna: 0 si es falso, 1 si es verdadero.
(define valor-verdad?

  (lambda (valor)

    (cond

      [(number? valor)
       (if (zero? valor) 0 1)]

      [else
       (eopl:error
         'valor-verdad?
         "Error, valor no numérico ~s"
         valor)])))


;; Evalúa un programa completo
;; utilizando el ambiente inicial.



; Punto de entrada para ejecutar un programa completo.
; Recibe: pgm (AST de tipo programa).
; Retorna: el resultado de evaluar su expresion principal en el ambiente inicial.
(define evaluar-programa

  (lambda (pgm)

    (cases programa pgm

      (un-programa (exp)

        (evaluar-expresion
          exp
          ambiente-inicial)))))

