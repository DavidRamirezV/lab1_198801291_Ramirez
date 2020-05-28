#lang racket

;Dominio: Un string y una lista
;Recorrido: Booleano
;Descripcion: Funcion que devuelve True si se encuentra el string en la lista, False si no se encuentra
;Recursion natural
(define (encontrar elemento L) 
  (if (null? L)
      #f
      (if (equal? elemento (car L))
              #t
              (encontrar elemento (cdr L) )
              
       )
   )
)


;Dominio: Dos strings
;Recorrido: Lista con strings eliminados y strings nuevos
;Descripcion: Funcion principal para comparar los elementos del primer string dentro del segundo string, devuelve una lista con los elementos que
;            se mantuvieron y otra con los elementos que son nuevos, respectivamente. 
;Ejemplo: >(comparar "Hola soy un texto" "soy otro texto")
;         '(("soy" "texto") ("Hola" #t "un" #t))
(define (comparar string1 string2)  
  (list (buscar (regexp-split #px" " string2) (regexp-split #px" " string1) 2)
        (buscar (regexp-split #px" " string1) (regexp-split #px" " string2) 1))
)


;Dominio: Dos Listas y un entero (1 o 2) que decide que funcion se ejecuta
;Recorrido:  Lista con los datos buscados
;Descripcion: Funcion que busca los elementos de la lista 1 en la lista 2,dependiendo del modo ejecutara algo distinto:
;             (modo 1) Cuando encuentra un elemento de la lista 1 en la lista 2 lo codifica en la lista de salida como un True,
;                      si no lo encuentra lo agrega a la lista de salida.             
;             (modo 2) Cuando encuentra un elemento de la lista 1 en la lista 2 lo agrega en la lista de salida,
;                      de otro modo se sigue ejecutando la funcion recursivamente, dejando solo los datos iguales en la lista 1 y la lista 2
;Recursion natural
(define (buscar L1 L2 modo)
  (if (null? L1)
      null
      (if (null? L2)
          null
          (cond
               [(= 1 modo)
                (if ( eq? (encontrar (car L1) L2) #t)                
                    (cons #t (buscar (cdr L1) (remove (car L1) L2) 1))
                    (cons (car L1)  (buscar (cdr L1) L2 1) ))
                ]
               [(= 2 modo)
                (if  ( eq? (encontrar (car L1) L2) #t)
                      (cons (car L1)  (buscar (cdr L1) L2 2) )
                      (buscar (cdr L1) (remove (car L1) L2) 2))
                ]
            )        
      )
  ) 
)



;Dominio: Dos Listas
;Recorrido: Retorna una lista que contiene ambas listas unidas
;Descripcion: Funcion que une dos listas            
;Recursion natural
(define unir-listas
  (lambda (L1 L2)
      (if (null? L1)
          L2
          (if (null? L2)
              L1
              (cons (car L1) (unir-listas (cdr L1) L2))                    
          )
       )
  )
)


(provide comparar)
(provide buscar)
(provide encontrar)
(provide unir-listas)