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
;Descripcion: Funcion principal para comparar los elementos del primer string en el segundo, devuelve una lista con los elementos que
;            se eliminaron y los que se agregaron respectivamente
;Ejemplo: >(comparar "Hola soy un texto" "soy otro texto")
;         >'((#t "otro" #t) ("Hola" #t "un" #t))
(define (comparar string1 string2)  
  (list (buscar (regexp-split #px" " string2) (regexp-split #px" " string1))
        (buscar (regexp-split #px" " string1) (regexp-split #px" " string2)))
)


;Dominio: Dos Listas
;Recorrido:  Lista con los datos buscados
;Descripcion: Funcion que busca los elementos de la lista 1 en la lista 2, cuando encuentra un elemento
;             lo elimina de lista 2, si no lo encuentra lo agrega a la lista de salida.             
;Recursion natural
(define (buscar L1 L2)
  (if (null? L1)
      null
      (if (null? L2)
          null
          (if  ( eq? (encontrar (car L1) L2) #t)
                  (cons #t (buscar (cdr L1) (remove (car L1) L2) ))
                  (cons (car L1)  (buscar (cdr L1) L2 ))
           )
              
      )
  ) 
)



(provide comparar)
(provide buscar)
(provide encontrar)