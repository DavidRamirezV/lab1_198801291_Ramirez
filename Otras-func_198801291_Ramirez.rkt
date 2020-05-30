#lang racket



;------Funciones para comando add---------

(define (work-to-index L workspace)
  (if (null? L)
      workspace
      (work-to-index-cada-valor L workspace)
   )  
 )


(define (work-to-index-cada-valor L workspace) 
   (if (null? L)
       null
       
       (cons (work-to-index-rec (car L) workspace)
             (work-to-index-cada-valor (cdr L) workspace))
    )
)


(define (work-to-index-rec string workspace)
  (if (null? workspace)
    null
    (if (eq? string (car(car workspace)))
        (car workspace)
        (work-to-index-rec string (cdr workspace))
     )
  )
)


;--------Funciones para comando commit------------

(define (index-to-local index remote string)
    (if (null? index)
        null
        (cons (index-to-local-rec (car index) remote string)
              (index-to-local (cdr index) remote string))
     )   
)

(define (index-to-local-rec archivo remote string)
    (if (null? remote)
        
        (cons-local-rep null archivo string)

        (if (eq? (car archivo) (car (car remote)))
            (cons-local-rep  (car remote) archivo string)
            (index-to-local-rec archivo (cdr remote) string)

         )
   ) 
 )

;Dominio: Dos listas (primera lista es el archivo viejo y segunda lista es el archivo nuevo)
;         y un strings (comentario del commit)
;Recorrido: lista con cambios entre el archivo antiguo y el nuevo
;Descripcion: Funcion que toma dos listas (archivos) y los compara para retornar los cambios entre uno y otro. 
(define (cons-local-rep L1 L2 string)
  (if (null? L1)
       (unir-listas (list (car L2))
                    (unir-listas  (comparar (car (cdr L2)) " ")
                                  (unir-listas (cdr (cdr L2))
                                               (list string))))
       (if (null? L2)
           (unir-listas (list (car L1))
                        (unir-listas (comparar  (car (cdr L1)) " ")
                                     (unir-listas (cdr (cdr L1))
                                                  (list string))))
           (unir-listas (list (car L2))
                        (unir-listas (comparar (car (cdr L2)) (car (cdr L1)))
                                     (unir-listas (cdr (cdr L2))
                                                  (list string)))))
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
        (buscar (regexp-split #px" " string2) (regexp-split #px" " string1) 1)
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
          L1
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




(provide index-to-local)
(provide index-to-local-rec)
(provide cons-local-rep)
(provide work-to-index)
(provide work-to-index-cada-valor)
(provide work-to-index-rec)
(provide comparar)
(provide buscar)
(provide encontrar)
(provide unir-listas)