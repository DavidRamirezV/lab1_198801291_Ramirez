#lang racket

(require "TDA_198801291_Ramirez.rkt")
;------Funciones para comando add---------


;Dominio:
;Recorrido;
;Descripcion;
;Recursion
(define (work-to-index L workspace)
  (if (null? L)
      workspace
      (work-to-index-cada-valor L workspace)
   )  
 )

;Dominio:
;Recorrido;
;Descripcion;
;Recursion
(define (work-to-index-cada-valor L workspace) 
   (if (null? L)
       null
       
       (cons (work-to-index-rec (car L) workspace)
             (work-to-index-cada-valor (cdr L) workspace))
    )
)


;Dominio:
;Recorrido;
;Descripcion;
;Recursion
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


;Dominio:
;Recorrido;
;Descripcion;
;Recursion
(define (index-to-local index remote string)
    (if (null? index)
        null
        (cons (index-to-local-rec (car index) remote string)
              (index-to-local (cdr index) remote string))
     )   
)


;Dominio:
;Recorrido;
;Descripcion;
;Recursion
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

;-----Funciones para comando push---------
(define (local-to-remote local remote)
  (if (null? local)
      null
      (cons (local-to-remote-rec (car local) remote)
            (local-to-remote (cdr local) remote))
   )  
)



(define (local-to-remote-rec commit remote)
    (if (null? remote)        
        (archivo-remote commit)
        (if (eq? (car commit) (car (car remote)))
            (archivo-remote commit)
            (local-to-remote-rec commit (cdr remote) )
            
            
         )
   ) 
 )

(define (archivo-remote archivo)
  (unir-listas (list (car archivo))
               (unir-listas (traducir (car (cdr archivo))
                                      (car (cdr (cdr (cdr archivo) ))))
                            (cdr archivo)))
   

 )


(define (traducir L1 L2 )
  (if (null? L1)
     (list (string-join L2))
     (list (string-join (decodificar L1 L2)))
   )
  
)


(define (decodificar L1 L2)
  (if (null? L2)
      null
      (if (eq? (car L2) #t)
          (cons  (car L1)
                 (decodificar (cdr L1) (cdr L2)))
          (cons (car L2)
                (decodificar L1 (cdr L2)))
          
       )   
   )
 )


;-----zonas->string-----
(define (zona-a-string zona char)
  (if (null? zona)
      ""
      (cond
          [(or (eq? char "w") (eq? char "i"))
           (a-string zona)]

          [(eq? char "l")
           (local-a-string zona)]
          
          [(eq? char "r")
           (remote-a-string zona)]
        
       )    
   )
)

;(add-\n '("aiuda" "porfavor"))
;>"aiuda\nporfavor\n"
(define (add-salto-linea L)
  (if (null? L)
     ""
    (string-append (string-append (car L) "\n")
           (add-salto-linea (cdr L)))
    )
)

(define (a-string zona)
  (if (null? zona)
      ""
     (string-append (string-append (add-salto-linea (car zona)) "\n")
           (a-string (cdr zona)))

   )  
 )


(define (local-a-string zona)
  (if (null? zona)
      ""
      (string-append (car (cdr(reverse (car zona)))) ": " (car (reverse (car zona))) "\n"
                     (car (car zona)) "\n"
                     "Contenido intacto: " (string-join (car (cdr (car zona)))) "\n"
                     "Contenido eliminado: " (string-join (remove* '(#t) (car (cdr (cdr(car zona)))))) "\n"
                     "Contenido nuevo: " (string-join (remove* '(#t) (car (cdr (cdr (cdr(car zona))))))) "\n\n"                     
                     (local-a-string (cdr zona)))
                     
   )
)
(define (remote-a-string zona)
  (if (null? zona)
      ""
      (string-append (car (car zona)) "\n"
                     (car (cdr (car zona))) "\n"
                     "commit: "  (car (reverse (car zona))) "\n"
                     "realizado por: " (car (cdr(reverse (car zona)))) "\n\n"
                     (remote-a-string (cdr zona)))
                     
   )
)

;------Otras Funciones-----

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

;Dominio: Lista con archivos y zonas
;Recorrido: zonas
;Descripcion: Funcion que edita el contenido de workspace ya que la funcion add añade siempre desde workspace y
;             se necesita una forma de probar funciones con workspace distintos.
(define (editar-workspace L zonas)
  (TDA-zonas L
             (car (cdr zonas))
             (car (cdr (cdr zonas))) 
             (car (cdr (cdr (cdr zonas))))
   
   )
)

;----provides-----

(provide index-to-local)
(provide work-to-index)
(provide local-to-remote)
(provide zona-a-string)
(provide unir-listas)
(provide editar-workspace)