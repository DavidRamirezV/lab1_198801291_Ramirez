#lang racket

(require "TDA_198801291_Ramirez.rkt")

;------Funciones para comando pull--------

;Dominio: lista (zona remote repository ) y lista (inicial null, pero funciona como apilador para recursion de cola)
;Recorrido: lista con archivos 
;Descripcion: Fucnion que toma todos los archivos de remote repository y los reemplaza en workspace, si los archivos no existen en
;             remote repository los archivos se eliminaran para que el remote repository y workspace queden iguales.
;Recursion a la cola
(define (remote-to-workspace remote construido)
  (if (null? remote)
      construido
      (if (null? construido)
          (remote-to-workspace (cdr remote) (list (archivo (car (car remote))
                                                           (car (cdr (car remote)))
                                                           (car (cdr (reverse (car remote))))))  )
          (remote-to-workspace (cdr remote)  (unir-listas  construido
                                                           (list (archivo (car (car remote))
                                                                          (car (cdr (car remote)))
                                                                          (car (cdr (reverse (car remote))))))))
      )
  )  
)


;------Funciones para comando add---------

;Dominio: Una lista con los nombres de achivos que se quieren editar (puede ser null) y
;         una lista que representa a la zona workspace
;Recorrido: workspace si la lista entregada es null o una lista con los archivos que se quieren agregar a index 
;Descripcion:Añade archivos (nombrados en una lista) a index
;Ejemplo: > (work-to-index (list "archivo1.rkt") (list (list "archivo1.rkt" "soy un archivo de ejemplo" "David Ramirez"  )))
;          '(("archivo1.rkt" "soy un archivo de ejemplo" "David Ramirez"))
(define (work-to-index L workspace)
  (if (null? L)
      workspace
      (work-to-index-cada-valor L workspace)
   )  
 )

;Dominio: Una lista con los nombres de achivos que se quieren editar (puede ser null) y
;         una lista que representa a la zona workspace
;Recorrido: Una lista con los archivos que se quieren agregar a index
;Descripcion: Busca cada valor de la lista en el workspace, construyendo una lista con los elementos que se quieren editar
;Recursion natural
(define (work-to-index-cada-valor L workspace) 
   (if (null? L)
       null
       
       (cons (work-to-index-rec (car L) workspace)
             (work-to-index-cada-valor (cdr L) workspace))
    )
)


;Dominio: string (nombre del archivo) y lista (workspace)
;Recorrido: lista (que representa un archivo)
;Descripcion: funcion recursiva que se encarga de encontrar el archivo que se busca dentro del workspace
;             si encuentra el nombre (string) retorna el archivo.
;Recursion natural
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


;Dominio: dos listas (la primera representa a index, la segunda a remote repository) y un string (representa el comentario)
;Recorrido: null si index esta vacio o lista con los cambios que existen entre el archivo nuevo (ubicado en index) y el antiguo (ubicado en remote repository)
;Descripcion: funcion principal de commit que buscara cada archivo del index en el remote repository, comparara los cambios que existen entre los archivos nuevos
;             y los antiguos, devolviendo una lista con el nombre del archivo encontrado, los cambios realizados, autor y en ultima posicion se guardara el comentario.
;Recursion natural
;Ejemplo:> (define index1 (list(list "archivo1.rkt" "contenido nuevo del archivo" "Juanito Perez")))
;        > index1
;        '(("archivo1.rkt" "contenido nuevo del archivo"))
;        > (define remote1 (list (list "archivo1.rkt" "contenido antiguo" "David Ramirez")))
;        > remote1
;        '(("archivo1.rkt" "contenido antiguo"))
;        > (index-to-local index1 remote1 "cambiado contenido archivo1.rkt")
;        '(("archivo1.rkt" ("contenido") (#t "antiguo") (#t "nuevo" "del" "archivo") "Juanito Perez" "cambiado contenido archivo1.rkt"))
 
(define (index-to-local index remote string)
    (if (null? index)
        null
        (cons (index-to-local-rec (car index) remote string)
              (index-to-local (cdr index) remote string))
     )   
)


;Dominio: dos listas (la primera es un archivo del index, la segunda el remote repository) y un string(comentario)
;Recorrido: lista con los cambios en de el archivo entregado (comparando archivo entregado con el mismo archivo en remote repository)
;Descripcion: Funcion que busca el archivo (por su nombre) en el remote repository y compara (funcion cons-local-rep) el archivo entregado
;             con el encontrado.
;Recursion natural
(define (index-to-local-rec archivo remote string)
    (if (null? remote)
        
        (cons-local-rep null archivo string)

        (if (eq? (car archivo) (car (car remote)))
            (cons-local-rep  (car remote) archivo string)
            (index-to-local-rec archivo (cdr remote) string)

         )
   ) 
 )


;Dominio: Dos listas (primera lista es el archivo antiguo y segunda lista es el archivo nuevo)
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
;            se mantuvieron, los que se eliminaron y otra con los elementos que son nuevos, respectivamente. 
;Ejemplo:> (comparar "Hola soy un texto" "soy otro texto")
;        > '(("soy" "texto") (#t "otro" #t) ("Hola" #t "un" #t))
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
; Ejemplo: > (encontrar "perro" (list "gato" "conejo" "perro")) 
;            #t
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
;Dominio: dos listas (la primera representa el local repository y la segunda el remote repository)
;Recorrido: lista con archivos de remote
;Descripcion: Funcion que envia cambios desde local repository a remote repository, decodificando cambios realizados para mostrar un archivo con
;             un contenido completo.
(define (local-to-remote local remote)
     (unir-listas  (remove* '(()) (archivos-no-editados local remote))
          (archivos-editados local remote)) 
 )


;Dominio: dos listas (la primera representa el local repository y la segunda el remote repository)
;Recorrido: lista con elementos no editados,
;Descripcion: Funcion busca los elementos que no han sido editados y los añade en una lista
;Recursion natural
(define (archivos-no-editados local remote)
  (if (null? remote)
       null
      (cons (archivos-no-editados-rec local (car remote))
            (archivos-no-editados local (cdr remote)))
            
   )   
 )

;Dominio: dos listas (la primera representa el local repository y la segunda representa un archivo de remote repository)
;Recorrido: elementos no editado
;Descripcion: Funcion que compara el nombre del archivo entregado con el de los archivos de index, si no encuentra el archivo, lo retorna,
;             si lo encuentra retorna null
;Recursion natural
(define( archivos-no-editados-rec index archivo )
   (if (null? index)
       archivo
       (if (eq? (car (car index)) (car archivo))
            null
            (archivos-no-editados-rec (cdr index) archivo )
            
            
        )       
   )
)

;Dominio: dos listas (la primera representa el local repository y la segunda el remote repository)
;Recorrido: lista con elementos editados
;Descripcion: Funcion busca los elementos que  han sido editados y los añade en una lista
;Recursion natural
(define (archivos-editados local remote)
  (if (null? local)
      null
      (cons (archivos-editados-rec (car local) remote)
            (archivos-editados (cdr local) remote))
   )  
)

;Dominio: dos listas (la primera representa representa un archivo del local repository y la segunda el remote repository)
;Recorrido: elementos editados
;Descripcion: Funcion que compara el nombre del archivo entregado con el de los archivos de remote repository, si no encuentra el archivo lo agregua,
;             si lo encuentra lo reemplaza
;Recursion natural
(define (archivos-editados-rec commit remote)
    (if (null? remote)        
        (archivo-remote commit)
        (if (eq? (car commit) (car (car remote)))
            (archivo-remote commit)
            (archivos-editados-rec commit (cdr remote) )
            
            
         )
   ) 
 )

;Dominio: archivo de local repository (lista)
;Recorrido: lista con un archivo decodificado
;Descripcion: Funcion que transforma un archivo de local repository a un archivo de remote repository, traduciendo
;             el archivo codificado con las palabras que se cambiaron entre el archivo nuevo y el antiguo
(define (archivo-remote archivo)
  (unir-listas (list (car archivo))
               (unir-listas (traducir (car (cdr archivo))
                                      (car (cdr (cdr (cdr archivo)))))
                            (cdr archivo)))
   

 )

;Dominio: dos listas (la primera representa las palabras que se mantuvieron al comparar y la segunda son las palabras nuevas)
;Recorrido: lista con el contenido completo decodificado
;Descripcion: Funcion que decodifica mi representacion de un archivo de local repository, estos archivos tienen el siguiente formato
;             '(("soy" "texto") (#t "otro" #t) ("Hola" #t "un" #t)) donde el el primer valor son las palabras que se mantienen, el segundo las que se borran
;             y el tercero las nuevas, reemplaza las #t en las nuevas por las palabras de las que se mantienen, esta funcion filtra el caso de L1 null
;Ejemplo: (traducir '("soy" "texto") '("Hola" #t "un" #t) )
;         '("Hola soy un texto")
(define (traducir L1 L2 )
  (if (null? L1)
     (list (string-join L2))
     (list (string-join (decodificar L1 L2)))
   )
  
)

;Dominio: dos listas (la primera representa las palabras que se mantuvieron al comparar y la segunda son las palabras nuevas)
;Recorrido: lista de strings de cada palabra decodificada
;Descripcion: Funcion que decodifica un archivo de local repository, buscando cada #t en L2 y reemplazando por una palabra en L1
;Ejemplo: (decodificar '("soy" "texto") '("Hola" #t "un" #t))
;         '("Hola" "soy" "un" "texto")
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

;Dominio: una lista(puede ser workspace, index, local repository o remote repository) y un caracter (puede ser "w" "i" "l" "r") 
;Recorrido: funciones
;Descripcion: Funcion principal que transforma una zona a string, para workspace e index se usa la misma funcion,
;             para local y remote repository se usa una distinta (una para cada una)
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


;Dominio: lista de strings
;Recorrido: string
;Descripcion:Transforma una lista de strings a string añadiendo el caracter "\n" entre cada elemento de la lista
;Ejemplo : (add-salto-linea '("uno" "dos" "tres"))
;          "uno\ndos\ntres\n"
(define (add-salto-linea L)
  (if (null? L)
     ""
    (string-append (string-append (car L) "\n")
           (add-salto-linea (cdr L)))
    )
)

;Dominio: lista (zona que puede ser workspace o index)
;Recorrido: string
;Descripcion:Transforma workspace o index a string, usando la funcion add-salto-linea para cada archivo dentro de la zona
;Recursion natural
;Ejemplo : (a-string (list '("uno" "dos" "tres") '("cuatro" "cinco")))
;          "uno\ndos\ntres\n\ncuatro\ncinco\n\n"
(define (a-string zona)
  (if (null? zona)
      ""
     (string-append (string-append (add-salto-linea (car zona)) "\n")
           (a-string (cdr zona)))

   )  
 )



;Dominio: lista (zona local repository)
;Recorrido: string
;Descripcion:Transforma local repository a string, lee cada archivo de local repository  recursivamente y lo transorfma a string
;Recursion natural
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
;Dominio: lista (zona remote repository)
;Recorrido: string
;Descripcion:Transforma remote repository a string, lee cada archivo de remote repository  recursivamente y lo transorfma a string
;            (no muestra todo el contenido guardado, solo muestra nombre del archivo, contenido, ultimo commit en ese archivo y por quien fue realizado dicho commit)
;Recursion natural
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
;Descripcion: Funcion que une dos listas  (funciona de manera distinta a list y cons)       
;Recursion natural
;Ejemplo:>(unir-listas (list "hola") (list "soy" "dos" "listas" "unidas"))
;         '("hola" "soy" "dos" "listas" "unidas")
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
;Recorrido: TDA-zonas
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
(provide remote-to-workspace)
(provide index-to-local)
(provide work-to-index)
(provide local-to-remote)
(provide zona-a-string)
(provide unir-listas)
(provide editar-workspace)