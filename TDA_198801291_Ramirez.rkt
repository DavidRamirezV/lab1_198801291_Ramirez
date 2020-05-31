#lang racket

;Dominio: -
;Recorrido: lista vacia
;Descripcion: Funcion null retorna una lista vacia
(define null '())

;Dominio: string , string , string
;Recorrido: lista que representa el contenido de un archivo
;Descripcion: Funcion constructora de archivos
(define archivo
  (lambda (nombre-archivo contenido autor)
       (list nombre-archivo contenido autor)
  )
)


;Dominio: lista workspace , lista index , lista local-repository, lista remote repository
;Recorrido: lista de zonas
;Descripcion: Funcion constructora de zonas
(define TDA-zonas
  (lambda(w i l r)
       (list w i l r)
    )
)



;Dominio: lista workspace , lista index , lista local-repository, lista remote repository
;Recorrido: lista de zonas
;Descripcion: Inicio con una lista de zonas vacio
(define zonas (TDA-zonas null null null null))


(provide archivo)
(provide TDA-zonas)
(provide zonas)
(provide null)

;-----testing---



