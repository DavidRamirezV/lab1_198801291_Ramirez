#lang racket

(define null '())
(define workspace '())
(define index '())
(define local-repository '())
(define remote-repository '())


;;Funcion constructora de archivos
;;Dominio: string , string , entero
;;Recorrido: lista que representa el contenido de un archivo
(define archivo
  (lambda (nombre-archivo contenido autor)
       (list nombre-archivo contenido autor)
  )
)

;;Funcion constructora de zonas
;;Dominio: lista workspace , lista index , lista local-repository, lista remote repository
;;Recorrido: lista de zonas
(define TDA-zonas
  (lambda(w i l r)
       (list w i l r)
    )
)

(define zonas (TDA-zonas workspace index local-repository remote-repository))


(provide archivo)
(provide TDA-zonas)
(provide zonas)
(provide null)
(provide workspace)
(provide index )
(provide local-repository)
(provide remote-repository)

;-----testing---



