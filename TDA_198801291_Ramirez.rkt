#lang racket

;;Funcion constructora de archivos
;;Dominio: string , string , entero
;;Recorrido: lista que representa el contenido de un archivo
(define (archivo)
  (lambda (contenido autor code)
       (list contenido autor code )
  )
)

;;Funcion constructora de zonas
;;Dominio: lista workspace , lista index , lista local-repository, lista remote repository
;;Recorrido: lista de zonas
(define (zonas)
  (lambda(x)
      (cond ((not (and (integer? x) (> x 0) (< x 5))) #f)
            ;workspace
            ((= x 1)
               '("repositorio incial")
             )
            
            ((= x 2) "index")
            
            ((= x 3) "local-repository")
            
            ((= x 4) "remote-repository")
       )

    )
)




(provide archivo)
(provide zonas)

;-----testing---



