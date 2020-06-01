#lang racket
;
(require "TDA_198801291_Ramirez.rkt")
(require "Otras-func_198801291_Ramirez.rkt")


;-----------comandos-----------

;workspace -> add -> index -> commit -> local repository -> push -> remote repository -> pull -> workspace ...


;Dominio: TDA-zonas 
;Recorrido: TDA-zonas 
;Descripcion: Funcion que envia cambios desde remote repository a workspace
;Ejemplo:  ((git pull)zonas)
(define (pull)
  (lambda (zona)
        (TDA-zonas   (remote-to-workspace (car (cdr (cdr (cdr zona)))) null)
                      null
                      null
                     (car (cdr (cdr (cdr zona))))
         )
    )
)

;Dominio: lista y TDA-zonas 
;Recorrido: TDA-zonas 
;Descripcion: Funcion que envia cambios desde workspace a index, se encarga de añadir los archivos de workspace a index,
;             para luego poder compararlos con los archivos ya existentes en el remote repository, la lista entregada puede
;             ser nula (null) o conter una lista con nombres de los archivos del workspace que se desean enviar a index
;             por ejemplo (list "archivo1.rkt" "archivo2.rkt")
;Ejemplo:  (((git add)null)zonas)
(define (add)
  (lambda (lista)
       (lambda(zona)        
          (TDA-zonas (car zona)
                     (remove* '(()) (work-to-index lista (car zona)))
                     (car (cdr (cdr zona))) 
                     (car (cdr (cdr (cdr zona))))
         )
       )
    )
)

;Dominio: string y TDA-zonas 
;Recorrido: TDA-zonas 
;Descripcion: Funcion que envia cambios desde index a local repository, guardando cambios que existen entre archivos con los mismos
;             nombres en index y en remote repository
;Ejemplo:  (((git commit)"comentario que explica brevemente cambios realizados")zonas)
(define (commit)
  (lambda (comentario)
     (lambda(zona)  
          (TDA-zonas (car zona)
                     (car (cdr zona))
                     (index-to-local (car (cdr zona)) (car (cdr (cdr (cdr zona)))) comentario)
                     (car (cdr (cdr (cdr zona))))
                 
          )
      )
   )
)



;Dominio: TDA-zonas 
;Recorrido: TDA-zonas 
;Descripcion: Funcion que envia cambios desde local repository a remote repository
;Ejemplo:  ((git push)zonas)
(define (push)
  (lambda (zona)
       (TDA-zonas (car zona)
                  (car (cdr zona))
                  (car (cdr (cdr zona)))
                  (local-to-remote (car (cdr (cdr zona))) (car (cdr (cdr (cdr zona)))))
        )    
    )
)


;Dominio: funcion
;Recorrido: funcion
;Descripcion: Funcion principal para ejecutar comandos
;Ejemplo:  (((git add)null)zonas)
(define (git comando)

    (cond
      ;pull
      [(eq? comando pull)
       (pull)]
      ;add
      [(eq? comando add)
       (add)]

      ;commit
      [(eq? comando commit)
       (commit)]
      
      ;push
      [(eq? comando push)
       (push)]
      
      [else
       (display "Comando Invalido\n")]
    )
    
)

;Dominio: zonas(lista)
;Recorrido: string
;Descripcion: Funcion que transforma una lista con zonas en un string, este string se puede mostrar con la funcion display para
;             que se aprecien los saltos de linea
(define (zonas->string zonas)
     (string-append    "------------WORKSPACE-----------\n"
                     (zona-a-string (car zonas) "w")
                     "\n--------------INDEX-------------\n"
                     (zona-a-string (car (cdr zonas)) "i")
                     "\n---------LOCAL REPOSITORY-------\n"
                     (zona-a-string (car (cdr (cdr zonas))) "l")
                     "\n--------REMOTE REPOSITORY-------\n"
                     (zona-a-string (car (cdr (cdr (cdr zonas)))) "r")
                     "\n")
)



;para el archivo "ejemplo_198801291_Ramirez.rkt"
(provide git)
(provide push)
(provide add)
(provide pull)
(provide commit)
(provide zonas->string)