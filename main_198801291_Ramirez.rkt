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



;Ejemplo Completo:
;Hay que considerar que se debe aplicar una funcion sobre otra en scheme, por lo tanto las lineas de codigo seran bastante amplias
;suponiendo que tenemos definidos los siguientes archivos (un archivo debe ser una lista con el nombre del archivo, su contenido y su autor (todos en strings)):
(define a1 (archivo "archivo1.rkt" "soy el primer cambio en el archivo" "Juanito Perez"  ))
(define a2 (archivo "archivo2.rkt" "este es el segundo archivo, este es mi contenido" "Juanito Perez"  ))
(define a3 (archivo "archivo1.rkt" "soy el segundo cambio en el archivo" "David Ramirez" ))
(define a4 (archivo "archivo2.rkt" "soy el segundo archivo y mi contenido es muy largo, como si fuera un parrafo completo" "David Ramirez"  ))
(define a5 (archivo "archivo3.rkt" "este es el tercer archivo" "David Ramirez"  ))
;
;Y que nuestra zona inica con un workspace que contiene a a1 y a2 realizados por Juanito Perez:
(define zona1 (TDA-zonas (list a1 a2) null null null))
;
;si quiero hacer un push seria de esta manera:
;((git push)(((git commit)"agregados archivo1.rkt y archivo2.rkt")  (((git add) null)zona1))))
;
;si el usuario quisiera hacer mas push el codigo seria muy largo, para este ejemplo se acortara definiendo cada push
(define push1 ((git push)(((git commit)"se cambio una palabra")  (((git add) null)zona1))))
;
;de este modo si se quisiera  hacer un segundo push el codigo quedaria de esta forma, remplazando las zonas por push1
(define push2 ((git push)(((git commit)"editado archivo2.rkt y agregado archivo3.rkt")  (((git add) (list "archivo2.rkt" "archivo3.rkt")) (editar-workspace (list a3 a4 a5) push1)))))
;
;cabe destacar que he creado una funcion llamada editar-workspace que edita el contenido del workspace, esto para probar con workspace distintos
;en el ejemplo anterior, se edito el workspace reemplazando archivos (a1 y a2) por (a3 a4 y a5)
;para mostrar por pantalla se usa (display(zonas->string zona)), teniendo en cuenta que push1 y push2 son zonas, un ejemplo de su uso seria
;(display(zonas->string push1))
;(display(zonas->string push2))
;
;para hacer pull se sigue funcion ((git pull)zonas), para este ejemplo funcionara si hago pull a push2, ya que push2 tiene contenido en remote repository
;((git pull)push2)
;o bien  para mostrarlo directamente por pantalla
;(display(zonas->string ((git pull)push2)))
;

