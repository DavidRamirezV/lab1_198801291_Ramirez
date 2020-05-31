#lang racket
;
(require "TDA_198801291_Ramirez.rkt")
(require "Otras-func_198801291_Ramirez.rkt")


;-----archivos de prueba---

(define a1 (archivo "archivo1.rkt" "soy el primer cambio en el archivo" "David Ramirez"  ))

(define a2 (archivo "archivo1.rkt" "soy el segundo cambio en el archivo" "Juanito Perez" ))

(define a3 (archivo "archivo2.rkt" "este es el segundo archivo, este es mi contenido" "David Ramirez"  ))

(define a4 (archivo "archivo2.rkt" "soy el segundo archivo y mi contenido es muy largo, como si fuera un parrafo completo" "David Ramirez"  ))

(define a5 (archivo "archivo3.rkt" "este es el tercer archivo" "Juanito Perez"  ))

(define z2 (TDA-zonas (list a1 a3) null null null))

;-----------comandos-----------
;pull -> workspacce -> add -> index -> commit -> local repository -> push -> remote repository -> pull ...


;pull
(define (pull)
  (lambda (zona)
        (displayln "Commits recibidos desde remote-repository al Workspace")
    )
)

;add
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

;commit
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


;(((git commit)"se cambio una palabra")  (((git add) (list "archivo1.rkt"))z1))
;(((git commit)"se cambio una palabra")  (((git add) (list "archivo1.rkt"))z2))
;(((git commit)"se agregaron dos archivos")  (((git add) null)z2))

;push
(define (push)
  (lambda (zona)
       (TDA-zonas (car zona)
                  (car (cdr zona))
                  (car (cdr (cdr zona)))
                  (local-to-remote (car (cdr (cdr zona))) (car (cdr (cdr (cdr zona)))))
        )    
    )
)

;; Funcion git principal para ejecutar codigos
;; Dominio: string
;; Recorrido: funcion
(define (git comando)

    (cond
      ;pull
      [(eq? comando pull)
       (pull)]
      ;add
      [(eq? comando add)
       (add)]

      ;commit
      ;((git commit) "soy un comentario")
      [(eq? comando commit)
       (commit)]
      
      ;push
      [(eq? comando push)
       (push)]
      
      [else
       (displayln "Comando Invalido")]
    )
    
)



;(display(zonas->string z2))
;(display(zonas->string (((git commit)"se cambio una palabra")  (((git add) (list "archivo1.rkt"))z2))))
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




(define push1 ((git push)(((git commit)"se cambio una palabra")  (((git add) null)z2))))
(define push2 ((git push)(((git commit)"editado archivo1.rkt, agregado archivo2.rkt")  (((git add) (list "archivo2.rkt" "archivo3.rkt")) (editar-workspace (list a2 a4 a5) push1)))))

;(display(zonas->string push1))
;(display(zonas->string push2))
