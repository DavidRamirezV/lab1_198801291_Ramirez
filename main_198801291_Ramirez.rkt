#lang racket
;
(require "TDA_198801291_Ramirez.rkt")
(require "Otras-func_198801291_Ramirez.rkt")


;-----archivos de prueba---

(define a1 (archivo "archivo1.rkt" "soy el primer cambio en el archivo" "David Ramirez"  ))

(define a2 (archivo "archivo1.rkt" "soy el segundo cambio en el archivo" "Juanito Perez" ))

(define a3 (archivo "archivo2.rkt" "soy el segundo archivo, este es mi contenido" "David Ramirez"  ))

(define z1 (TDA-zonas (list a2 a3) null null (list a1)))


;(index-to-local (list a1) (car z1) "comentarioo")


;-----------comandos-----------
;pull -> workspacce -> add -> index -> commit -> local repository -> push -> remote repository -> pull ...


;pull
(define (pull)
  (lambda (commits)
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

;push
(define (push)
  (lambda (commits)
        (displayln "Commits agregado a remote-repository")
    
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