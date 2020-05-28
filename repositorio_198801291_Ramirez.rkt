#lang racket
;
(require "TDA_198801291_Ramirez.rkt")
(require "Otras-func_198801291_Ramirez.rkt")


;-----archivos de prueba---

(define p1 ((archivo)"soy el primer cambio en el archivo" "David Ramirez" "00001"))

(define p2 ((archivo)"soy el segundo cambio en el archivo" "Juanito Perez" "00002"))

(define p3 ((archivo)"soy el tercer cambio en el archivo" "David Ramirez" "00003"))


;-----------comandos-----------
;pull -> add -> commit -> push -> pull ...


;pull
(define (pull)
  (lambda (commits)
        (displayln "Commits recibidos desde remote-repository al Workspace")
    )
)
  
;add

(define (add)
  (lambda (archivo1)
       (lambda(zona) 
        (displayln "Cambios desde Workspace a Index:")        
        (list (car zona)
              (cons (car(cdr zona)) (list archivo1))
              (car (cdr (cdr zona))) 
              (car(cdr (cdr (cdr zona))))
         )
       )
    )
)
  
;commit
(define (commit)
  (lambda (comentario)
     (lambda(zona)
        (displayln "Cambio realizado: ")
        (writeln comentario)
        (list (car zona)
              (car(cdr zona))
              (cons (car (cdr (cdr zona))) (list comentario))
              (car(cdr (cdr (cdr zona))))
         )
       )
   )
)
  
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
    )
    
)