#lang racket
;
(require "TDA_198801291_Ramirez.rkt")
(require "Otras-func_198801291_Ramirez.rkt")


;-----archivos de prueba---

(define a1 (archivo "archivo1.rkt" "soy el primer cambio en el archivo" "David Ramirez"  ))

(define a2 (archivo "archivo1.rkt" "soy el segundo cambio en el archivo" "Juanito Perez" ))

(define a3 (archivo "archivo2.rkt" "soy el tercer cambio en el archivo" "David Ramirez"  ))

(define z1 (TDA-zonas (list a1 a3) null null null))

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
  (lambda (archivo1)
       (lambda(zona)        
        ((TDA-zonas) (car zona)
                     (if (null? (car(cdr zona)))
                         (cons (car(cdr zona))
                               (list archivo1))
                         (unir-listas (car(cdr zona))
                                      (list archivo1)))
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
        ((TDA-zonas) (car zona)
                     (car (cdr zona))
                     (cons-local-rep (car (reverse (car (cdr zona))))
                                     (car (cdr (reverse (car (cdr zona)))))
                                     comentario)
                     (car (cdr (cdr (cdr zona))))
                 
        )
      )
   )
)


;(((git commit)"se cambio una palabra")  (((git add) a1)zonas))
;Primero el nuevo, luego el antiguo
(define (cons-local-rep L1 L2 string)
  (if (null? L2)
       (unir-listas (comparar (car L1) " ")
                    (unir-listas (cdr L1)
                                 (list string)))
       (if (null? L1)
           (unir-listas (comparar  (car L2) " ")
                        (unir-listas (cdr L2)
                                     (list string)))
           (unir-listas (comparar (car L1) (car L2))
                        (unir-listas (cdr L1)
                                     (list string))))
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