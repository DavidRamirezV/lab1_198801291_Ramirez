#lang racket
;
(require "TDA_198801291_Ramirez.rkt")
(define null '())
(define workspace '())
(define index '())
(define local-repository '())
(define remote-repository '())

;-----archivos de prueba---

(define p1 ((archivo)"soy el primer cambio en el archivo" "David Ramirez" "00001"))

(define p2 ((archivo)"soy el segundo cambio en el archivo" "Juanito Perez" "00002"))

(define p3 ((archivo)"soy el tercer cambio en el archivo" "David Ramirez" "00003"))


;-----------comandos-----------
;pull -> add -> commit -> push -> pull ...


;pull
(define (pull)
  (lambda (commits)
        (displayln "Commits recibidos desde remote-repository")
    )
)
  
;add

(define (add)
  (lambda (archivo1)
        
        (displayln "Cambios desde Workspace a Index")
        ;archivo1
    )
)
  
;commit
(define (commit)
  (lambda (comentario)
        (displayln "Cambio realizado: ")
        (displayln comentario)
        (unir-listas ((zonas)1) (list comentario))
    
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

;---- otras funciones -----
;Unir Listas
(define unir-listas
  (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (cons (car l1) (unir-listas (cdr l1) l2))                    
          )
       )
  )
)
