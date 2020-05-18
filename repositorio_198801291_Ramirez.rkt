#lang racket

;---TDA------

;Workspace

;Index

;Local Repository

;Remote Repository

;Archivo

;Zonas TDA
(define (zonas)
  (define workspace null)
  (define index null)
  (define local-repository null)
  (define remote-repository null)
  (list workspace index local-repository remote-repository)
)


;-----------comandos-----------
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
    )
)
  
;commit
(define (commit)
  (lambda (comentario)
        (displayln "Cambio realizado: ")
        (displayln comentario)
    )
)
  
;push
(define (push)
  (lambda (commits)
        (displayln "Commits agregado a remote-repository")
    )
)
  

;-------------git---------

;pull -> add -> commit -> push

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