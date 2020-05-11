#lang racket

;---TDA Zonas-------





(define codigo 0)

;-----------comandos-----------

;commit
(define commit(lambda (comentario)
    (display "Has realizado un commit: ")
    (displayln comentario)
    (display "Codigo unico del commit: ")
    (displayln codigo)
    (set! codigo (+ codigo 1))
    )  
)


;-------------git---------

;pull -> add -> commit -> push

(define (git comando)
    (cond
      ;pull
      
      ;add
      
      ;commit
      ;(git (commit "soy un comentario"))
      [(eq? comando commit)
       (commit)]
      
      ;push
      
    )
)