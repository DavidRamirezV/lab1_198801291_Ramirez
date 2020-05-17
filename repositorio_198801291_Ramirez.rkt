#lang racket

;---TDA------
;TDA zonas
(define (zonas)
  (define workplace (list))
  (define index (list))
  (define loca-repository (list))
  (define remote-repository (list))
  (list workplace index loca-repository remote-repository)
)

;TDA archivos
(define (archivo)
 )



;-----------comandos-----------
;pull

;add


;commit


;push

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