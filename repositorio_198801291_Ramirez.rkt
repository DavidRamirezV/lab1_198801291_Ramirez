#lang racket

; en vista deque debo probar el codigo creare  un archivo de prueba
;a la izquierda el commit, a la derecha el codigo del commit
;en el futuro de implementar que el usuario cambie el string del commit
;y cambiar el codigo de este commit
(define (comentario)
  (cons "commit: cambie una linea del codigo" "codigo: 2146314")
)


;-----------comandos-----------

;commit
(define commit(lambda (comentario codigo)
         (displayln "Has realizado un commit:")
         (displayln comentario)
         (displayln "Codigo unico del commit:")
         (displayln codigo)               
      )  
  )