#lang racket



(define (dif a b)
  (if (string? a)
      (if (string? b)
          (list->string (comparar (string->list a) (string->list b) ))
          #f

      )
   #f
  )
)

(define (comparar L1 L2)
     (if (or (null? L1 ) (null? L2 ))
         null
         (if (eq? (car L1) (car L2))

             (comparar (cdr L1)(cdr L2))

             (cons (car L2)
                   (comparar (cdr L1)(cdr L2)))

         )

 )
)