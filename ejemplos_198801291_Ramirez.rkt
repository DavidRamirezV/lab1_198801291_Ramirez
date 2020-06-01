#lang racket
;
(require "TDA_198801291_Ramirez.rkt")
(require "Otras-func_198801291_Ramirez.rkt")
(require "main_198801291_Ramirez.rkt")


;Ejemplo Completo:
;Hay que considerar que se debe aplicar una funcion sobre otra en scheme, por lo tanto las lineas de codigo seran bastante amplias
;suponiendo que tenemos definidos los siguientes archivos (un archivo debe ser una lista con el nombre del archivo, su contenido y su autor (todos en strings)):
(define a1 (archivo "archivo1.rkt" "soy el primer cambio en el archivo" "Juanito Perez"  ))
(define a2 (archivo "archivo2.rkt" "este es el segundo archivo, este es mi contenido" "Juanito Perez"  ))
(define a3 (archivo "archivo1.rkt" "soy el segundo cambio en el archivo" "David Ramirez" ))
(define a4 (archivo "archivo2.rkt" "soy el segundo archivo y mi contenido es muy largo, como si fuera un parrafo completo" "David Ramirez"  ))
(define a5 (archivo "archivo3.rkt" "este es el tercer archivo" "David Ramirez"  ))
;
;Y que nuestra zona inica con un workspace que contiene a a1 y a2 realizados por Juanito Perez:
(define zona1 (TDA-zonas (list a1 a2) null null null))
;
;si quiero hacer un push seria de esta manera:
;((git push)(((git commit)"agregados archivo1.rkt y archivo2.rkt")  (((git add) null)zona1))))
;
;si el usuario quisiera hacer mas push el codigo seria muy largo, para este ejemplo se acortara definiendo cada push
(define push1 ((git push)(((git commit)"se cambio una palabra")  (((git add) null)zona1))))
;
;de este modo si se quisiera  hacer un segundo push el codigo quedaria de esta forma, remplazando las zonas por push1
(define push2 ((git push)(((git commit)"editado archivo2.rkt y agregado archivo3.rkt")  (((git add) (list "archivo2.rkt" "archivo3.rkt")) (editar-workspace (list a3 a4 a5) push1)))))
;
;cabe destacar que he creado una funcion llamada editar-workspace que edita el contenido del workspace, esto para probar con workspace distintos
;en el ejemplo anterior, se edito el workspace reemplazando archivos (a1 y a2) por (a3 a4 y a5)
;para mostrar por pantalla se usa (display(zonas->string zona)), teniendo en cuenta que push1 y push2 son zonas, un ejemplo de su uso seria
;(display(zonas->string push1))
;(display(zonas->string push2))
;
;para hacer pull se sigue funcion ((git pull)zonas), para este ejemplo funcionara si hago pull a push2, ya que push2 tiene contenido en remote repository
;((git pull)push2)
;o bien  para mostrarlo directamente por pantalla
;(display(zonas->string ((git pull)push2)))
;
