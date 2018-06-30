# Proyecto - Estadistica
# Nombres : Maria Victoria Oliveros 14-10755
#           Gabriel Perez Salgado   13-11104
#           Paul Baptista           10-10056

library(knitr)  # Para poder sacar una tabla bonita en html con kable

# Lectura base de datos
 datos<-proceso
 attach(datos)

# Funcion que retorna matriz con datos estadisticos de la lista pasada como argumento
 analisis <-function(lista)
 {
   m <-matrix(NA, nrow =8, ncol = 7 )
   for(i in 1: 8)
   { 
     m[i,] <-c(summary(lista[[i]]), sd(lista[[i]]))
   }
   m <-round(m, 3)
   colnames(m) <- c("Mínimo", "q1", "q2", "Media", "q3", "Máximo", "SD")
   rownames(m) <- c("y", "x1", "x2", "x3", "x4", "x5", "x6", "x7")
   return(m)
 }

# Valores calculados para las distintas variables encontradas en las muestras independientes del Centro de Investigacion 
  kable(analisis(datos))

# Valores calculados para las distintas variables encontradas en las muestras dependiendo del Centro de Investigacion 
  #datosCentros<-split(datos, Cent)
  kable(analisis(A))
  kable(analisis(B))
  kable(analisis(C))
  kable(analisis(D))
  
# Histogramas para 

  