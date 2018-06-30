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
   m <-matrix(NA, nrow =8, ncol = 8 )
   for(i in 1: 8)
   { 
     m[i,] <-c(length(lista[[i]]), summary(lista[[i]]), sd(lista[[i]]))
   }
   m <-round(m, 3)
   colnames(m) <- c("Muestras","Mínimo", "q1", "q2", "Media", "q3", "Máximo", "SD")
   rownames(m) <- c("y", "x1", "x2", "x3", "x4", "x5", "x6", "x7")
   return(m)
 }

# Valores calculados para las distintas variables encontradas en las muestras independientes del Centro de Investigacion 
  kable(analisis(datos))

# Valores calculados para las distintas variables encontradas en las muestras dependiendo del Centro de Investigacion 
  datosCentros<-split(datos, Cent)
  kable(analisis(datosCentros$A))
  kable(analisis(datosCentros$B))
  kable(analisis(datosCentros$C))
  kable(analisis(datosCentros$D))
  
# Histogramas para cada variable independiente del Centro de Investigacion
  hist  (y, main = "Porcentaje de óxido de aluminio que precipita.", ylab = "Frecuencia", xlab = "Porcentaje")
  hist  (x1, main = "Concentración del ácido.", ylab = "Frecuencia", xlab = "Contracion", ylim = c(0,25))
  hist  (x2, main = "Valor final de pH de la solución en unidades de pH.", ylab = "Frecuencia", xlab = "pH")
  hist  (x3, main = "Temperatura del proceso en grados centígrados.", ylab = "Frecuencia", xlab = "Grados Centigrados", ylim = c(0,70))
  hist  (x4, main = "Concentración de la base.", ylab = "Frecuencia", xlab = "Concentracion")
  hist  (x5, main = "Velocidad de agitación en rpm.", ylab = "Frecuencia", xlab = "RPM", xlim = c(0,2000))
  hist  (x6, main = "Velocidad de adición de la base en mililitros por hora.", ylab = "Frecuencia", xlab = "Mililitros por hora", ylim = c(0,60))
  hist  (x7, main = "Tiempo del proceso en horas.", ylab = "Frecuencia", xlab = "Horas", ylim = c(0,60))

# Histogramas para cada variable independiente del Centro de Investigacion dependiendo del Centro de Investigacion
  Y<-split(y,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(Y))
  {
    hist(Y[[i]], main = paste("Centro de Investigacion", names(Y)[i]), ylab = "Frecuencia", xlab = "Porcentaje de óxido de aluminio que precipita.")
  }
  
  X1<-split(x1,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(X1))
  {
    hist(X1[[i]], main = paste("Centro de Investigacion", names(X1)[i]), ylab = "Frecuencia", xlab = "Concentración del ácido.", ylim = c(0,20))
  }
  
  X2<-split(x2,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(Y))
  {
    hist(X2[[i]], main = paste("Centro de Investigacion", names(X2)[i]), ylab = "Frecuencia", xlab = "Valor final de pH de la solución en unidades de pH.")
  }
  
  X3<-split(x3,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(X3))
  {
    hist(X3[[i]], main = paste("Centro de Investigacion", names(X3)[i]), ylab = "Frecuencia", xlab = "Temperatura del proceso en grados centígrados.")
  }
  
  X4<-split(x4,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(X4))
  {
    hist  (X4[[i]], main = paste("Centro de Investigacion", names(X4)[i]), ylab = "Frecuencia", xlab = "Concentración de la base.")
  }
  
  X5<-split(x5,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(X5))
  {
    hist  (X5[[i]], main = paste("Centro de Investigacion", names(X5)[i]), ylab = "Frecuencia", xlab = "Velocidad de agitación en RPM.")
  }
  
  X6<-split(x6,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(X6))
  {
    hist  (X6[[i]], main = paste("Centro de Investigacion", names(X6)[i]), ylab = "Frecuencia", xlab = "Velocidad de adición de la base en mililitros por hora.")
  }
  
  X7<-split(x7,Cent)
  par(mfrow =c(2,2))
  for(i in 1 :length(X7))
  {
    hist  (X7[[i]], main = paste("Centro de Investigacion", names(X7)[i]), ylab = "Frecuencia", xlab = "Tiempo del proceso en horas.")
  }

# Diagramas de Caja para cada variable independientes del Centro de Ivestigacion
  par(mfrow = c(1,1))
  boxplot(y, main = "Porcentaje de óxido de aluminio que precipita.")
  boxplot(x1, main = "Concentración del ácido.")
  boxplot(x2, main = "Valor final de pH de la solución en unidades de pH.")
  boxplot(x3, main = "Temperatura del proceso en grados centígrados.")
  boxplot(x4, main = "Concentración de la base.")
  boxplot(x5, main = "Velocidad de agitación en RPM.")
  boxplot(x6, main = "Velocidad de adición de la base en mililitros por hora.")
  boxplot(x7, main = "Tiempo del proceso en horas.")

# Diagramas de caja para cada variable dependiendo del Centro de Investigacion
  boxplot(Y$A, Y$B, Y$C, Y$D, main = "Porcentaje de óxido de aluminio que precipita.", names = c("A","B","C","D") )  
  boxplot(X1$A, X1$B, X1$C, X1$D, main = "Concentración del ácido.", names = c("A","B","C","D") )  
  boxplot(X2$A, X2$B, X2$C, X2$D, main = "Valor final de pH de la solución en unidades de pH.", names = c("A","B","C","D") )  
  boxplot(X3$A, X3$B, X3$C, X3$D, main = "Temperatura del proceso en grados centígrados.", names = c("A","B","C","D") )  
  boxplot(X4$A, X4$B, X4$C, X4$D, main = "Concentración de la base.", names = c("A","B","C","D") )  
  boxplot(X5$A, X5$B, X5$C, X5$D, main = "Velocidad de agitación en RPM.", names = c("A","B","C","D") )  
  boxplot(X6$A, X6$B, X6$C, X6$D, main = "Velocidad de adición de la base en mililitros por hora.", names = c("A","B","C","D") )  
  boxplot(X7$A, X7$B, X7$C, X7$D, main = "Tiempo del proceso en horas.", names = c("A","B","C","D") )  

  
# Grafico de dispersion de las variables independiente del Centro de investigacion
  datosSinCentro<-subset(datos, select = y:x7)
  pairs(datosSinCentro)
  
# Grafico de dispersion de las variables dependiendo del Centro de investigacion
  A<-subset(datosCentros$A, select = y:x7)
  B<-subset(datosCentros$B, select = y:x7)
  C<-subset(datosCentros$C, select = y:x7)
  D<-subset(datosCentros$D, select = y:x7)
  pairs(A)
  pairs(B)
  pairs(C)
  pairs(D)

# Matriz de Correlacion de las variables independientes del Centro de Investigacion
  cor(datosSinCentro)

# Matriz de Correlacion de las variables dependiento del Centro de Investigacion
  cor(A)
  cor(B)
  cor(C)
  cor(D)


#######################   FIN   ##############################
  