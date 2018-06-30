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

# Prueba de bondad de ajuste para determinar si la variable “y” se distribuye en forma normal.
  
  # Graficos de normalidad
  par(mfrow=c(1,3))
  hist(y, xlab="Porcentaje de óxido de aluminio que precipita.", ylab="Frecuencia", las=1, main="")
  plot(density(y), xlab="Porcentaje de óxido de aluminio que precipita.", ylab="Densidad", las=1, main="")
  qqnorm(y, xlab="Cuantiles teoricos", ylab="Cuantiles muestrales", las=1,main="")
  qqline(y)
  
  # Calculamos la tabla de frecuencia de la variable y (porcentaje de oxido que precipita) de la siguiente forma
  frecuency = hist(y,plot=F)
  
  # Luego, la prueba de bondad de ajuste es la siguiente
  fi = frecuency$counts
  mi = frecuency$mids
  xbarra = mean(y)
  S = sd(y)
  k = length(fi)
  n = sum(fi)
  r = 2 # se pierden dos gl por el calculo de la media y la varianza
  pi = pnorm(frecuency$breaks[1:k+1],xbarra,S) - pnorm(frecuency$breaks[1:k],xbarra,S)
  chi_2 = sum((fi-n*pi)^2/(n*pi))
  alpha = 0.05
  chi_2_alpha = qchisq(1-alpha,k-1-r)
  p_valor = pchisq(chi_2,k-1-r)
  

# Intervalo de confianza del 93% para cada variable.
  
  # Independientes del Centro de Investigacion
  t.test(datos[1], conf.level = 0.93)  
  t.test(datos[2], conf.level = 0.93)  
  t.test(datos[3], conf.level = 0.93)  
  t.test(datos[4], conf.level = 0.93)  
  t.test(datos[5], conf.level = 0.93)  
  t.test(datos[6], conf.level = 0.93)  
  t.test(datos[7], conf.level = 0.93)  
  t.test(datos[8], conf.level = 0.93)
  
  # Centro de Investigacion A
  t.test(A[1], conf.level = 0.93)  
  t.test(A[2], conf.level = 0.93)  
  t.test(A[3], conf.level = 0.93)  
  t.test(A[4], conf.level = 0.93)  
  t.test(A[5], conf.level = 0.93)  
  t.test(A[6], conf.level = 0.93)  
  t.test(A[7], conf.level = 0.93)  
  t.test(A[8], conf.level = 0.93)
  
  # Centro de Investigacion B
  t.test(B[1], conf.level = 0.93)  
  t.test(B[2], conf.level = 0.93)  
  t.test(B[3], conf.level = 0.93)  
  t.test(B[4], conf.level = 0.93)  
  t.test(B[5], conf.level = 0.93)  
  t.test(B[6], conf.level = 0.93)  
  t.test(B[7], conf.level = 0.93)  
  t.test(B[8], conf.level = 0.93)
  
  # Centro de Investigacion C
  t.test(C[1], conf.level = 0.93)  
  t.test(C[2], conf.level = 0.93)  
  t.test(C[3], conf.level = 0.93)  
  t.test(C[4], conf.level = 0.93)  
  t.test(C[5], conf.level = 0.93)  
  t.test(C[6], conf.level = 0.93)  
  t.test(C[7], conf.level = 0.93)  
  t.test(C[8], conf.level = 0.93)
  
  # Centro de Investigacion D
  t.test(D[1], conf.level = 0.93)  
  t.test(D[2], conf.level = 0.93)  
  t.test(D[3], conf.level = 0.93)  
  t.test(D[4], conf.level = 0.93)  
  t.test(D[5], conf.level = 0.93)  
  t.test(D[6], conf.level = 0.93)  
  t.test(D[7], conf.level = 0.93)  
  t.test(D[8], conf.level = 0.93)


# Halle un modelo lineal que explique mejor la variable “y”.  
  
# Modelo independiente del Centro de investigacion
  # Primer modelo con todas las variables
  m1 = lm( y ~ x1 + x2 + x3 + x4 + x5 + x6 +x7)
  summary(m1)
  
  # Segundo modelo con todas las variables menos el interceptor
  m2 = lm( y ~ x1 + x2 + x3 + x4 + x5 + x6 +x7 - 1)
  summary(m2)
  
  # Tercer modelo excluyendo el interceptor y x4
  m3 = lm( y ~ x1 + x2 + x3 + x5 + x6 +x7 - 1)
  summary(m3)
  
  # Cuarto modelo excluyendo el interceptor, x4 y x1
  m4 = lm( y ~ x2 + x3 + x5 + x6 +x7 - 1)
  summary(m4)
  
  # Quinto modelo excluyendo el interceptor, x4, x1 y x3
  m5 = lm( y ~ x2 + x5 + x6 +x7 - 1)
  summary(m5)
  
  # Sexto modelo excluyendo el interceptor, x4, x1, x3 y x5
  m6 = lm( y ~ x2 + x6 +x7 - 1)
  summary(m6)
  
  # Septimo modelo excluyendo el interceptor, x4, x1, x3, x5 y x2
  m7 = lm( y ~ x6 +x7 - 1)
  summary(m7)
  
# Modelo para el Centro de investigacion A
  # Primer modelo con todas las variables
  mA1 = lm(A$y ~ A$x1 + A$x2 + A$x3 + A$x4 + A$x5 + A$x6 + A$x7)
  summary(mA1)
  
  # Segundo modelo excluyendo A$x5
  mA2 = lm(A$y ~ A$x1 + A$x2 + A$x3 + A$x4 + A$x6 + A$x7)
  summary(mA2)

  # Tercer modelo excluyendo A$x5 y A$x3
  mA3 = lm(A$y ~ A$x1 + A$x2 + A$x4 + A$x6 + A$x7)
  summary(mA3)
  
  # Cuarto modelo excluyendo A$x5, A$x3 y el interceptor
  mA4 = lm(A$y ~ A$x1 + A$x2 + A$x4 + A$x6 + A$x7 - 1)
  summary(mA4)
  
  # Quinto modelo excluyendo A$x5, A$x3, el interceptor y x1
  mA5 = lm(A$y ~ A$x2 + A$x4 + A$x6 + A$x7 - 1)
  summary(mA5)

  # Sexto modelo excluyendo A$x5, A$x3, el interceptor, A$x1, A$x2
  mA6 = lm(A$y ~ A$x4 + A$x6 + A$x7 - 1)
  summary(mA6)
  
# Modelo para el Centro de investigacion B
  # Primer modelo con todas las variables
  mB1 = lm(B$y ~ B$x1 + B$x2 + B$x3 + B$x4 + B$x5 + B$x6 + B$x7)
  summary(mB1)

  # Segundo excluyendo a B$x3
  mB2 = lm(B$y ~ B$x1 + B$x2 + B$x4 + B$x5 + B$x6 + B$x7)
  summary(mB2)
  
  # Tercer modelo excluyendo a B$x3 y a B$x2
  mB3 = lm(B$y ~ B$x1 + B$x4 + B$x5 + B$x6 + B$x7)
  summary(mB3)

  # Cuarto modelo excluyendo a B$x3, B$x2 y a B$x5
  mB4 = lm(B$y ~ B$x1 + B$x4 + B$x6 + B$x7)
  summary(mB4)
  
  # Quinto modelo excluyendo a B$x3, B$x2, B$x5 y a B$x4
  mB5 = lm(B$y ~ B$x1+ B$x6 + B$x7)
  summary(mB5)

  # Sexto modelo excluyendo a B$x3, B$x2, B$x5, B$x4 y al interceptor
  mB6 = lm(B$y ~ B$x1+ B$x6 + B$x7 - 1)
  summary(mB6)

  # Sexto modelo excluyendo a B$x3, B$x2, B$x5, B$x4, al interceptor y a B$x7
  mB7 = lm(B$y ~ B$x1+ B$x6 - 1)
  summary(mB7)

# Modelo para el Centro de investigacion C
  # Primer modelo con todas las variables
  mC1 = lm(C$y ~ C$x1 + C$x2 + C$x3 + C$x4 + C$x5 + C$x6 + C$x7)
  summary(mC1)
  
  # Segundo modelo excluyendo a C$x2
  mC2 = lm(C$y ~ C$x1 + C$x3 + C$x4 + C$x5 + C$x6 + C$x7)
  summary(mC2)
  
  # Tercer modelo excluyendo a C$x2 y a C$x3
  mC3 = lm(C$y ~ C$x1 + C$x4 + C$x5 + C$x6 + C$x7)
  summary(mC3)

  # Cuarto modelo excluyendo a C$x2, C$x3 y al interceptor
  mC4 = lm(C$y ~ C$x1 + C$x4 + C$x5 + C$x6 + C$x7 - 1)
  summary(mC4)

  # Quinto modelo excluyendo a C$x2, C$x3, al interceptor y a C$x1
  mC5 = lm(C$y ~ C$x4 + C$x5 + C$x6 + C$x7 - 1)
  summary(mC5)
  
  # Sexto modelo excluyendo a C$x2, C$x3, al interceptor, C$x1 y a C$x5
  mC6 = lm(C$y ~ C$x4 + C$x6 + C$x7 - 1)
  summary(mC6)
  
  # Septimo modelo excluyendo a C$x2, C$x3, al interceptor, C$x1, C$x5 y a C$x4
  mC7 = lm(C$y ~ C$x6 + C$x7 - 1)
  summary(mC7)
  
# Modelo para el Centro de investigacion D
  # Primer modelo con todas las variables
  mD1 = lm(D$y ~ D$x1 + D$x2 + D$x3 + D$x4 + D$x5 + D$x6 + D$x7)
  summary(mD1)
  
  # Segundo modelo excluyendo a D$x4
  mD2 = lm(D$y ~ D$x1 + D$x2 + D$x3 + D$x5 + D$x6 + D$x7)
  summary(mD2)
  
  # Tercer modelo excluyendo a D$x4 y al interceptor
  mD3 = lm(D$y ~ D$x1 + D$x2 + D$x3 + D$x5 + D$x6 + D$x7 - 1)
  summary(mD3)
  
  # Cuarto modelo excluyendo a D$x4, al interceptor y a D$x3 
  mD4 = lm(D$y ~ D$x1 + D$x2 + D$x5 + D$x6 + D$x7 - 1)
  summary(mD4)
  
  # Quinto modelo excluyendo a D$x4, al interceptor, D$x3 y a D$x1
  mD5 = lm(D$y ~ D$x2 + D$x5 + D$x6 + D$x7 - 1)
  summary(mD5)

  # Quinto modelo excluyendo a D$x4, al interceptor, D$x3, D$x1 y a D$x5
  mD6 = lm(D$y ~ D$x2 + D$x6 + D$x7 - 1)
  summary(mD6)
  
  
#######################   FIN   #############################