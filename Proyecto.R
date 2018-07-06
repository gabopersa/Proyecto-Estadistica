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
  plot(Cent, main = "Centros de Investigacion", col = c("red","yellow","orange","brown"), ylab = "Frecuencia", ylim = c(0,70))
  
# Valores calculados para las distintas variables encontradas en las muestras dependiendo del Centro de Investigacion 
  datosCentros<-split(datos, Cent)
  kable(analisis(datosCentros$A))
  kable(analisis(datosCentros$B))
  kable(analisis(datosCentros$C))
  kable(analisis(datosCentros$D))
  
# Histogramas para cada variable independiente del Centro de Investigacion
  par(mfrow =c(2,2))
  hist  (y, main = "Porcentaje de óxido de aluminio que precipita.", ylab = "Frecuencia", xlab = "Porcentaje")
  hist  (x1, main = "Concentración del ácido.", ylab = "Frecuencia", xlab = "Contracion", ylim = c(0,25))
  hist  (x2, main = "Valor final de pH de la solución en unidades de pH.", ylab = "Frecuencia", xlab = "pH")
  hist  (x3, main = "Temperatura del proceso en grados centígrados.", ylab = "Frecuencia", xlab = "Grados Centigrados", ylim = c(0,70))
  par(mfrow =c(2,2))
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
  par(mfrow = c(2,2))
  boxplot(y, main = "Porcentaje de óxido de aluminio que precipita.")
  boxplot(x1, main = "Concentración del ácido.")
  boxplot(x2, main = "Valor final de pH de la solución en unidades de pH.")
  boxplot(x3, main = "Temperatura del proceso en grados centígrados.")
  par(mfrow = c(2,2))
  boxplot(x4, main = "Concentración de la base.")
  boxplot(x5, main = "Velocidad de agitación en RPM.")
  boxplot(x6, main = "Velocidad de adición de la base en mililitros por hora.")
  boxplot(x7, main = "Tiempo del proceso en horas.")

# Diagramas de caja para cada variable dependiendo del Centro de Investigacion
  par(mfrow = c(1,1))
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
  

# Matriz de Correlacion de las variables independientes del Centro de Investigacion
  cor(datosSinCentro)


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
  
  # Analisis de Residuos
  par(mfrow = c(2,2))
  plot(m7)
  
  # Estudio de independencia
  for(i in 1:8)
  {
    par(mfrow = c(1,1))
    plot(datos[[i]], rstandard(m2), xlab = names(datos[i]), main="Estudio de Independencia")
  }
  
# Prediccion de la variable "y"
  datos2<-proceso_pre
  attach(datos2)
  y_mod = 0.0183364*x6 + 3.1004344*x7
  error = y - y_mod 
  error
  length(error)
  summary(error)
  sd(error)
  hist(error, main = "Errores", ylab = "Frecuencia", xlab = "Error")
  boxplot(error, main = "Error")
  

# ANOVA
  tapply(y, Cent, mean)
  boxplot(y~Cent,ylab="Y")
  anova(lm(y~Cent))
  pairwise.t.test(y,Cent)

  tapply(x1, Cent, mean)
  boxplot(x1~Cent,ylab="X1")
  anova(lm(x1~Cent))
  pairwise.t.test(x1,Cent)
  
  tapply(x2, Cent, mean)
  boxplot(x2~Cent,ylab="X2")
  anova(lm(x2~Cent))
  pairwise.t.test(x2,Cent)
  
  tapply(x3, Cent, mean)
  boxplot(x3~Cent,ylab="X3")
  anova(lm(x3~Cent))
  pairwise.t.test(x3,Cent)
  
  tapply(x4, Cent, mean)
  boxplot(x4~Cent,ylab="X4")
  anova(lm(x4~Cent))
  pairwise.t.test(x4,Cent)
  
  tapply(x5, Cent, mean)
  boxplot(x5~Cent,ylab="X5")
  anova(lm(x5~Cent))
  pairwise.t.test(x5,Cent)
  
  tapply(x6, Cent, mean)
  boxplot(x6~Cent,ylab="X6")
  anova(lm(x6~Cent))
  pairwise.t.test(x6,Cent)
  
  tapply(x7, Cent, mean)
  boxplot(x7~Cent,ylab="X7")
  anova(lm(x7~Cent))
  pairwise.t.test(x7,Cent)
  
#######################   FIN   #############################