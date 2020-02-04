#Para ubicar el wd: setwd('C:/Users/JaqDe/Desktop/MBDA/R/datos')

#Ejercicio 1. Cupones de la ONCE. 
data1 <- runif(1000, min = 1000, max = 9999)
dataSerial <- round(data1, digits = 3)
dataFinal <- round(dataSerial, digits = 0)
is.even <- function(x) x%%2==0
dataFinal[is.even(dataFinal)]
sum(is.even(dataFinal))

#Ejercicio 2. Conjunto de chalets.
#a) Lee el fichero "chalets.txt" utilizando  la función read.table() y crea el data frame "chalets"
#El primer argumento de la función read.table corresponde a la root del archivo chalets.txt
chalets <- read.table("D:/Universidad/master/documentacion/Master BDBA/R/datos/datos/chalets.txt", quote="\"", skip = 8, header = TRUE)
df1 <- as.data.frame(chalets)
sapply(X = df1[, c("x1", "x3")],MARGIN = 2,FUN = mean)
SimFisher <- function(x){
  #Calculamos la media de la lista.
  media <- sapply(x, mean)
  #print(media)
  
  #Calculamos la desviación estándar de la lista.
  desviacionTipica <- sapply(x, sd)
  #print(desviacionTipica)
  
  #Calculamos el coeficiente de Sim. Fisher.
  n <- nrow(x)
  sumaCuadraticaSinMedia <- 0
  for (i in 1:n) 
  {
    #print(typeof(x[i,1]))
    #print(typeof(media))
    #print(typeof(x[i,1] - media))
    #print(typeof((x[i,1] - media)^2))
    sumaCuadraticaSinMedia <- sumaCuadraticaSinMedia + (x[i,1] - media)^2 
    #print(sumaCuadraticaSinMedia)
  }
  
  S3X <- (sumaCuadraticaSinMedia / (n - 1))^(3/2) 
  
  coefSimFisher <- sumaCuadraticaSinMedia / (n * S3X)
  
  #print(coefSimFisher)
  
  listaResultados <- list("media" = media, "desviacionTipica" = desviacionTipica, "coefSimFisher" = coefSimFisher)
  return(listaResultados)
  
}	


#Ejercicio 3: kens y shakus
Conversor_cm <- function(x){
  
  if (x < 0) 
    stop("Número introducido menor que cero.")
  
  shaku <- x/30.3
  ken <- 6*(shaku)
  listaResultados <- list("shaku" = round(shaku, digits =2), "ken" = round(ken, digits = 2))
  
  return(listaResultados)
}

# Ejercicio 4: valores y hoteles.
#a)
Resultados <- c(7000,3000, 5000, 4000, 5000, 7000, 4000, 7500, 8000, 5000, 5000, 500, 3000, 7000, 10000, 15000, 5000, 7500, 12000, 8000, 4000, 5000, 3000, 5000, 10000, 3000, 4000, 5000, 7000, 5000, 3000, 4000, 7000, 4000, 7000, 5000, 4000, 7000, 10000, 7500, 7000, 8000, 7500, 7000, 7500, 8000, 7000, 7000, 12000, 8000)
hist(Resultados)
#b)
intervaloInicial <- min(Resultados)
intervaloFinal   <- max(Resultados)
numeroPasos      <- 5
paso             <- (intervaloFinal - intervaloInicial) / numeroPasos
grupos <- seq(from = intervaloInicial, to = intervaloFinal, by = paso)
hist(x = Resultados, breaks = grupos)

#Ejercicio 5: Empleados
# a) Crear un fichero permanente 'Empleados.RData'
  #Primero leemos el fichero de Empleados.
  Empleados <- read.delim("D:/Universidad/master/documentacion/Master BDBA/R/datos/Empleados.dat")
  #después lo guardamos.
  save(Empleados, file = "Empleados.RData")
# b) Selecciona las 10 primeras filas de la variable "salini"
  names(Empleados) <- c('id', 'sexo', 'fechanac', 'educ', 'catlab', 'salario', 'salini', 'tiempopemp', 'expprev')
  salini <- Empleados[,"salini"]
  salini[1:10]

# c)Selecciona las filas 4 y 8 de la variable "salario"
  Empleados$salario[c(4, 8)]
# d)Seleccione el elemento 3 de la columna 4
  Empleados[3,4]
# e) Seleccione las columnas 1 y 4 de dos formas distinta. ¿Los objetos son los mismos?
  
 a <- Empleados[c("id", "educ")]
 b <- Empleados[,c(1,4)]
 class(a)
 class(b)
 #son el mismo objeto
# f)Seleccione los casos que verifican 23000 < salario < 25500 y educ > 7.
subset <- subset.data.frame(Empleados, Empleados$salario > 23000 
                            & Empleados$salario < 25000
                            & Empleados$educ > 7)
# g)Seleccione los casos con valores en todas las variables (donde tenga sentido) por encima del correspondiente tercer cuartil (????3).
subset_qts <- subset.data.frame(Empleados, Empleados$salario > quantile(Empleados$salario, probs = 0.75) 
                                & Empleados$salini > quantile(Empleados$salini,probs = 0.75)
                                & Empleados$educ > quantile(Empleados$educ, probs = 0.75)
                                & Empleados$tiempopemp > quantile(Empleados$tiempopemp, probs = 0.75)
                                & Empleados$expprev > quantile(Empleados$expprev, probs = 0.75)
)

#6) plots
ozono <- airquality[,"Ozone"]
solar <- airquality[, "Solar.R"]
wind  <- airquality[, "Wind"]
temp  <- airquality[, "Temp"]

plotRange  <- range(c(solar, wind, temp, ozono), na.rm = TRUE)
plotColors <- c("red", "blue", "pink", "orange")

#El tamaño de la ventana original de RStudio es demasiado pequeña y no muestra correctamente el plot
windows(width=16, height=9)
plot(ozono, type = "l", col = "red", lty = 1, ylim = plotRange, main = "airquality", ylab = "dat", xlab = "")
par(new = TRUE)
plot(solar, type = "l", col = "blue", lty = 2, ylim = plotRange, ylab = "", xlab = "")
par(new = TRUE)
plot(wind, type = "l", col = "pink", lty = 3, ylim = plotRange, ylab = "", xlab = "")
par(new = TRUE)
plot(temp, type = "l", col = "orange", lty = 4, ylim = plotRange, ylab = "", xlab = "")


#legend('topright', legend=c("Ozone", "Solar", "Wind", "Temp"),
#     col=c("red", "blue", "pink", "orange"), lty=1:4, cex=0.8, bty = "n")

legend("topright", legend=c("Ozone", "Solar", "Wind", "Temp"),
       col=plotColors, fill = plotColors, cex=0.75, bty = "n", xjust = 1, yjust = 1)

#c) cajas y bigotes.
boxplot(Temp~Month, data=airquality, main="Cajas y bigotes", xlab="Meses", ylab="Temperatura", col=rainbow(5))
