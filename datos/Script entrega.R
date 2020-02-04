#Para ubicar el wd: setwd('C:/Users/JaqDe/Desktop/MBDA/R/datos')

#Ejercicio 1. Cupones de la ONCE. 
data1 <- runif(1000, min = 999, max = 9999)
dataserial <- round(data1, digits = 3)
datafinal <- round(dataserial, digits = 0)
is.even <- function(x) x%%2==0
datafinal[is.even(datafinal)]
sum(is.even(datafinal))

#Ejercicio 2. Trabajando con chalets.
chalets <- read.table("C:/Users/JaqDe/Desktop/MBDA/R/datos/chalets.txt", header=TRUE, quote="\"")
df <- as.data.frame(chalets)
apply(X = df,MARGIN = 2,FUN = mean)
apply(df,2,mean)
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

#Ejercicio 5
