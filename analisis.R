library(openxlsx)
library(forecast)
library(ggplot2)
source("funciones_soporte.R")

xlsx.pais<-read.xlsx("casos_pais.xlsx")

n.paises<-setdiff(colnames(xlsx.pais),c("fecha","dia"))
names(n.paises)<-n.paises
leer.pais<-function(x) {
  x<-x[!is.na(x$casos),]
  x$dia<-1:nrow(x)
  x$casos.nuevos<-c(x$casos[1], diff(x$casos))
  x
}

names(n.paises)<-n.paises
datos.pais<-lapply(n.paises,function(i) {
  x<-leer.pais(data.frame(
    fecha=xlsx.pais$fecha,
    dia  =xlsx.pais$dia,
    casos=xlsx.pais[[i]]
  ))
  x$pais<-i
  x
})



