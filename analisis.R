library(openxlsx)
library(forecast)
library(ggplot2)
source("funciones_soporte.R")

xlsx.pais<-read.xlsx("casos_pais.xlsx")

n.paises<-setdiff(colnames(xlsx.pais),c("fecha","dia"))
names(n.paises)<-n.paises

# El algoritmo del país debe ser capaz de detectar cuando hay datos faltantes e interpolar.

names(n.paises)<-n.paises
datos.pais<-lapply(n.paises,function(i) {
  uu<-data.frame(
    fecha=xlsx.pais$fecha,
    dia  =xlsx.pais$dia,
    casos=xlsx.pais[[i]]
  )
  x<-leer.pais(uu)
  x$pais<-i
  x
})



