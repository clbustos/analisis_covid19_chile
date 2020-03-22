library(openxlsx)
library(forecast)
library(ggplot2)
source("funciones_soporte.R")

xlsx.regiones<-read.xlsx("casos_chile_regiones.xlsx")

n.regiones<-setdiff(colnames(xlsx.regiones),c("fecha","dia"))
names(n.regiones)<-n.regiones
leer.region<-function(x) {
  x<-x[!is.na(x$casos),]
  x$dia<-1:nrow(x)
  x$casos.nuevos<-c(x$casos[1], diff(x$casos))
  x
}

datos.pais<-lapply(n.regiones,function(i) {
  x<-leer.region(data.frame(
    fecha=xlsx.regiones$fecha,
    dia  =xlsx.regiones$dia,
    casos=xlsx.regiones[[i]]
  ))
  x$pais<-i
  x
})

# Eliminemos las regiones que tienen menos de 10 casos

for(i in n.regiones) {
  casos.ultimo<-tail(datos.pais[[i]],1)$casos
  if(casos.ultimo<20) {
    datos.pais[[i]]<-NULL
  }
}

datos.pais[["total"]]<-NULL
