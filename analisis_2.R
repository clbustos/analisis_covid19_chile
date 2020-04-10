# Este archivo reúne las lecturas diarios para Chile, así como los decesos
# Reemplaza a analisis.R y analisis_regiones_chile.R
library(openxlsx)
library(forecast)
library(ggplot2)
source("funciones_soporte.R")

xlsx.casos<-read.xlsx("casos_chile_regiones.xlsx",sheet = 1)
xlsx.decesos<-read.xlsx("casos_chile_regiones.xlsx",sheet = 2)

leer.datos<-function(x) {

  n.regiones<-setdiff(colnames(x),c("fecha","dia"))
  names(n.regiones)<-n.regiones

  datos.pais<-lapply(n.regiones,function(i) {
    uu<-data.frame(
      fecha=x$fecha,
      dia  =x$dia,
      casos=x[[i]]
    )
    x<-leer.pais(uu)
    x$pais<-i
    x
  })

}

datos.casos.total<-datos.casos<-leer.datos(xlsx.casos)
datos.decesos.total<-datos.decesos<-leer.datos(xlsx.decesos)
