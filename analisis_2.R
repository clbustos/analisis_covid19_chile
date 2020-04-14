# Este archivo reúne las lecturas diarios para Chile, así como los decesos
# Reemplaza a analisis.R y analisis_regiones_chile.R
library(openxlsx)
library(forecast)
library(ggplot2)
library(nlme)
library(propagate)
library(rms)
source("funciones_soporte.R")


zonas.chile<-list('Arica.y.Parinacota'='Norte Grande' , 'Tarapacá'='Norte Grande' , 'Antofagasta'='Norte Grande' , 'Atacama'='Norte Grande' , 'Coquimbo'='Norte Chico' , 'Valparaíso'='Norte Chico' , 'Metropolitana'='Central' , 'O’Higgins'='Central' , 'Maule'='Central' , 'Ñuble'='Central' , 'Biobío'='Sur' , 'Araucanía'='Sur' , 'Los.Ríos'='Sur' , 'Los.Lagos'='Sur' , 'Aysén'='Austral' , 'Magallanes'='Austral' , 'total'='Total')

xlsx.casos<-read.xlsx("casos_chile_regiones.xlsx",sheet = 1)
xlsx.decesos<-read.xlsx("casos_chile_regiones.xlsx",sheet = 2)

leer.datos<-function(x,zonas) {

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
    x$zona=zonas[[i]]
    x
  })

}

datos.casos.total<-datos.casos<-leer.datos(xlsx.casos,zonas.chile)

datos.decesos.total<-datos.decesos<-leer.datos(xlsx.decesos,zonas.chile)
