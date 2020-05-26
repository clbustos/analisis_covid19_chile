# Este archivo reúne las lecturas diarios para Chile, así como los decesos
# Incluye datos de camas y ventiladores, por lo que reemplaza a analisis_2.R
# Reemplaza a analisis.R y analisis_regiones_chile.R
library(openxlsx)
library(forecast)
library(reshape2)
library(ggplot2)
library(nlme)
library(propagate)
library(rms)
library(gganimate)
library(ggrepel)
library(R.cache)
library (RCurl)
library(benford.analysis)
library(dynlm)
library(stlplus)

source("funciones_soporte.R")


zonas.chile<-list('Arica.y.Parinacota'='Norte' ,
                  'Tarapacá'='Norte' ,
                  'Antofagasta'='Norte' ,
                  'Atacama'='Norte' , 'Coquimbo'='Norte' , 'Valparaíso'='Central' , 'Metropolitana'='Central' , 'O’Higgins'='Central' , 'Maule'='Central' , 'Ñuble'='Central' , 'Biobío'='Sur' , 'Araucanía'='Sur' , 'Los.Ríos'='Sur' , 'Los.Lagos'='Sur' , 'Aysén'='Austral' , 'Magallanes'='Austral' , 'total'='Total')


paleta<-paleta.regiones()
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


# Para hacer la animación de tasa periodos, tengo que ir por la base

plot.tasa.casos.animado.cache<-addMemoization(plot.tasa.casos.animado)


descargar.desde.github<-function(url) {
  filename=paste0("datos.min/",basename(url),".",Sys.Date(),".csv")
  if(!file.exists(filename)) {
    texto=getURL(url)
    cat(texto,file=filename)
  }
  or<-read.csv(filename,check.names = F)
  rn<-or[,1]
  res<-data.frame(t(or[,-1]))
  colnames(res)<-rn
  f1<-rownames(res)
  res$fecha<-f1
  res$dia<-1:nrow(res)
  rownames(res)<-paste0("f:",f1)
  res
}

uri.github.min<-list(vent.diario="https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/ReporteDiario/NumeroVentiladores.csv",
hosp.uci.diario="https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/ReporteDiario/HospitalizadosUCIEtario.csv",
hosp.uci.regiones="https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/ReporteDiario/UCI.csv")



datos.ministerio.bruto<-lapply(uri.github.min, descargar.desde.github)

datos.ministerio.bruto$hosp.uci.regiones<-datos.ministerio.bruto$hosp.uci.regiones[!(datos.ministerio.bruto$hosp.uci.regiones$fecha %in% c("Codigo region", "Poblacion")),]

datos.ministerio.bruto$hosp.uci.regiones$total<-rowSums(subset(datos.ministerio.bruto$hosp.uci.regiones,select=`Arica y Parinacota`:`Magallanes y la Antartica`))

datos.hosp.uci<-melt(datos.ministerio.bruto$hosp.uci.regiones,id.vars = c("fecha","dia"))

colnames(datos.hosp.uci)<-c("fecha","dia","region","casos")
