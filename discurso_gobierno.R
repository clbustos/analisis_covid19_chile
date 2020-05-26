source("analisis_3.R")
textos<-read.xlsx("casos_chile_regiones.xlsx",sheet="discurso")
textos$Fecha<-openxlsx::convertToDate(textos$Fecha)
textos$y<-NA

dl.1<-data.frame(fecha=openxlsx::convertToDate(datos.casos$total$fecha), casos.nuevos=datos.casos$total$casos.nuevos, textos="", autor="--",stringsAsFactors = F)

for(i in 1:nrow(textos)) {
  asign<-which(dl.1$fecha==textos[i,"Fecha"])
  print(asign)
  #print(asign)
  #print(textos[i,"Titulo"])
  textos[i,"y"]<-dl.1$casos.nuevos[asign]
  dl.1$autor[asign]<-textos[i,"Quien"]
#  dl.1$textos[asign]<-textos[i,"Titulo"]
}



