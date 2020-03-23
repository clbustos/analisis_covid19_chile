# Como usted se está dando la lata de leer el código
# Le cuento que todo este ejercicio está hecho para predecir en qué día se nos acaban las camas
# críticas. Las tasas de pacientes críticos están en el paper
# "Characteristics of and Important Lessons From the Coronavirus Disease 2019 (COVID-19) Outbreak in China"
# que todos ocupan para las cifras que se esperan de hospitalizados
# la cifra de 860 corresponde al número de camas críticas disponibles
# disponibles de las totales (20% de 1800 de adultos) más
# 500 que pueden ser reconvertidas.
# Debería cargar primero el archivo analisis.R para que corra bien.

predecir.dia.critico.chile<-function() {
  lm.1<-lm(log(casos)~dia, data=datos.pais$Chile)
  prediccion.40<-predict(lm.1,newdata=data.frame(dia=1:40))
  distribucion<-data.frame(
    moderado=exp(prediccion.40)*0.81,
    severa=exp(prediccion.40)*0.14,
    critico=exp(prediccion.40)*0.05)

  which(distribucion$critico>860)

}

#' Permite graficar el avance por país/zona
#'
#' @param x base de datos, revisar analisis.R para ver ejemplo
#' @param min.casos número mínimo de casos a considerar, desde el cual se grafica
#' @param span.param parámetro de suavizado de curva
#' @param log.param  si se presenta o no en escala logaritmica el eje Y
#' @param predicted si se presenta o no la curva exponencial predicha.

plot.avance.pais<-function(x, min.casos=5, span.param=0.40, log.param=T, predicted=TRUE) {

  x<-lapply(x,function(x) {
    x<-x[x$casos>=min.casos,]
    x$dia<-1:nrow(x)
    lm.1<-lm(log(casos)~dia,x)
    x$pr<-exp(predict(lm.1))
    x
  })
  lm.paises<-sapply(x,function(x) {
    lm.1<-lm(log(casos)~dia,x)
    as.numeric(round(100*(exp(coef(lm.1)[2])-1),1))
  })
  # Creamos modelos

  texto.nota<-paste0(names(lm.paises),": ", as.numeric(lm.paises),"%",collapse=". ")

  unido<-do.call(rbind,x )

  gg<-ggplot(unido, aes(x=dia, y=casos, color=pais))+geom_point()

  #+annotate("text",x=5,y=10000,label="hola")
  if(log.param) {
    gg<-gg+scale_y_continuous(trans="log10")
  }
  if(predicted) {
    gg<-gg+geom_line(aes(x=dia,y=pr),alpha=0.75)
    gg<-gg+labs(caption=texto.nota)

  }
  if (!is.null(span.param)) {
    gg<-gg+geom_smooth(span=span.param)
  }

  gg
}

#' Grafica la tasa de casos
#'
#' @param  x base de datos, revisar analisis.R para ver ejemplo
#' @param min.casos mínimo número de casos válidos desde el cual se construye la curva
#' @param span.param parámetro de suavizado de la curva
#' @param ventana ventana de días para realizar media móvil.

plot.tasa.casos<-function(x, min.casos=5, span.param=NULL, ventana=3) {
  x<-lapply(x,function(x) {
    x<-x[x$casos>=min.casos,]
    dif.log.casos<-c(0,diff(log(x$casos)))
    roll.mean<-zoo::rollmean(x=dif.log.casos,k=ventana)
    data.frame(dia=x$dia[-(1:(ventana-1))], dif.log.casos=roll.mean, pais=x$pais[1])
    #x$dia<-0:(nrow(x)-1)
    #x<-x[-1,]
    #x
  })
  unido<-do.call(rbind,x )
  gg<-ggplot(unido, aes(x=dia, y=exp(dif.log.casos), color=pais))+geom_point()+ylab("casos dia n / dia n-1")+geom_line()
  if(!is.null(span.param)) {
    gg<-gg+geom_smooth(span=span.param)
  }

  gg
}

prediccion.casos<-function(xx, min.casos=5,n.ahead=7) {
  x.exp<-lapply(xx,function(x) {
    x<-x[x$casos>=min.casos,]
    lm.1<-lm(log(casos)~dia,x)
    ult.dia<-x$dia[length(x$dia)]
    pr<-predict(lm.1, newdata=data.frame(dia= (ult.dia+1):(ult.dia+7)  ),interval="prediction")
    prediccion<-data.frame(exp(pr))
    data.frame(
      dia=c(x$dia, (ult.dia+1):(ult.dia+7)),
      casos=c(x$casos, prediccion$fit),
      li=c(x$casos, prediccion$lwr),
      ls=c(x$casos, prediccion$upr),
      tipo=c(rep("obs", nrow(x)), rep("exponencial", nrow(prediccion))),
      pais=x$pais[1]
    )
  })


  x.arima<-lapply(xx,function(x) {
    x<-x[x$casos>=min.casos,]
    ult.dia<-x$dia[length(x$dia)]
    aa<-forecast::Arima(log(x$casos), c(1,1,0), include.constant = T)

    pr.resid.0<-forecast(aa,h=n.ahead,level=95)

    prediccion.casos.nuevos<-data.frame(
      fit=exp(pr.resid.0$mean),
      lwr=as.numeric(exp(pr.resid.0$lower)),
      upr=as.numeric(exp(pr.resid.0$upper))
    )

    data.frame(
      dia=c(x$dia, (ult.dia+1):(ult.dia+7)),
      casos=c(x$casos, prediccion.casos.nuevos$fit),
      li=c(x$casos, prediccion.casos.nuevos$lwr),
      ls=c(x$casos, prediccion.casos.nuevos$upr),
      tipo=c(rep("obs", nrow(x)), rep("ARIMA(1,1,0)+drift", nrow(prediccion.casos.nuevos))),
      pais=x$pais[1]
    )
  })

  # Ya que es una cuenta, mejor modelo los casos predichos usando Poisson (imposible que sea negativo)
  x.tar1<-lapply(xx,function(x) {
    x<-x[x$casos>=min.casos,]
    ult.dia<-x$dia[length(x$dia)]
    #log.casos<-diff(log(x$casos))
    d.casos<-diff(x$casos)
    d.casos[d.casos==0]<-1
    log.diff<-log(d.casos)
    dias.dif<-1:length(log.diff)
    lm.parc<-lm(log.diff~poly(dias.dif,2))
    #lm.parc<-glm(d.casos~poly(dias.dif,2), family = "poisson")
    #print(exp(predict(lm.parc)))
    #print(predict(glm.parc,type="response"))
    aa<-arima(resid(lm.parc),c(1,0,0))

    # Debemos predecir el trend, mas el residuo
    pr.trend.0<-predict(lm.parc, newdata=data.frame(dias.dif=(length(log.diff)+1):(length(log.diff)+n.ahead)),se.fit=T)
    pr.resid.0<-predict(aa,n.ahead=n.ahead)
    prediccion.casos.nuevos<-data.frame(
      fit=exp(pr.resid.0$pred+pr.trend.0$fit),
      lwr=exp(pr.resid.0$pred+pr.trend.0$fit  - 1.96*(pr.resid.0$se+pr.trend.0$se.fit)),
      upr=exp(pr.resid.0$pred+pr.trend.0$fit  + 1.96*(pr.resid.0$se+pr.trend.0$se.fit))
    )
    data.frame(
      dia=c(x$dia, (ult.dia+1):(ult.dia+7)),
      casos=c(x$casos, tail(x$casos,1)+cumsum(prediccion.casos.nuevos$fit)),
      li=c(x$casos, tail(x$casos,1)+cumsum(prediccion.casos.nuevos$lwr)),
      ls=c(x$casos, tail(x$casos,1)+cumsum(prediccion.casos.nuevos$upr)),
      tipo=c(rep("obs", nrow(x)), rep("T+AR(1)", nrow(prediccion.casos.nuevos))),
      pais=x$pais[1]
    )

  })
  res<-list(exp=x.exp,
            tar1=x.tar1,
            arima=x.arima
  )
  class(res)<-"prediccion.casos"
  res
}


plot.prediccion.casos<-function(x) {
  todo.unido<-rbind(do.call(rbind,x$exp),
                    do.call(rbind,x$arima),
                    do.call(rbind,x$tar1))
  todo.unido<-unique(todo.unido)
  ggplot(todo.unido, aes(x=dia, y=casos, ymin=li,ymax=ls,color=tipo))+
    geom_point(size=0.5)+
    geom_line(size=2)+
    geom_ribbon(alpha=0.1)+
    facet_wrap(~pais)+
    scale_y_continuous(trans="log10")
}

formatear_tablas<-function(x, nombre) {
  library(pander)
  for(i in names(x)) {
    pandoc.table(x[[i]], paste0(nombre, " : ", i))
  }
}
