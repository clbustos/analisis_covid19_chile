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

  gg<-ggplot(unido, aes(x=dia, y=casos, color=pais))+geom_point(alpha=0.5)

  #+annotate("text",x=5,y=10000,label="hola")
  if(log.param) {
    gg<-gg+scale_y_continuous(trans="log10")
  }
  if(predicted) {
    gg<-gg+geom_line(aes(x=dia,y=pr),alpha=0.75)
    gg<-gg+labs(caption=texto.nota)

  }
  if (!is.null(span.param)) {
    gg<-gg+geom_smooth(span=span.param, alpha=0.75,se = FALSE)
  }

  gg
}

#' Grafica la tasa de casos
#'
#' @param  x base de datos, revisar analisis.R para ver ejemplo
#' @param min.casos mínimo número de casos válidos desde el cual se construye la curva
#' @param span.param parámetro de suavizado de la curva
#' @param ventana ventana de días para realizar media móvil.

plot.tasa.casos<-function(x, min.casos=5, span.param=NULL, ventana=3, derivada.2=FALSE) {
  if(derivada.2) {
    ylabt="tasa dia n / tasa dia n-1"
  } else {
    ylabt="casos dia n / casos dia n-1"
  }
  x<-lapply(x,function(x) {
    x<-x[x$casos>=min.casos,]
    dif.log.casos<-c(0,diff(log(x$casos)))

    if(derivada.2) {

      roll.mean<-zoo::rollmean(x=c(0,diff(dif.log.casos)),k=ventana)
      data.frame(dia=x$dia[-(1:(ventana-1))], dif.log.casos=roll.mean, pais=x$pais[1])
    } else {

      roll.mean<-zoo::rollmean(x=dif.log.casos,k=ventana)
      data.frame(dia=x$dia[-(1:(ventana-1))], dif.log.casos=roll.mean, pais=x$pais[1])
    }
    #x$dia<-0:(nrow(x)-1)
    #x<-x[-1,]
    #x
  })
  unido<-do.call(rbind,x )
  gg<-ggplot(unido, aes(x=dia, y=exp(dif.log.casos), color=pais))+geom_point()+ylab(ylabt)+geom_line()
  if(!is.null(span.param)) {
    gg<-gg+geom_smooth(span=span.param)
  }

  gg
}

prediccion.casos<-function(xx, min.casos=5,n.ahead=7, modelos=c("exp","arima","tar1","tar2","tar4")) {

  generar.nuevo.forecast<-function(x,prediccion, nombre) {
    ult.dia<-tail(x$dia,1)
    prediccion.casos.nuevos<-data.frame(
      fit=exp(prediccion$mean),
      lwr=as.numeric(exp(prediccion$lower)),
      upr=as.numeric(exp(prediccion$upper))
    )

    data.frame(
      dia=c(x$dia, (ult.dia+1):(ult.dia+7)),
      casos=c(x$casos, prediccion.casos.nuevos$fit),
      li=c(x$casos, prediccion.casos.nuevos$lwr),
      ls=c(x$casos, prediccion.casos.nuevos$upr),
      tipo=c(rep("obs", nrow(x)), rep(nombre, nrow(prediccion.casos.nuevos))),
      pais=x$pais[1]
    )
  }

  x.exp<-function() {lapply(xx,function(x) {
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
      tipo=c(rep("obs", nrow(x)), rep("General: Exponencial", nrow(prediccion))),
      pais=x$pais[1]
    )
  })
  }



  x.arima<-function() {lapply(xx,function(x) {
    x<-x[x$casos>=min.casos,]
    ult.dia<-x$dia[length(x$dia)]
    aa<-forecast::Arima(log(x$casos), c(1,1,0), include.constant = T)
    prediccion<-forecast(aa,h=n.ahead,level=95)
    generar.nuevo.forecast(x = x,prediccion = prediccion,nombre="General: ARIMA(1,1,0)+drift")
  })
  }




  x.tar2<-function() {lapply(xx,function(x) {
    x<-x[x$casos>=min.casos,]
    ult.dia<-x$dia[length(x$dia)]
    d.casos<-diff(x$casos)
    d.casos[d.casos==0]<-1
    log.diff<-log(d.casos)
    dias.dif<-1:length(log.diff)
    dias.dif2<-dias.dif^2

    dias.dif.ahead<-(length(log.diff)+1):(length(log.diff)+n.ahead)
    dias.dif.ahead2<-dias.dif.ahead^2

    art2<-Arima(log.diff,order = c(1,0,0), xreg = cbind(d1=dias.dif,d2=dias.dif2))
    #print(art2)
     #   print(acf(resid(art2)))
    prediccion<-forecast(art2,h=n.ahead,level=95,xreg=cbind(d1=dias.dif.ahead, d2=dias.dif.ahead2))
    ultimo.caso<-tail(x$casos,1)

    data.frame(
      dia=c(x$dia, (ult.dia+1):(ult.dia+7)),
      casos=c(x$casos, ultimo.caso+cumsum(exp(prediccion$mean))),
      li=c(x$casos, ultimo.caso+cumsum(exp(prediccion$lower))),
      ls=c(x$casos, ultimo.caso+cumsum(exp(prediccion$upper))),
      tipo=c(rep("obs", nrow(x)), rep("Casos nuevo: Tendencia + AR(1)", length(prediccion$mean))),
      pais=x$pais[1]
    )



  })
  }

  x.tar4<-function() {lapply(xx,function(x) {
    x<-x[x$casos>=min.casos,]
    ult.dia<-x$dia[length(x$dia)]
    d.casos<-diff(x$casos)
    d.casos[d.casos==0]<-1
    log.diff<-log(d.casos)
    dias.dif<-1:length(log.diff)
    dias.dif2<-dias.dif^2

    dias.dif.ahead<-(length(log.diff)+1):(length(log.diff)+n.ahead)
    dias.dif.ahead2<-dias.dif.ahead^2

    art2<-Arima(log.diff,order = c(4,0,0), xreg = cbind(d1=dias.dif,d2=dias.dif2))
   # print(art2)
  #  print(acf(resid(art2)))
    prediccion<-forecast(art2,h=n.ahead,level=95,xreg=cbind(d1=dias.dif.ahead, d2=dias.dif.ahead2))
    ultimo.caso<-tail(x$casos,1)

    data.frame(
      dia=c(x$dia, (ult.dia+1):(ult.dia+7)),
      casos=c(x$casos, ultimo.caso+cumsum(exp(prediccion$mean))),
      li=c(x$casos, ultimo.caso+cumsum(exp(prediccion$lower))),
      ls=c(x$casos, ultimo.caso+cumsum(exp(prediccion$upper))),
      tipo=c(rep("obs", nrow(x)), rep("Casos nuevo: Tendencia + AR(4)", length(prediccion$mean))),
      pais=x$pais[1]
    )



  })
  }

  x.tar1<-function() {lapply(xx,function(x) {
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
  }



  mod.a.func<-list(exp=x.exp,
            tar1=x.tar1,
            arima=x.arima,
            tar2=x.tar2,
            tar4=x.tar4
  )
  res<-list()

  for(i in modelos) {
    res[[i]]<-do.call(mod.a.func[[i]], list())
  }

  class(res)<-"prediccion.casos"
  res
}


plot.prediccion.casos<-function(x) {
  x2<-lapply(x,function(xx) {do.call(rbind,xx)})
  todo.unido<-do.call(rbind, x2)
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

#' Lee los datos en bruto, elimina los datos faltantes e interpola huecos
#'
#' @param data.frame, con tres campos: fecha, dia, casos
#'
#' @return data.frame
#' @export
#'
leer.pais<-function(x) {
  n<-nrow(x)
  primero<-match(FALSE,is.na(x$casos))
  ultimo<-n-match(FALSE,rev(is.na(x$casos)))+1
  x2<-x[primero:ultimo,]
  # Interpolamos los casos faltantes
  if(sum(is.na(x2$casos))>0) {

    faltantes<-which(is.na(x2$casos))
    interpolaciones<-round(exp(na.interp(log(x2$casos))))
    x2$casos[faltantes]<-interpolaciones[faltantes]
  }
  x2$dia<-1:nrow(x2)
  x2$casos.nuevos<-c(x2$casos[1], diff(x2$casos))
  x2
}


resumen.lm<-function(x) {
  s1<-summary(x)
  rr<-list(
    r2=sprintf("R²: %0.3f , adj R²: %0.3f ",s1$r.squared, s1$adj.r.squared),
    f=sprintf("F(%d,%d)=%0.3f, p=%s",s1$fstatistic["numdf"], s1$fstatistic["dendf"], s1$fstatistic["value"], format.pval(pf(s1$fstatistic["numdf"], s1$fstatistic["dendf"], s1$fstatistic["value"],lower.tail = F)))
  )
  class(rr)<-"resumen.lm"
  rr
}
print.resumen.lm<-function(x) {
  cat(x$r2)
  cat("\n")
  cat(x$f)
}
