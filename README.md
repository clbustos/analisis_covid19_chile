---
title: Análisis de series de tiempo, usando los datos de casos para Covid-19
author: Claudio Bustos
---

Se presentan ejemplos de visualización de series de tiempo, usando los datos para Covid-19, en R. Entre ellos, está:

- Comparación entre datos en bruto y aplicación de escala logarítmica
- Presentación de tasas de cambio
- Predicción de valores futuros, usando regresión exponencial, ARIMA con deriva, así como una combinación de análisis de tendencia, más cálculo del modelo AR(1) para sus residuos.


**NOTA**: Lo que presento aquí es un ejemplo de como realizar análisis de series de tiempo, usando datos de la evolución total de casos disponible en fuentes públicas de internet. No tiene ninguna validez epidemiológica, ya que eso requería un modelo de la diseminación de la enfermedad. En particular, las predicciones son solo válidas en cuanto no existan cambios en las políticas públicas; si los hay, se esperaría que (ojalá) las predicciones fuesen incorrectas.

La licencia de uso del código fuente es MIT. Revise el archivo LICENSE para más información.

## Informes comparativos de Chile vs Italia, España, Corea del Sur y Brasil

-   20/03/2020: https://rpubs.com/clbustos/587389
-   21/03/2020: https://rpubs.com/clbustos/587595
-   22/03/2020: https://rpubs.com/clbustos/587947
-   23/03/2020: https://rpubs.com/clbustos/588293
-   24/03/2020: https://rpubs.com/clbustos/covid-19-24-03-2020-chile
    
## Informes regionales

* 22/03/2020: https://rpubs.com/clbustos/587952
* 23/03/2020: https://rpubs.com/clbustos/covid-19-23-03-2020-chile-regiones
* 24/03/2020: https://rpubs.com/clbustos/588748