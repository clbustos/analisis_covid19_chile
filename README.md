---
title: "Análisis de series de tiempo, usando los datos de casos para Covid-19"
author: "Claudio Bustos"
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
-   25/03/2020: https://rpubs.com/clbustos/589304
-   26/03/2020: https://rpubs.com/clbustos/590058
-   27/03/2020:  https://rpubs.com/clbustos/590727
-   28/03/2020: https://rpubs.com/clbustos/591120
-   29/03/2020: https://rpubs.com/clbustos/591626
-   30/03/2020: https://rpubs.com/clbustos/592278
-   31/03/2020: https://rpubs.com/clbustos/592810
-   01/04/2020: https://rpubs.com/clbustos/593316
-   02/04/2020: https://rpubs.com/clbustos/593866
-   03/04/2020: https://rpubs.com/clbustos/594294
-   04/04/2020: https://rpubs.com/clbustos/594787


## Informes regionales

* 22/03/2020: https://rpubs.com/clbustos/587952
* 23/03/2020: https://rpubs.com/clbustos/covid-19-23-03-2020-chile-regiones
* 24/03/2020: https://rpubs.com/clbustos/588748
* 25/03/2020: https://rpubs.com/clbustos/589302
* 26/03/2020: https://rpubs.com/clbustos/590066
* 27/03/2020: https://rpubs.com/clbustos/590726
* 28/03/2020: https://rpubs.com/clbustos/591118
* 29/03/2020: https://rpubs.com/clbustos/591628
* 30/03/2020: https://rpubs.com/clbustos/592284
* 31/03/2020: https://rpubs.com/clbustos/592808
* 01/04/2020: https://rpubs.com/clbustos/593314
* 02/04/2020: https://rpubs.com/clbustos/593865
* 03/04/2020: https://rpubs.com/clbustos/594308
* 04/04/2020: https://rpubs.com/clbustos/594793

## Pruebas del modelo predictivo:

* 27 de Marzo al 3 de Abril: https://rpubs.com/clbustos/594816

## Lecturas interesantes.


* [Projecting hospital utilization during the COVID-19 outbreaks in the United States](https://www.pnas.org/content/early/2020/04/02/2004064117)
* [Everyone Thinks They’re Right About Masks](https://www.theatlantic.com/health/archive/2020/04/coronavirus-pandemic-airborne-go-outside-masks/609235/)
* [Bidding wars are causing New York to pay up to 15 times the normal price for medical equipment](https://www.businessinsider.com/bidding-wars-driving-up-medical-equipment-prices-for-states-cities-2020-4)
* [Don't trust the psychologist on coronavirus](https://unherd.com/2020/03/dont-trust-the-psychologists-on-coronavirus/)