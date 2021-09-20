set.seed(2)

library(xts)
library(tidyverse)
library(lubridate)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(nlme)

# NUEVA BD SIN LOS ÚLTIMOS 50 DATOS

db <- serie$V1[1:(length(serie$V1) - 50),]

# METODOLOGÍA BOX-JENKINS

serie.ts <- ts(db, start =c(1,01), frequency = 1)
plot(serie.ts) # El gráfico contiene tendencia , por tanto, no es estacionaria.

# Análisis de Autocorrelación

acf(serie.ts, main = "Función de Autocorrelación Simple", sub = "") 
# Comentario: La autocorrelación sigue disminuyendo a medida que aumenta el retraso, lo que confirma que no existe una asociación lineal entre las observaciones separadas por retrasos más grandes.
pacf(serie.ts, main = "Función de Autocorrelación Parcial", sub = "")

# Prueba Dickey-Fuller

# H0: La serie es no estacionaria: Tiene raíz unitaria
# H1: La serie es estacionaria: No tiene raíz unitaria

adfTest(serie.ts, alternative = c("stationary", "explosive"), k = trunc((length(serie.ts) - 1) ^ (1/3)))
# Comentario: La hipótesis nula no se rechaza

# Número de veces que debemos transformar la serie para que sea estacionaria
num_process <- ndiffs(serie.ts)
num_process

# Tomando la primera diferencia
new_serie <- diff(serie.ts)
plot(new_serie, main = "Serie en Primera Diferencia", sub = "", xlab = "Tiempo", ylab = "Primera Diferencia")
# Comentario: Vemos que la serie se estabiliza en torno a un valor medio

acf(new_serie, main = "AFC en Primera Diferencia", sub = "")
pacf(new_serie, main = "PACF en Primera Diferencia", sub = "")

# Ajuste del Modelo

# Modelo 1
arima1 <- arima(serie.ts, c(2, 1, 1), method = "ML")
arima1

coeftest(arima1)

confint(arima1)

# Modelo 2
arima2 <- arima(serie.ts, c(1, 1, 2), method = "ML")
arima2

coeftest(arima2)

confint(arima2)

# Validación del modelo
plot(arima1$residuals, main = "Residuos del Modelo ARIMA(2, 1, 1)", xlab = "Tiempo", ylab = "Residuos")

# Autocorrelación de los residuos
acf(arima1$residuals, main = "Autocorrelaciones de los Residuos del Modelo ARIMA(2, 1, 1)", xlab = "Tiempo", ylab = "Autocorrelación")
pacf(arima1$residuals, main = "Autocorrelaciones Parciales de los Residuos del Modelo ARIMA(2, 1, 1)", xlab = "Tiempo", ylab = "Autocorrelación")

# Normalidad de los residuos
qqnorm(arima1$residuals, main = "Gráfico Q para evaluar la Normalidad")
qqline(arima1$residuals)
# Comentario: En el gráfico, los puntos parecen seguir la línea recta bastante cerca. Por lo tanto no nos llevará a rechazar la normalidad de los términos de error en este modelo.

shapiro.test(arima1$residuals)

# Pronósticos
predict(arima1, n.head = 5)
plot(forecast(auto.arima(serie.ts)), main = "Valores Pronosticados")
