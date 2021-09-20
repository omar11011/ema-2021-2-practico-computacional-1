set.seed(100)

attach(serie)

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

serie6<-ts(serie,start =c(1,01), frequency = 1)

"ANALISIS DE ESTACIONARIDAD"
"en niveles"
plot(serie6)
lines(serie6)
abline(h = mean(serie6), col = "blue")
par(mfrow=c(1,2))

acf((serie6), lag.max =20, plot =F)
acf((serie6), lag.max =20, plot =T)
pacf((serie6), lag.max =20, plot =F)
pacf((serie6), lag.max =20, plot =T)

"primera difeencia"
dserie6=diff(serie6)
plot(dserie6)
lines(dserie6)
abline(h = mean(dserie6), col = "blue")

acf(diff(serie6), lag.max =20, plot =F)
acf(diff(serie6), lag.max =20, plot =T)

pacf(diff(serie6), lag.max =20, plot =F)
pacf(diff(serie6), lag.max =20, plot =T)
df.test((diff(data)), alternative="stationary", k=0)
adf.test((diff(dserie6)), alternative="stationary", k=0)

diff(data)
"--------------------------------------------------------------------------"

"INCISO B"

"primero se transforma la serie data a su primera diferencia"

ddata=diff(data)

"luego usamos los comandos siguientes, los cuales son la formual de la funcion de autocorrelacion simple"
veccorrel <- rep(0, 20)
n <- length(ddata)
for (k in 0:20) {
  veccorrel[k + 1] <- sum((ddata[1:(n - k)] - mean(ddata)) * (ddata[(1 + k):n] - mean(ddata)))/(sum((ddata - mean(ddata))^2))
}

veccorrel


vector6 <- rep(0, 20)
n <- length(dserie6)
for (k in 0:20) {
  vector6[k + 1] <- sum((dserie6[1:(n - k)] - mean(deserie6)) * (deserie6[(1 + k):n] - mean(deserie6)))/(sum((deserie6 - mean(deserie6))^2))
}
vector6


"para poder graficar"
plot(vector6)
plot(vector6,type='h', main = "ACF - DIFERENCIA DE LA SERIE 6",
     xlab = "Lag", ylab = "ACF", ylim=c(-1,+1)); abline(h=0)
abline(h = 0, col = "red")
abline(h = 0.15, col = "blue")
abline(h = -0.15, col = "blue")


"INCISO C"

install.packages("fUnitRoots")
library("fUnitRoots")
urkpssTest(serie6, type = c("tau"), lags = c("short"),use.lag = 20, doplot = TRUE)



"INCISO D"
"modelo1"
arima1<- arima(dserie6,order=c(1,1,1), method = "ML")
arima1
"modelo2"
arima2<- arima(dserie6,order=c(2,1,2), method = "ML")
arima2

"modelo3"
arima3<- arima(dserie6,order=c(3,1,3), method = "ML")
arima3
"modelo4"
arima4<- arima(dserie6,order=c(4,1,4), method = "ML")
arima4

"modelo5"
arima5<- arima(dserie6,order=c(5,1,5), method = "ML")


"inciso de"


independencia <- Box.test(arima4$residuals, type="Ljung-Box") 
independencia$p.value



plot.ts(arima4$residuals,sub="Figura 06: Residuales del modelo AR(1)", xlab="Tiempo",ylab="Residuales")
plot(acf(arima4$residuals, plot = FALSE))