#cargar paquete readr
library(readr)
library(tidyverse)
library(readxl)
#importar datos gapminder
ruta <-  "C:\\Users\\Encarni\\Desktop\\TFM\\Prediccion\\autoridades\\huelva.xlsx"
datos_huelva <- read_excel(ruta)
names(datos_huelva)
datos_huelva
total_ton = ts(datos_huelva$Total_toneladas, freq=12, start = c(2022,1), end = c(2024,4))
str(total_ton)
total_ton


plot(total_ton, main='Tráfico de Toneladas mensuales en Huelva', xlab='Año', ylab = 'Toneladas', col="steelblue1")

####descomposición aditiva (Yt=Tt+St+Rt)---Y_t=f(Tt,St,Rt)
aditiva=decompose(total_ton, type='add')
plot(aditiva) ####descomposición
plot(aditiva$figure)####nube de puntos 
summary(aditiva)

####Ddescomposición multiplicativa (Yt=Tt*St*Rt)---Y_t=f(Tt,St,Rt)
total_ton
multi=decompose(total_ton, type='mult')
plot(multi) ####descomposición
plot(multi$figure)####nube de puntos 
summary(multi)
boxplot(total_ton ~ cycle(total_ton))

####predicción
#####install.packages('forecast')
library(forecast)
##separamos dataset train y test

### 28*0.8=22.4  Por meses, 2 años, 28 meses.
cTrain=1:28<=22

train=ts(subset(total_ton, subset = cTrain), start = c(2022,1), frequency = 12)
test=ts(subset(total_ton, subset = !cTrain), start = c(2022,1), frequency = 12)

boxplot(train ~ cycle(train))
plot(train, ylab="")

#####suavizado exponencial
sExpMOd<-ses(train, h=48)
plot(sExpMOd)
lines(test, col=2)
summary(sExpMOd)
errTestsExp=sExpMOd$mean-test
plot(errTestsExp)

#####holt1

holt<-holt(train, h=6)
plot(holt)
lines(test, col=2)
summary(holt)
errTestsHolt=holt$mean-test

#####HOLT2
holtWin<-hw(train, h=6)
plot(holtWin)
lines(test, col=2)
summary(holtWin)
errTestsHoltWin=holtWin$mean-test

#####HOLT 3
holtWinMult<-hw(train, seasonal = 'multiplicative', h=18)
plot(holtWinMult)
lines(test, col=2)
summary(holtWinMult)
errTestsHoltWinMult=holtWinMult$mean-test

plot(errTestsHoltWin)
lines(errTestsHoltWinMult, col=2)

####ejemplo 

ejempl03=ts(data=total_ton)

acf(x=total_ton, main="Función de Autocorrelación Simple", col="steelblue1") ##### 1 =q
pacf(x = total_ton, main="Función de Autocorrelación Parcial", col="steelblue1")##### función de autocorrelación parcial p=1

tseries::adf.test(total_ton) ## p-value: 0.37



PP.test(total_ton) ##  p-value = 0.01 

## aplicar diferencias para hacerla estacionaria.
## Con diferencia
dnot<-diff(total_ton)
plot(dnot, type="l", main="Tráfico en Toneladas mensuales en Cartagena", sub=NA, xlab="Meses/Año", ylab="Toneladas", col="steelblue1")
acf(dnot, main="Función de Autocorrelación Simple", col="steelblue1") #q=1
pacf(dnot, main="Función de Autocorrelación Parcial", col="steelblue1")# P= 0 1

plot(train, ylab="Toneladas", main="Time")

tseries::adf.test(dnot) #p-valor= 0.01004

## Se hace una segunda diferenciación
diff_2<-diff((total_ton), differences = 2)
plot(x=diff_2,  type="l", main="Tráfico en Toneladas mensuales en Bilbao", sub=NA, xlab="Meses/Año", ylab="Toneladas", col="steelblue1")
acf(diff_2, main="Función de Autocorrelación Simple", col="steelblue1",xlab =,ylab=) #q=2
pacf(diff_2, main='Función de Autocorrelación Parcial', col="steelblue1",xlab =,ylab= ) #p=2  

tseries::adf.test(diff_2) ##  p-valor=0.01 

## ESTIMACIÓN DEL MODELO ARIMA ## 
library(forecast)
fit <- auto.arima(train)
summary(fit)

## Estimación Modelo ARIMMA (0,1,1) huelva
astsa::sarima(train, 0,1,1) #  AIC = 28,54
astsa::sarima(train, 0,2,2) # AIC= 28.95
astsa::sarima(train, 1,2,1) # AIC= 29,10069
astsa::sarima(train, 2,2,2) # AIC= 29,12






# Verificación
model1<-arima(x=train, order = c(0,2,2))# 
summary(model1) # MAE: 193508.3

model2<-arima(x=train, order= c(1,2,1))
summary(model2) # MAE: 255602.3

model3<-arima(x=train, order= c(2,2,2))
summary(model3) # MAE: 200095


## Predicción
forecast::forecast(model1,h=12)

plot(forecast::forecast(model1,h=12), main="Predicción Huelva")


forecast::forecast(model2,h=12)

plot(forecast::forecast(model2,h=12), main="Predicción Huelva")




forecast::forecast(model3,h=12)

plot(forecast::forecast(model3,h=12), main="Predicción Huelva")