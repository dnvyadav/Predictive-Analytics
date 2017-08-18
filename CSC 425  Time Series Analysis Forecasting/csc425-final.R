library(zoo)
library(tseries)
library(fBasics)
library(lmtest)
library(forecast)

#read table and create time series
D=read.table('GDPC1.csv',header=T,sep=',')
head(D)
value=D[,2]
Value = ts(value, start=c(1947,1), freq=4)
plot(Value,type="l", xlab="Time", ylab="GDP History(Billions of chained 2009 Dollars)", main= 'GDP:History(1947.01-2015.01)')
lnvalue=log(Value)
plot(lnvalue,type="l", xlab="Time", ylab="log(GDP)History(Billions of chained 2009 Dollars)",main= 'log(GDP):History(1947.01-2015.01)')
basicStats(Value)
basicStats(lnvalue)
acf(coredata(lnvalue), plot=T, lag=15)
pacf(coredata(lnvalue), plot=T, lag=15)

#check Distribution
hist(lnvalue, xlab="Log GDPC", prob=TRUE, main="Histogram")
xfit<-seq(min(lnvalue),max(lnvalue),length=40)
yfit<-dnorm(xfit,mean=mean(lnvalue),sd=sd(lnvalue))
lines(xfit, yfit, col="blue", lwd=2) 
qqnorm(lnvalue)
qqline(lnvalue,col='blue',lwd=2)


#model selection 
m1=Arima(lnvalue,order=c(3,1,2), include.drift = T,method="ML")
coeftest(m1)
acf(coredata(m1$residuals),lag=20, main='ACF Plot of Residuals') 
Box.test(m1$residuals, lag=10, type='Ljung',fitdf = 5)
Box.test(m1$residuals, lag=20, type='Ljung',fitdf = 5)
polyroot(c(1,-m1$coef[1:2]))

#backtesting
source("backtest.R")
pm1 = backtest(m1, lnvalue, 240, 1)


#model forecast
pred=forecast.Arima(m1)
plot(forecast.Arima(m1, h=20), include =100, main='Log(real-GDP)ARIMA(3,1,2)with drift:History(1947.01-2015.01 and Forecast(2015.02-2020.02))')

# plot predicted values and 95% prediction bounds in blue
pred_GDPC=exp(pred$mean)
plot.ts(c(Value,pred_GDPC))
lines(c(rep(NA, length(Value)), exp(pred$lower[,2])), col='blue', lwd=5)
lines(c(rep(NA, length(Value)), exp(pred$upper[,2])),col='blue',lwd=5)

f1=forecast.Arima(m1,h=20)

plot(f1, include=100,main='Model Predictions VS Original Value')
lines(ts(c(f1$fitted, f1$mean), frequency=4,start=c(1947,1)), col="blue")



m1=auto.arima(lnvalue,ic="bic",max.P = 8,max.Q = 8,trace = T)
coeftest(m1)
m1=Arima(lnvalue,order=c(2,1,2), include.drift = T,method="ML")
coeftest(m1)
