dji.data <- read.csv("C:/The Dow Jones index.csv", sep=";",header=T);dji.data
plot(dji.data[,2],type="o",pch=16,cex=.5,xlab='Date',ylab='DJI',
     xaxt='n')
axis(1, seq(1,85,12), dji.data[seq(1,85,12),1])
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(dji.data[,2],lag.max=25,type="correlation",main="ACF for the Number \nof Dow Jones Index")
acf(dji.data[,2], lag.max=25,type="partial",main="PACF for the Number \nof Dow Jones Index ")
dji.fit.ar1<-arima(dji.data[,2],order=c(1, 0, 0))
dji.fit.ar1
res.dji.ar1<-as.vector(residuals(dji.fit.ar1))
#to obtain the fitted values we use the function fitted() from
#the forecast package
library(forecast)
fit.dji.ar1<-as.vector(fitted(dji.fit.ar1))
Box.test(res.dji.ar1,lag=48,fitdf=3,type="Ljung")
#ACF and PACF of the Residuals
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(res.dji.ar1,lag.max=25,type="correlation",main="ACF of the Residuals \nof AR(1) Model")
acf(res.dji.ar1, lag.max=25,type="partial",main="PACF of the Residuals \nof AR(1) Model")
#4-in-1 plot of the residuals
par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(res.dji.ar1,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(res.dji.ar1,datax=TRUE)
plot(fit.dji.ar1,res.dji.ar1,pch=16, xlab='Fitted Value',
     ylab='Residual')
abline(h=0)
hist(res.dji.ar1,col="gray",xlab='Residual',main='')
plot(res.dji.ar1,type="l",xlab='Observation Order',
     ylab='Residual')
points(res.dji.ar1,pch=16,cex=.5)
abline(h=0)
wt.dji<-diff(dji.data[,2])
par(mfrow=c(1,1),oma=c(0,0,0,0))
plot(wt.dji,type="o",pch=16,cex=.5,xlab='Date',ylab='w(t)', xaxt='n')
axis(1, seq(1,85,12), dji.data[seq(1,85,12),1])
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(wt.dji,lag.max=25,type="correlation",main="ACF for the Number \nof w(t)")
acf(wt.dji, lag.max=25,type="partial",main="PACF for the Number \nof w(t)")
