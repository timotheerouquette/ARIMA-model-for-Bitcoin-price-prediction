#Assignment Exercise 2

#read data
btc.data=read.csv("/Data/btc.csv")

#Processing and cleaning data
btc.data$date=strptime(btc.data$date,format = "%Y-%m-%d")
btc.data$PriceUSD=round(btc.data$PriceUSD,2)
btc.data=btc.data[c(1,25)]
btc.data=btc.data[-c(1:561),]

#Plots
plot(btc.data$date,btc.data$PriceUSD,type="l",xlab="Date",ylab="Price in $",main="Bitcoin chart")
plot(btc.data$date,log(btc.data$PriceUSD),type="l",xlab="Date",main="Bitcoin log chart")

#creating the time series
btc.ts=ts(btc.data$PriceUSD,frequency=365,start=c(2010,200))

#Time series plots
plot.ts(btc.ts)
plot(log(btc.ts))
acf(log(btc.ts))
plot(diff(log(btc.ts)))

#Stationary time series
y=diff(log(btc.ts))

Box.test(y, lag=10, type="Ljung-Box") #reject that it is a white noise

par(mfrow=c(1,2))
acf(y)
pacf(y)
par(mfrow=c(1,1))


#test
mymodel<-arima(log(btc.ts),order=c(0,1,6))
acf(mymodel$res,plot=T,main="residual correlogram")
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box")




#MA models
mymodel<-arima(log(btc.ts),order=c(0,1,6))
acf(mymodel$res,plot=T,main="residual correlogram")
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we do not reject that it is a white noise

mymodel<-arima(log(btc.ts),order=c(0,1,5))
acf(mymodel$res,plot=T,main="residual correlogram")
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we reject that it is a white noise

mymodel<-arima(y,order=c(0,1,6))
acf(mymodel$res,plot=T,main="residual correlogram")
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we reject that it is a white noise

#AR model
mymodel<-arima(log(btc.ts),order=c(6,1,0))
pacf(mymodel$res,plot=T,main="residual correlogram") # not great residuals pacf
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we do not reject that it is a white noise

mymodel<-arima(log(btc.ts),order=c(7,1,0))
pacf(mymodel$res,plot=T,main="residual correlogram") # not great residuals pacf
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we do not reject that it is a white noise, best p-value 

mymodel<-arima(log(btc.ts),order=c(6,1,0))
pacf(mymodel$res,plot=T,main="residual correlogram") # not great residuals pacf
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we reject that it is a white noise

#ARMA model

mymodel<-arima(log(btc.ts),order=c(6,1,6))
acf(mymodel$res,plot=T,main="residual correlogram")
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we do not reject that it is a white noise
#This model is the best validated from the Box-Ljung test,  p-value=0.819
AIC(mymodel)
BIC(mymodel)

mymodel<-arima(log(btc.ts),order=c(7,1,6))
acf(mymodel$res,plot=T,main="residual correlogram")
mymodel
Box.test(mymodel$res, lag=10, type="Ljung-Box") #we do not reject that it is a white noise
#This model is the best validated from the Box-Ljung test,  p-value=0.9177
AIC(mymodel)
BIC(mymodel)

#Highest order coefficient are significant

summary(mymodel)

#out-of-sample criteria
y<-log(btc.ts)
S=365;h=7;
error1.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(6,1,6))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error1.h<-c(error1.h,y[i+h]-predict.h)
}
summary(abs(error1.h))

error2.h<-c()
for (i in S:(length(y)-h))
{
  mymodel.sub<-arima(y[1:i],order=c(7,1,6))
  predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
  error2.h<-c(error2.h,y[i+h]-predict.h)
}
summary(abs(error1.h))


#Forecasts
myforecast<-predict(mymodel,n.ahead=365)
expected=myforecast$pred
alpha=0.05;q=qnorm(1-alpha/2)
lower=myforecast$pred-q*myforecast$se;
upper=myforecast$pred+q*myforecast$se;
cbind(lower,expected,upper)

plot(log(btc.ts),xlim=c(2020,2022),ylim=c(8,12))
lines(expected,col="red")
lines(lower,col="blue")
lines(upper,col="blue")

library(forecast)
shortseries<-window(log(btc.ts),start=2019)
myforecast<-forecast(shortseries,method="arima")
myforecast$model
plot(myforecast)

shortseries<-window(log(btc.ts),start=2011)
myforecast<-forecast(shortseries,method="arima")
myforecast$model
plot(myforecast)

