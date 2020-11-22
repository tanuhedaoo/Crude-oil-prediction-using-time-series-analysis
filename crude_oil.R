# -------------------------------------------------------------------------------------------------------------------
# Package required
# -------------------------------------------------------------------------------------------------------------------
library(forecast)
library(quantmod)
library(tseries)
library(timeSeries)
library(xts)
library(dplyr)

# -------------------------------------------------------------------------------------------------------------------
# Data cleaning 
# -------------------------------------------------------------------------------------------------------------------
getwd()
df = read.csv("CL=F.csv")
dim(df)
head(df)
price = df$Adj.Close
head(price)
adf.test(price)
plot.ts(df$Adj.Close)
decompose_price = decompose(ts(price,start = c(2005,1),end = c(2020,12),frequency = 12))
plot(decompose_price)
# we see that there is no trend present , hence it is stationary time series 
# we can see some trend 

# p-value is greater than 0.05, we accept null hypothesis => Data is non-stationary 
# take the first difference



return_price = diff(log(price))
plot(return_price,type = "l")
decompose_price = decompose(ts(return_share_price,start = c(2005,1),end = c(2020,12),frequency = 12))
plot(decompose_price)

library("car")
qqPlot(return_price)
library("ggpubr")
ggdensity(return_price, 
          main = "Density plot of return price",
          xlab = "return Price ")
plot.ts(return_price)
return_price = ts(return_price,start = c(2005))
plot.ts(return_price , y=df$Date)
stockvar.ts = ts(price , start = 2005-01-01,frequency = 240)
stockvar.de = decompose(stockvar.ts)
plot(stockvar.de)
length(return_price)
length(date)
class(df$Date)
date
# --------------------------------------------------------------------------------------------------------------------
# Autocorrelation 
# --------------------------------------------------------------------------------------------------------------------
# Ljung Box test for autocorrelation
Box.test(return_share_price,lag = 10,type = 'Ljung-Box')

# Since p-val is less than 0.05 we reject the null hypothesis 
# The autocorrelation is present 

# --------------------------------------------------------------------------------------------------------------------
# checking stationarity-
# --------------------------------------------------------------------------------------------------------------------
decompose_price = decompose(ts(return_share_price,start = c(2005,1),end = c(2020,12),frequency = 12))
plot(decompose_price)
# we see that there is no trend present , hence it is stationary time series 

# we can also use adf test to check the stationarity
library(tseries)
adf.test(return_share_price)
# since p-val is <0.05 hence , reject null hypothesis, that is stationarity is present

# --------------------------------------------------------------------------------------------------------------------
# PACF
# --------------------------------------------------------------------------------------------------------------------
library(forecast)
pacf(ts(return_price),lag = 10)
# 1 , 2 , 4 , 5 
# p = 2 or 5
acf(return_price,lag = 10)
# q = 1 or 6
# --------------------------------------------------------------------------------------------------------------------
# Auto arima
# --------------------------------------------------------------------------------------------------------------------

auto.arima(return_price,max.p = 10,max.d = 0,max.q = 0,ic = 'aic')
auto.arima(return_price,max.p = 10,max.d = 0,max.q = 0,ic = 'bic')

# AR(3),AR(2)

auto.arima(return_price,max.p = 0,max.d = 0,max.q = 10,ic = 'aic')
auto.arima(return_price,max.p = 0,max.d = 0,max.q = 10,ic = 'bic')

# MA(6) and MA(6)

auto.arima(return_price,max.p = 10,max.d = 0,max.q = 10,ic = 'aic')
auto.arima(return_price,max.p = 10,max.d = 0,max.q = 10,ic = 'bic')

# ARMA(7,4) and ARMA(4,3)
# --------------------------------------------------------------------------------------------------------------------
# Model building 
# --------------------------------------------------------------------------------------------------------------------

model1 = arima(return_price,order=c(3,0,0),include.mean = FALSE)
summary(model1)
model1$residuals
model2 = arima(return_price,order=c(2,0,0),include.mean = FALSE)
model3 = arima(return_price,order=c(0,0,6),include.mean = FALSE)
model4 = arima(return_price,order=c(6,0,0),include.mean = FALSE)
model5 = arima(return_price,order=c(4,0,3),include.mean = FALSE)

# --------------------------------------------------------------------------------------------------------------------
# MODEL DIAGNOSTIC
# --------------------------------------------------------------------------------------------------------------------
# if p_val < 0.05 then reject null hypothesis 
# H0: there is no autocorrelation present 
Box.test(model1$residuals,10,type = 'Ljung-Box')
Box.test(model2$residuals,10,type = 'Ljung-Box')
Box.test(model3$residuals,10,type = 'Ljung-Box')
Box.test(model4$residuals,10,type = 'Ljung-Box')
Box.test(model5$residuals,10,type = 'Ljung-Box')

# model 2 and model 4 accepted

length(return_price)

train = return_price[1:2398]
test = return_price[2399:4796]
length(train)
length(test)
pred1 = 0
pred2 = 0
pred3 = 0
pred4 = 0
for (i in 1:length(test)){
  model_11 = arima(train,order=c(3,0,0),include.mean = FALSE)
  model_22 = arima(train,order=c(2,0,0),include.mean = FALSE)
  model_33 = arima(train,order=c(0,0,6),include.mean = FALSE)
  model_44 = arima(train,order=c(6,0,0),include.mean = FALSE)
  print(i)
  pre1 = predict(model_11,1)
  pre2 = predict(model_22,1)
  pre3 = predict(model_33,1)
  pre4 = predict(model_44,1)
  pred1[i] = pre1$pred[1]
  pred2[i] = pre2$pred[1]
  pred3[i] = pre3$pred[1]
  pred4[i] = pre4$pred[1]
}
cbind(pred1,pred2,pred3,pred4,test)
for (i in 1:length(test)){
  model_11 = arima(train,order=c(3,0,0),include.mean = FALSE)
  pre1 = predict(model_11,1)
  pred1[i] = pre1$pred[1]
}
print(pred1)
  
install.packages("Metrics")
library(Metrics)
rmse(test,pred1)
rmse(test,pred2)
mse(test,pred1)
mse(test,pred2)
mse(test,pred3)
mse(test,pred4)

# AR(2) Model selected 

model = arima(return_price,order=c(4,0,3),include.mean = FALSE)
forecast = predict(model,10)
forecast
crude_forecast = c()
crude_forecast[1] = df$Adj.Close[nrow(df)]+exp(forecast$pred[1])
for(i in 2:10){
  crude_forecast[i] = crude_forecast[i-1]+exp(forecast$pred[i])
}
crude_forecast
head(df)

# --------------------------------------------------------------------------------------------------------------------
# GARCH
# --------------------------------------------------------------------------------------------------------------------

tsObj<- ts(as.vector(df$Adj.Close), start=2005,frequency = 300)
plot.ts(tsObj)
r.crude = diff(log(tsObj))*100
plot(r.crude)
abs = abs(r.crude)
par(mfrow=c(1,2))
acf(abs, ci.type="ma",main=" ACF for abs. returns")
pacf(abs, main=" PACF plot for abs.returns")
eacf(abs)

garch1 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(4, 3)), 
                    mean.model = list(armaOrder = c(3, 4), include.mean = TRUE), 
                    distribution.model = "norm")
garchfit = ugarchfit(spec = garch1, data = r.crude, solver = "hybrid")
garchfit
spec           <- getspec(garchfit)
setfixed(spec) <- as.list(coef(garchfit))
garchforecast1 <- ugarchforecast(spec, n.ahead = 1,data = r.crude)

plot(garchforecast1, which = "all")

model.34 = garch(r.crude,order = c(3,4),trace = FALSE)
model.22 = garch(r.crude , order = c(2,2),trace = FALSE)
model.23 = garch(r.crude , order = c(2,3),trace = FALSE)
library(dLagM)
install.packages("dLagM")
sc.AIC = AIC(model.34,model.22,model.23)
sortScore(sc.AIC, score ="aic")

model34<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 4)), 
                    mean.model = list(armaOrder = c(4, 3), include.mean = TRUE), 
                    distribution.model = "norm")

m.34<-ugarchfit(spec=model34,data=r.crude, out.sample = 100)
plot(m.34,which="all")

model34_t_skw<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 4)), 
                          mean.model = list(armaOrder = c(4, 3), include.mean = TRUE), 
                          distribution.model = "sstd")

m.34_t_skw<-ugarchfit(spec=model34_t_skw,data=r.crude, out.sample = 100)
m.34_t_skw

forc = ugarchforecast(m.34, data = bitcoin, n.ahead = 10, n.roll =10)
print(forc)
plot(forc, which= "all")

p.t_1 = df$Adj.Close[nrow(df)]
R_t <-c( -0.67611,0.13247,0.07659 ,0.22100,0.19547,0.1304 ,
         0.08099,0.06694, 0.06966 ,0.07081 )
p_t= 0 
for (i in 1:10){
  p_t =  p.t_1 *((2.71828)^(R_t[i]/100))
  print(p_t)
  p.t_1=p_t
}


