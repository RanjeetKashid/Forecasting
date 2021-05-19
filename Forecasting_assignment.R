#################### Coca Cola Sales Data ####################

library(readr)
library(readxl)

cola <- read_xlsx(file.choose())
plot(cola$Sales,type = "o")

# Creating Dummy variable

Q1 <-  ifelse(grepl("Q1",cola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cola$Quarter),'1','0')

cola <- cbind(cola,Q1,Q2,Q3,Q4)

cola["t"] <- 1:42
cola["log_sales"] <- log(cola["Sales"])
cola["t_square"] <- cola["t"]*cola["t"]
attach(cola)

# Splitting data

train_cola <- cola[1:38,]
test_cola <- cola[39:42,]

##### Linear Model #####

cola_linear <- lm(Sales ~ t, data = train_cola)
summary(cola_linear)
cola_linear_pred <- data.frame(predict(cola_linear,interval = 'predict',newdata = test_cola))
View(cola_linear_pred)
RMSE_cola_linear <- sqrt(mean((test_cola$Sales - cola_linear_pred$fit)^2,na.rm = T))
RMSE_cola_linear # 519.55

##### Exponential Model #####

cola_exp <- lm(log_sales~t,data = train_cola)
summary(cola_exp)
cola_exp_pred <- data.frame(predict(cola_exp,interval = 'predict',newdata = test_cola))
RMSE_cola_exp <- sqrt(mean((test_cola$Sales - exp(cola_exp_pred$fit))^2,na.rm = T))
RMSE_cola_exp # 466.25

##### Quadratic Model #####

cola_quad <- lm(Sales~t+t_square,data = train_cola)
summary(cola_quad)
cola_quad_pred <- data.frame(predict(cola_quad,interval = 'predict',newdata = test_cola))
RMSE_cola_quad <- sqrt(mean((test_cola$Sales - cola_quad_pred$fit)^2,na.rm = T))
RMSE_cola_quad # 475.56

##### Additive Seasonality #####

cola_as <- lm(Sales~Q1+Q2+Q3+Q4,data = train_cola)
summary(cola_as)
cola_as_pred <- data.frame(predict(cola_as,interval = 'predict',newdata = test_cola))
RMSE_cola_as <- sqrt(mean((test_cola$Sales - cola_as_pred$fit)^2,na.rm = T))
RMSE_cola_as # 1860

##### Additive Seasonality with linearity #####

cola_asl <- lm(Sales~t+Q1+Q2+Q3+Q4,data = train_cola)
summary(cola_asl)
cola_asl_pred <- data.frame(predict(cola_asl,interval = 'predict',newdata = test_cola))
RMSE_cola_asl <- sqrt(mean((test_cola$Sales - cola_asl_pred$fit)^2,na.rm = T))
RMSE_cola_asl # 464.98

##### Additive Seasonality with Quadratic #####

cola_asq <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data = train_cola)
summary(cola_asq)
cola_asq_pred <- data.frame(predict(cola_asq,interval = 'predict',newdata = test_cola))
RMSE_cola_asq <- sqrt(mean((test_cola$Sales - cola_asq_pred$fit)^2,na.rm = T))
RMSE_cola_asq # 301.74

##### Multiplicative Seasonality #####

cola_ms <- lm(log_sales~Q1+Q2+Q3+Q4,data = train_cola)
summary(cola_ms)
cola_ms_pred <- data.frame(predict(cola_ms,interval = 'predict',newdata = test_cola))
RMSE_cola_ms <- sqrt(mean((test_cola$Sales - exp(cola_ms_pred$fit))^2,na.rm = T))
RMSE_cola_ms # 1963.39

##### Multiplicative Seasonality with linear trend #####

cola_msl <- lm(log_sales~t+Q1+Q2+Q3+Q4,data = train_cola)
summary(cola_msl)
cola_msl_pred <- data.frame(predict(cola_msl,interval = 'predict',newdata = test_cola))
RMSE_cola_msl <- sqrt(mean((test_cola$Sales - exp(cola_msl_pred$fit))^2,na.rm = T))
RMSE_cola_msl # 225.52

# Converting into time series

library(tseries)

train_cola_ts <- ts(train_cola$Sales,frequency = 4)
test_cola_ts  <- ts(test_cola$Sales, frequency = 4)

library(forecast)
library(fpp)
library(smooth)

##### ARIMA ####

acf(cola$Sales, lag.max = NULL) # detects q value, q value = 8

pacf(cola$Sales, lag.max = 4) # detects p value, here p value = 1

# d value here I would consider 1 for integrated model, because sales data is volatile, need to consider moving average

cola_arima <- Arima(train_cola_ts,order = c(1,1,8))
cola_arima_forecast <- forecast(cola_arima,h=4)
plot(cola_arima_forecast)
cola_arima_accuracy <- accuracy(cola_arima_forecast,x=as.numeric(test_cola_ts))
cola_arima_accuracy # RMSE = 430.22
RMSE_cola_arima = 430.22

##### Moving Average #####

cola_ma <- sma(train_cola_ts)
cola_ma_pred <- data.frame(predict(cola_ma,h=4))
plot(forecast(cola_ma))
cola_ma_accuracy <- accuracy(cola_ma_pred$Point.Forecast,test_cola_ts)
cola_ma_accuracy # RMSE = 493.17
RMSE_cola_ma = 493.17

##### Holt Winters #####

cola_hw <- HoltWinters(train_cola_ts)
cola_hw_pred <- data.frame(predict(cola_hw,n.ahead = 4))
plot(forecast(cola_hw,h=4))
cola_hw_accuracy <- accuracy(cola_hw_pred$fit,test_cola_ts)
cola_hw_accuracy # RMSE = 126.61
RMSE_cola_hw = 126.61

##### RMSE Comparison #####

cola_RMSE <- data.frame(c("RMSE_cola_linear","RMSE_cola_exp","RMSE_cola_quad","RMSE_cola_as", "RMSE_cola_asl","RMSE_cola_asq", "RMSE_cola_ms", "RMSE_cola_msl", "RMSE_cola_arima", "RMSE_cola_ma", "RMSE_cola_hw"),
                        c(RMSE_cola_linear,RMSE_cola_exp,RMSE_cola_quad,RMSE_cola_as, RMSE_cola_asl,RMSE_cola_asq, RMSE_cola_ms, RMSE_cola_msl, RMSE_cola_arima, RMSE_cola_ma, RMSE_cola_hw))
colnames(cola_RMSE) <- c("Model","RMSE")



#################### Airline Passenger Data ####################

passenger <- read_xlsx(file.choose())
plot(passenger$Passengers,type = "o")

# Creating Dummy variable

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names 

airline <- cbind(passenger,X)

airline["t"] <- 1:96
airline["log_passenger"] <- log(airline["Passengers"])
airline["t_square"] <- airline["t"]*airline["t"]
attach(airline)

# Splitting data

train_airline <- airline[1:84,]
test_airline <- airline[85:96,]


##### Linear Model #####

airline_linear <- lm(Passengers ~ t, data = train_airline)
summary(airline_linear)
airline_linear_pred <- data.frame(predict(airline_linear,interval = 'predict',newdata = test_airline))
View(airline_linear_pred)
RMSE_airline_linear <- sqrt(mean((test_airline$Passengers - airline_linear_pred$fit)^2,na.rm = T))
RMSE_airline_linear # 53.19

##### Exponential Model #####

airline_exp <- lm(log_passenger~t,data = train_airline)
summary(airline_exp)
airline_exp_pred <- data.frame(predict(airline_exp,interval = 'predict',newdata = test_airline))
RMSE_airline_exp <- sqrt(mean((test_airline$Passengers - exp(airline_exp_pred$fit))^2,na.rm = T))
RMSE_airline_exp # 46.05


##### Quadratic Model #####

airline_quad <- lm(Passengers~t+t_square,data = train_airline)
summary(airline_quad)
airline_quad_pred <- data.frame(predict(airline_quad,interval = 'predict',newdata = test_airline))
RMSE_airline_quad <- sqrt(mean((test_airline$Passengers - airline_quad_pred$fit)^2,na.rm = T))
RMSE_airline_quad # 48.05

##### Additive Seasonality #####

airline_as <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_airline)
summary(airline_as)
airline_as_pred <- data.frame(predict(airline_as,interval = 'predict',newdata = test_airline))
RMSE_airline_as <- sqrt(mean((test_airline$Passengers - airline_as_pred$fit)^2,na.rm = T))
RMSE_airline_as # 132.82

##### Additive Seasonality with linearity #####

airline_asl <- lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_airline)
summary(airline_asl)
airline_asl_pred <- data.frame(predict(airline_asl,interval = 'predict',newdata = test_airline))
RMSE_airline_asl <- sqrt(mean((test_airline$Passengers - airline_asl_pred$fit)^2,na.rm = T))
RMSE_airline_asl # 35.35

##### Additive Seasonality with Quadratic #####

airline_asq <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_airline)
summary(airline_asq)
airline_asq_pred <- data.frame(predict(airline_asq,interval = 'predict',newdata = test_airline))
RMSE_airline_asq <- sqrt(mean((test_airline$Passengers - airline_asq_pred$fit)^2,na.rm = T))
RMSE_airline_asq # 26.36

##### Multiplicative Seasonality #####

airline_ms <- lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_airline)
summary(airline_ms)
airline_ms_pred <- data.frame(predict(airline_ms,interval = 'predict',newdata = test_airline))
RMSE_airline_ms <- sqrt(mean((test_airline$Passengers - exp(airline_ms_pred$fit))^2,na.rm = T))
RMSE_airline_ms # 140.06

##### Multiplicative Seasonality with linear trend #####

airline_msl <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_airline)
summary(airline_msl)
airline_msl_pred <- data.frame(predict(airline_msl,interval = 'predict',newdata = test_airline))
RMSE_airline_msl <- sqrt(mean((test_airline$Passengers - exp(airline_msl_pred$fit))^2,na.rm = T))
RMSE_airline_msl # 10.51

# Converting into time series

library(tseries)

train_airline_ts <- ts(train_airline$Passengers,frequency = 12)
test_airline_ts  <- ts(test_airline$Passengers, frequency = 12)


##### ARIMA ####

acf(airline$Passengers, lag.max = 30) # detects q value, q value = 25

pacf(airline$Passengers, lag.max = 12) # detects p value, here p value = 1

# d value here I would consider 0 for stationary model, because passenger data is not so volatile

airline_arima <- Arima(train_airline_ts,order = c(1,0,25))
airline_arima_forecast <- forecast(airline_arima,h=12)
plot(airline_arima_forecast)
airline_arima_accuracy <- accuracy(airline_arima_forecast,x=as.numeric(test_airline_ts))
airline_arima_accuracy # RMSE = 38.02
RMSE_airline_arima = 38.02

##### Moving Average #####

airline_ma <- sma(train_airline_ts)
airline_ma_pred <- data.frame(predict(airline_ma,h=12))
plot(forecast(airline_ma))
airline_ma_accuracy <- accuracy(airline_ma_pred$Point.Forecast,test_airline_ts)
airline_ma_accuracy # RMSE = 68.00
RMSE_airline_ma = 68.00

##### Holt Winters #####

airline_hw <- HoltWinters(train_airline_ts)
airline_hw_pred <- data.frame(predict(airline_hw,n.ahead = 12))
plot(forecast(airline_hw,h=12))
airline_hw_accuracy <- accuracy(airline_hw_pred$fit,test_airline_ts)
airline_hw_accuracy # RMSE = 9.04
RMSE_airline_hw = 9.04

##### RMSE Comparison #####

airline_RMSE <- data.frame(c("RMSE_airline_linear","RMSE_airline_exp","RMSE_airline_quad","RMSE_airline_as", "RMSE_airline_asl","RMSE_airline_asq", "RMSE_airline_ms", "RMSE_airline_msl", "RMSE_airline_arima", "RMSE_airline_ma", "RMSE_airline_hw"),
                        c(RMSE_airline_linear,RMSE_airline_exp,RMSE_airline_quad,RMSE_airline_as, RMSE_airline_asl,RMSE_airline_asq, RMSE_airline_ms, RMSE_airline_msl, RMSE_airline_arima, RMSE_cola_ma, RMSE_airline_hw))
colnames(airline_RMSE) <- c("Model","RMSE")
