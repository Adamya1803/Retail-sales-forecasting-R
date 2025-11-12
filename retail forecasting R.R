# 1) Generate weekly data
set.seed(42)
start <- as.Date("2019-01-06")
n <- 220
date <- start + 7*(0:(n-1))

sales <- sapply(1:n, function(i){
  200 + 0.5*i + 30*sin(2*pi*(i/52)) + rnorm(1,0,10)
})

df <- data.frame(date, sales)
head(df)

# 2) Train / Test split
h <- 12
train <- df[1:(n-h), ]
test  <- df[(n-h+1):n, ]

# 3) Convert to time series
ts_train <- ts(train$sales, frequency = 52)

# 4) Install and load forecast package
if(!"forecast" %in% rownames(installed.packages())){
  install.packages("forecast")
}
library(forecast)

fit_arima <- auto.arima(ts_train)
fit_ets   <- ets(ts_train)

fc_arima <- forecast(fit_arima, h = h)
fc_ets   <- forecast(fit_ets, h = h)

# 5) Accuracy check
pred_arima <- as.numeric(fc_arima$mean)
pred_ets   <- as.numeric(fc_ets$mean)
actual     <- test$sales

rmse <- function(p,a) sqrt(mean((p-a)^2))
mae  <- function(p,a) mean(abs(p-a))

cat("\nMETRICS:")
cat("\nARIMA RMSE:", rmse(pred_arima, actual),
    " MAE:", mae(pred_arima, actual))
cat("\nETS   RMSE:", rmse(pred_ets, actual),
    " MAE:", mae(pred_ets, actual))

# 6) Plot result
plot(df$date, df$sales, type="l", col="gray", lwd=1, 
     main="Retail Sales Forecast", xlab="Date", ylab="Sales")
lines(test$date, actual, col="black", lwd=2)
lines(test$date, pred_arima, col="blue", lty=2, lwd=2)
lines(test$date, pred_ets, col="green", lty=3, lwd=2)

legend("topleft", legend=c("Actual","ARIMA","ETS"),
       col=c("black","blue","green"), lty=c(1,2,3), lwd=2)


