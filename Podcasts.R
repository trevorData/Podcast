library(ggplot2)
library(forecast)
library(tseries)

setwd("~/Projects/Podcasts")
data <- read.csv('data_2019-07-29.csv')
data$date <- data$date %>% as.Date

freq <- 30

##################################################################
# Analyize each podcast time series to determine arima arguments #
##################################################################

##### chapo #####
ts <- data$chapo %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # slightly negative at lag 1, q=1, p=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70)

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 1), seasonal = c(1, 1, 0)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 0), seasonal = c(1, 1, 0)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 2084.918
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 2189.211
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 2246.897

# Plot forecast on holdout set
fcast %>% plot(PI=F)
ts %>% lines

##### lpotl #####
ts <- data$lpotl %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # slightly positive at lag 1, q=0, p=1
ts %>% diff(differences = diff) %>% pacf(lag.max = 70)

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 300

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 0), seasonal = c(1, 1, 0)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 1139.321
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 1177.701
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 1649.689

# Plot forecast on holdout set
fcast.c %>% plot(PI=F)
ts %>% lines

##### seccap #####
ts <- data$seccap %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # very slightly negative at lag 1, consider either q=1 or q=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) # vert little action on PCF. no "cutoff", will test model with AR=MA=0

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(0, 1, 0), seasonal = c(1, 1, 0)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 310.9942
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 327.5197
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 477.4125

# Plot forecast on holdout set
fcast %>% plot(PI=F)
ts %>% lines

##### tco #####
ts <- data$tco %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # slightly positive at lag 1, q=0, p=1
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) 

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 0), seasonal = c(1, 1, 0)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 1577.489
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 1575.642
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 1654.743

# Plot forecast on holdout set
fcast %>% plot(PI=F)
ts %>% lines

##### tcb #####
ts <- data$tcb %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # negative at lag 1, q=1, p=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) 

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(0, 1, 1), seasonal = c(1, 1, 0)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 252.6302
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 262.5422
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 224.2995 # this is the lowest RMSE, but because it is close I will stay with the params from the ACF

# Plot forecast on holdout set
fcast %>% plot(PI=F)
ts %>% lines

##### tmg #####
ts <- data$tmg %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # very slightly positive at lag 1, q=0, p=1, will also test AR=MA=1 and AR=MA=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) 

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 1252.668
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 1246.933
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 1232.527 # will use this one

# Plot forecast on holdout set
fcast.c %>% plot(PI=F)
ts %>% lines

##### naddp #####
ts <- data$naddp %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # very slightly positive at lag 1, q=0, p=1, will also test AR=MA=1 and AR=MA=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) 

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 260.5205
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 294.392
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 232.941 # will use this one

# Plot forecast on holdout set
fcast %>% plot(PI=F)
ts %>% lines

##### dough #####
ts <- data$dough %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70)

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # slightly positive at lag 1, q=0 p=1, will also test AR=MA=1 and AR=MA=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) 

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 99.40896
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 76.66148 # will use this one
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 886.5453

# Plot forecast on holdout set
fcast %>% plot(PI=F)
ts %>% lines

##### msw #####
ts <- data$msw %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # too much autocorrelation, will need d=2
ts %>% diff(differences = 2) %>% acf(lag.max = 70) # d=2

# Set chosen differencing level, test again after differencing
diff <- 2

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # negative at lag 1, q=1 p=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) 

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(0, 2, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(1, 2, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(0, 2, 1), seasonal = c(1, 1, 0)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 1742.065
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 1847.149
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 6069.821

# Plot forecast on holdout set
fcast.c %>% plot(PI=F)
ts %>% lines

##### tesd #####
ts <- data$tesd %>% na.omit %>% ts(frequency = freq)

# Test for stationarity
ts %>% adf.test(alternative = 'stationary') 

# Find level of differencing 
ts %>% acf(lag.max = 70)
ts %>% diff(differences = 1) %>% acf(lag.max = 70) # d=1
ts %>% diff(differences = 2) %>% acf(lag.max = 70) 

# Set chosen differencing level, test again after differencing
diff <- 1

ts %>% diff(differences = diff) %>% adf.test(alternative = 'stationary')

# Plot Autocorrelation and Partial Autocorrelation to determine AR and MA terms
ts %>% diff(differences = diff) %>% acf(lag.max = 70) # slightly positive at lag 1, q=0 p=1, will also consider AR=MA=1 and AR=MA=0
ts %>% diff(differences = diff) %>% pacf(lag.max = 70) 

# All ts have monthly seasonal trend so D=1 is assumed. Plot ACF after seasonal differencing to determine SAR and SMA terms
ts %>% diff(differences = diff) %>% diff(differences = 1, lag = freq) %>% acf(lag.max = 70) # negative at seasonal lag, Q=1, P=0

# Test forecast on holdout set
days <- 200

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)

# Calculate RMSE
((test - fcast$mean)^2)   %>% mean %>% sqrt # 1212.932
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 1304.691
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 1213.309

# Plot forecast on holdout set
fcast %>% plot(PI=F)
ts %>% lines

# All of the forecasts are pretty bad, caused by a change in trend that starts right at the beginning of the test set
# I think a better training set should include the change in trend, so I'll shrink the holdout set to 100 days
days <- 100

train <- ts %>% window(end = (ts %>% length - days - 1)/freq + 1)
test <- ts %>% window(start = (ts %>% length - days)/freq + 1)

fcast   <- train %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.b <- train %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
fcast.c <- train %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)

((test - fcast$mean)^2)   %>% mean %>% sqrt # 151.4409
((test - fcast.b$mean)^2) %>% mean %>% sqrt # 152.9747
((test - fcast.c$mean)^2) %>% mean %>% sqrt # 66.94237

# The 1,1,1 model creates a better forecast, so I will use those params
fcast %>% plot(PI=F)
ts %>% lines

##########################
# Forecast next 200 days #
##########################

days <- 200

# Create a column of dates to append forecast data to
forecast.data <- seq(data[nrow(data),]$date + 1, by = 1, length.out = days) %>% as.data.frame
colnames(forecast.data) <- 'date'

# Forecast 200 days out for each pod
chapo.f  <- data$chapo  %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
lpotl.f  <- data$lpotl  %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days) 
seccap.f <- data$seccap %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
tco.f    <- data$tco    %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(1, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
tcb.f    <- data$tcb    %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
tmg.f    <- data$tmg    %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
naddp.f  <- data$naddp  %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
dough.f  <- data$dough  %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(0, 1, 0), seasonal = c(0, 1, 1)) %>% forecast(h=days)
msw.f    <- data$msw    %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(0, 2, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)
tesd.f   <- data$tesd   %>% na.omit %>% ts(frequency = 30) %>% Arima(order = c(1, 1, 1), seasonal = c(0, 1, 1)) %>% forecast(h=days)

# Add column for each forecast
forecast.data$chapo  <- chapo.f$mean
forecast.data$lpotl  <- lpotl.f$mean
forecast.data$seccap <- seccap.f$mean
forecast.data$tco    <- tco.f$mean
forecast.data$tcb    <- tcb.f$mean
forecast.data$tmg    <- tmg.f$mean
forecast.data$naddp  <- naddp.f$mean
forecast.data$dough  <- dough.f$mean
forecast.data$msw    <- msw.f$mean
forecast.data$tesd   <- tesd.f$mean

# Bind the forecast data at the end of the original data
df <- data[c("date", "chapo", "lpotl",  "seccap", "tco", "tcb", "tmg", "naddp", "dough", "msw", "tesd")] %>% rbind(forecast.data)

#########################
# Main plot - all lines #
#########################

cols <- c("chapo"='steelblue1', "lpotl"='tomato',  "seccap"='black', "tco"='violetred1', "tcb"='darkgreen', "tmg"='royalblue4',
          "naddp"='saddlebrown', "dough"='orange', "msw"='rosybrown1', "tesd"='limegreen')

# Plots original line for each podcast, as well as the new forecasted line
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = chapo , color='chapo'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = lpotl , color='lpotl'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = seccap, color='seccap'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tco   , color='tco'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tcb   , color='tcb'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tmg   , color='tmg'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = naddp , color='naddp'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = dough , color='dough'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = msw   , color='msw'), size=1) +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tesd  , color='tesd'), size=1) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = chapo , group=1, color='chapo'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = lpotl , group=1, color='lpotl'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = seccap, group=1, color='seccap'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = tco   , group=1, color='tco'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = tcb   , group=1, color='tcb'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = tmg   , group=1, color='tmg'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = naddp , group=1, color='naddp'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = dough , group=1, color='dough'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = msw   , group=1, color='msw'), alpha=.4) +
  geom_line(data = df[nrow(data)+1:nrow(df),], aes(x = date, y = tesd  , group=1, color='tesd'), alpha=.4) +
  scale_color_manual(values = cols) +
  theme_minimal() +
  labs(title = "Podcast Subscribers Over Time", y = "Patreon Subscribers", x = '', color='') 

####################
# Individual Plots #
####################

# Plots original line for each podcast, along with new forecasted line, as well as the 95% and 85% confidence intervals

# CHAPO
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = chapo), size=1) +
  geom_ribbon(data = chapo.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x=`forecast.data$date`), fill='gray91') +
  geom_ribbon(data = chapo.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x=`forecast.data$date`), fill='gray82') +
  geom_line(data = chapo.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='steelblue1', size=1)+ 
  labs(title = "Chapo Trap House", y = "Patreon Subscribers", x = "") + 
  theme_minimal()

# LPOTL
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = lpotl), size=1) +
  geom_ribbon(data = lpotl.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = lpotl.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = lpotl.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='tomato', size=1) + 
  labs(title = "Last Podcast On The Left", y = "Patreon Subscribers", x = '') +
  theme_minimal()

# SECCAP
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = seccap), size=1) +
  geom_ribbon(data = seccap.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = seccap.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = seccap.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='gray42', size=1) + 
  labs(title = "Second Captains", y = "Patreon Subscribers", x = '') +
  theme_minimal()

# TCO
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tco), size=1) +
  geom_ribbon(data = tco.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = tco.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = tco.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='violetred1', size=1) + 
  labs(title = "True Crime Obsessed", y = "Patreon Subscribers", x = '') +
  theme_minimal()

# TCB
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tcb), size=1) +
  geom_ribbon(data = tcb.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = tcb.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = tcb.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='darkgreen', size=1) + 
  labs(title = "The Cum Boys", y = "Patreon Subscribers", x = '') +
  theme_minimal()

# TMG
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tmg), size=1) +
  geom_ribbon(data = tmg.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = tmg.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = tmg.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='royalblue4', size=1) + 
  labs(title = "Tiny Meat Gang", y = "Patreon Subscribers", x = '') +
  theme_minimal()

# NADDP
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = naddp), size=1) +
  geom_ribbon(data = naddp.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = naddp.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = naddp.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='saddlebrown', size=1) + 
  labs(title = "Not Another D&D Podcast", y = "Patreon Subscribers", x = '') +
  theme_minimal()

# DOUGH
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = dough), size=1) +
  geom_ribbon(data = dough.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = dough.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = dough.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='orange', size=1) + 
  labs(title = 'Doughboys', y = "Patreon Subscribers", x = '') +
  theme_minimal()

# MSW
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = msw), size=1) +
  geom_ribbon(data = msw.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = msw.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = msw.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='rosybrown2', size=1) + 
  labs(title = 'Mueller, She Wrote', y = "Patreon Subscribers", x = '') +
  coord_cartesian(ylim=c(0, 17000)) +
  theme_minimal()

# TESD
ggplot() +
  geom_line(data = df[1:nrow(data),], aes(x = date, y = tesd), size=1) +
  geom_ribbon(data = tesd.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 95`, ymax=`Hi 95`, x = `forecast.data$date`), fill='gray91') +
  geom_ribbon(data = tesd.f %>% as.data.frame %>% cbind(forecast.data$date),
              aes(ymin = `Lo 80`, ymax=`Hi 80`, x = `forecast.data$date`), fill='gray82') +
  geom_line(data = tesd.f %>% as.data.frame %>% cbind(forecast.data$date),
            aes(x = `forecast.data$date`, y = `Point Forecast`) , color='limegreen', size=1) + 
  labs(title = 'Tell \'em Steve-Dave!', y = "Patreon Subscribers", x = '') +
  theme_minimal()
