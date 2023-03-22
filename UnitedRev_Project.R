library(forecast)
library(zoo)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/dileep/Downloads/TimeSeries")

# Create data frame.
unitedrevenue.data <- read.csv("united_air_revenue.csv")

# See the first 6 records of the file.
head(unitedrevenue.data)
tail(unitedrevenue.data)

# Create timeseries dataset
unitedrev.ts <- ts(unitedrevenue.data$Revenue, 
                   start = c(2000, 1), end = c(2019, 4), freq = 4)
unitedrev.ts

#TEST predictability of United Air Revenues dataset.

# Use Arima() function to fit AR(1) model for United Air Revenues dataset.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
unitedrev.ar1<- Arima(unitedrev.ts, order = c(1,0,0))
summary(unitedrev.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9594
s.e. <- 0.0291
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}


# Create first differenced United Air Revenue dataset using lag1.
diff.unitedrev.ts <- diff(unitedrev.ts, lag = 1)
diff.unitedrev.ts

# Use Acf() function to identify autocorrealtion for first differenced 
# United Air Revenue dataset, and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(diff.unitedrev.ts, lag.max = 8, 
    main = "Autocorrelation for Differenced United Air Revenue dataset")

# Plot of the time series data  
plot(unitedrev.ts, 
     xlab = "Time", ylab = "Revenue (in thousand $)", 
     ylim = c(2500000, 12000000), xaxt = 'n',
     main = "United Air Revenue")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(2000, 2019, 1), labels = format(seq(2000, 2019, 1)))

# The plot with original data, trend, seasonal, and reminder 
# (level and noise component).
unitedrev.stl <- stl(unitedrev.ts, s.window = "periodic")
autoplot(unitedrev.stl, main = "United Air Revenue Time Series Components")

# Autocorrelation for different lags.
unitedrev.autocor <- Acf(unitedrev.ts, lag.max = 12, 
               main = "Autocorrelation for United Air Revenue Data")

# Create data partitioning for United Air Revenue data.
nValid <- 16
nTrain <- length(unitedrev.ts) - nValid
train.ts <- window(unitedrev.ts, start = c(2000, 1), end = c(2000, nTrain))
valid.ts <- window(unitedrev.ts, start = c(2000, nTrain + 1), 
                   end = c(2000, nTrain + nValid))
valid.ts
train.ts

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Revenue (in 000s $)", ylim = c(2500000, 15000000), 
     bty = "l", xlim = c(2000, 2021), xaxt = 'n', main = "", lwd = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 15000000))
lines(c(2019.8, 2019.8), c(0, 15000000))
text(2006, 14000000, "Training")
text(2017.5, 14000000, "Validation")
text(2021, 14000000, "Future")
arrows(2000, 13000000, 2016, 13000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.1, 13000000, 2019.5, 13000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020, 13000000, 2021.5, 13000000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## FIT OPTIMAL HOLT's WINTER MODEL.
# Created Holt-Winter's (HW) exponential smoothing for partitioned data.
# Used ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Used optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ # Model appears to be (M, N, M), with alpha = 0.9713 and gamma = 0.0001.

# Used forecast() function to make predictions using this HW model for
# validation period (nValid). 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Revenue (in 000s $)", ylim = c(2500000, 15000000), 
     bty = "l", xlim = c(2000, 2021), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(unitedrev.ts)
legend(2000,13500000, 
       legend = c("Revenues", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 15000000))
lines(c(2020, 2020), c(0, 15000000))
text(2006, 14000000, "Training")
text(2017.5, 14000000, "Validation")
text(2021, 14000000, "Future")
arrows(2000, 13500000, 2015, 13500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.5, 13500000, 2018.5, 13500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 13500000, 2021.5, 13500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT AUTO ARIMA MODEL.
# Used auto.arima() function to fit ARIMA model.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 8, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(2500000, 15000000), xaxt = "n", 
     bty = "l", xlim = c(2000, 2021), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2000, 2021), labels = format(seq(2000, 2021)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2000,13500000, legend = c("Revenues Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 15000000))
lines(c(2020, 2020), c(0, 15000000))
text(2006, 14000000, "Training")
text(2017.5, 14000000, "Validation")
text(2021, 14000000, "Future")
arrows(2000, 13500000, 2015, 13500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.5, 13500000, 2018.5, 13500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 13500000, 2021.5, 13500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT OPTIMAL REGRESSION MODEL

## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
# Used tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)
summary(train.lin)

# Applied forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 2. 
# Used tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 3. 
# Used tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)
summary(train.season)

# Applied forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4. 
# Used tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)

# Applied forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 5. 
# Used tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quad.season)

# Applied forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

# Used accuracy() function to identify common accuracy measures
# for various regression models: (1) linear trend, (2) quadratic (polynomial) trend, 
# (4) seasonality, (5) linear trend and seasonality, and (6) quadratic trend 
# and seasonality. 
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)

# Plot ts data, linear trend and seasonality data, and predictions for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(2500000, 13000000), bty = "l",
     xlim = c(2000, 2022), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(2000,11000000, legend = c("Revenue Time Series", 
                             "Linear Trend and Seasonality Model for Training Data",
                             "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 13500000))
lines(c(2020, 2020), c(0, 13500000))
text(2006, 12000000, "Training")
text(2017.5, 12000000, "Validation")
text(2022, 12000000, "Future")
arrows(2000, 11500000, 2016, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 11500000, 2019.5, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 11500000, 2022.5, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)




## 2-LEVEL FORECASTING REGRESSION + MA
# Based on the regression models above the regression model chosen for Level-1
# forecasting is model with Linear Trend and Seasonality.

# Identified regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
train.lin.season.res <- train.lin.season$residuals
train.lin.season.res

# Applied trailing MA for residuals with window width k = 4
# for training partition.
ma.trail.res <- rollmean(train.lin.season.res, k = 4, align = "right")
ma.trail.res

# Created residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Developed two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level.ma <- train.lin.season.pred$mean + ma.trail.res.pred$mean
fst.2level.ma

# Created a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level.ma), 3)
names(valid.df) <- c("Revenues", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

#using this in plot below
fst.2level.train <- ma.trail.res.pred$fitted + train.lin.season$fitted

# Plot original Ridership time series data and regression model.
plot(unitedrev.ts, 
     xlab = "Time", ylab = "Revenue (in 000s $)", ylim = c(2500000, 13000000), 
     bty = "l", xlim = c(2000, 2022), lwd =1, xaxt = "n",
     main = "Revenue Data and Regression with Trend and Seasonality") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(fst.2level.train, col = "blue", lwd = 2)
lines(fst.2level.ma, col = "blue", lty =5, lwd = 2)
legend(2000,11000000, legend = c("Revenue", "2-Level Fst for training",
                             "2-Level Fst for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 13500000))
lines(c(2020, 2020), c(0, 13500000))
text(2006, 12000000, "Training")
text(2017.5, 12000000, "Validation")
text(2022, 12000000, "Future")
arrows(2000, 11500000, 2016, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 11500000, 2019.5, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 11500000, 2022.5, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



## 2-LEVEL FORECASTING REGRESSION + AR model
# Based on the regression models above the regression model chosen for Level-1
# forecasting is model with Linear Trend and Seasonality.

# Identified regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
train.lin.season.res <- train.lin.season$residuals
train.lin.season.res

# Used Acf() function to identify autocorrelation for the model residuals 
Acf(train.lin.season.res, lag.max = 12, 
    main = "Autocorrelation for Revenue Training Residuals")

# Used Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
res.ar1 <- Arima(train.lin.season.res, order = c(1,0,0))
summary(res.ar1)

# Used forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Used Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Revenue Training Residuals of Residuals")

# Developed two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.
fst.2level.ar<- train.lin.season.pred$mean + res.ar1.pred$mean
fst.2level.ar

# Created data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, fst.2level.ar),3)
names(valid.df) <- c("Revenues", "Regression.Fst", 
                     "AR(1).Residuals.Fst", "Combined.Fst")
valid.df

fst.2level.train <- res.ar1.pred$fitted + train.lin.season$fitted


# Plot original Ridership time series data and regression model.
plot(unitedrev.ts, 
     xlab = "Time", ylab = "Revenue (in 000s $)", ylim = c(2500000, 13000000), 
     bty = "l", xlim = c(2000, 2022), lwd =1, xaxt = "n",
     main = "Revenue Data and Regression with Trend and Seasonality") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(fst.2level.train, col = "blue", lwd = 2)
lines(fst.2level.ar, col = "blue", lty =5, lwd = 2)
legend(2000,11000000, legend = c("Revenue", "2-Level Fst for training",
                                 "2-Level Fst for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 13500000))
lines(c(2020, 2020), c(0, 13500000))
text(2006, 12000000, "Training")
text(2017.5, 12000000, "Validation")
text(2022, 12000000, "Future")
arrows(2000, 11500000, 2016, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 11500000, 2019.5, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.5, 11500000, 2022.5, 11500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Use accuracy() function to identify common accuracy measures.
round(accuracy(hw.ZZZ.pred$mean,valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean,valid.ts), 3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(fst.2level.ma, valid.ts), 3)
round(accuracy(fst.2level.ar, valid.ts), 3)




## FIT OPTIMAL MODEL ON ENTIRE DATASET

# The chosen models to apply on the entire dataset include:
# (1) Regression model with Linear Trend and Seasonality
# (2) 2-Level Forecasting Regression + MA
# (3) 2-Level Forecasting Regression + AR() model

## Fitted a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(unitedrev.ts ~ trend  + season)
summary(tot.trend.seas)

# (1) This shows the future 12 periods prediction using 
# Regression model with Linear Trend and Seasonality.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

## Fitted 2-Level forecasting model with Regression and MA model

# Identified and displayed regression model(1) residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Developed trailing MA model to forecast regression residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res

# Used trailing MA model to create trailing MA residual forecast for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Developed two-level forecast for entire dataset by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level.tot.ma <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
fst.2level.tot.ma

# Created a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
Fst_MA.df <- round(data.frame(tot.trend.seas.pred$mean,
                              tot.ma.trail.res.pred$mean, 
                              fst.2level.tot.ma), 3)
names(Fst_MA.df) <- c("Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
Fst_MA.df
# 2nd model ended here

fst.2level.tot <- tot.ma.trail.res.pred$fitted + tot.trend.seas.pred$fitted

fst.2level.ma.tot <- tot.ma.trail.res.pred$fitted + tot.trend.seas.pred$fitted


plot(unitedrev.ts, 
     xlab = "Time", ylab = "Revenue (in 000s $)", ylim = c(2500000, 13000000), 
     bty = "l", xlim = c(2000, 2022), lwd =1, xaxt = "n",
     main = "Revenue Data and Regression with Trend and Seasonality") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(fst.2level.tot, col = "blue", lwd = 2)
lines(fst.2level.tot.ma, col = "blue", lty =5, lwd = 2)
legend(2000,11000000, legend = c("Revenue", "2-Level Fst for training",
                                 "2-Level Fst for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 13500000))
lines(c(2020, 2020), c(0, 13500000))
text(2006, 13000000, "Training")
text(2017.5, 13000000, "Validation")
text(2022, 13000000, "Future")
arrows(2000, 12500000, 2016, 12500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 12500000, 2020, 12500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020, 12600000, 2022.5, 12600000, code = 3, length = 0.1,
       lwd = 1, angle = 30)




## Fitted 2-Level forecasting model with Regression and AR model

# Identified and displayed regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Used Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
Acf(tot.trend.seas.res, lag.max = 8, 
    main = "Autocorrelation for Revenues Residuals of Residuals")

# Used Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
tot.ar1 <- Arima(tot.trend.seas.res, order = c(1,0,0))
summary(tot.ar1)

# Used forecast() function to make prediction of residuals for future 12 periods
tot.ar1.pred <- forecast(tot.ar1, h = 12, level = 0)
tot.ar1.pred
#------------------------------------------
# Used Acf() function to identify autocorrelation for the 
# residual of residuals and plot autocorrelation for different lags 
# This validates that the AR model has captured the trend patterns in the 
# residuals of the regression models 
Acf(tot.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Revenues Data Residuals of Residuals for 2-level Fst(AR1)")

# Developed two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for future periods.
fst.2level.tot.ar<- tot.trend.seas.pred$mean + tot.ar1.pred$mean
fst.2level.tot.ar

# Created data table with Future 12 periods data, regression forecast
# for Future 12 periods, AR(1) residuals for Future 12 periods, and 
# two level model results. 

Fst_AR.df <- round(data.frame(tot.trend.seas.pred$mean, 
                             tot.ar1.pred$mean, fst.2level.tot.ar),3)
names(Fst_AR.df) <- c("UnitedRev.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
Fst_AR.df

fst.2level.tot <- tot.ar1.pred$fitted + tot.trend.seas$fitted
fst.2level.ar.tot <- tot.ar1.pred$fitted + tot.trend.seas$fitted


plot(unitedrev.ts, 
     xlab = "Time", ylab = "Revenue (in 000s $)", ylim = c(2500000, 13000000), 
     bty = "l", xlim = c(2000, 2022), lwd =1, xaxt = "n",
     main = "2-Level Fst of Revenue Data with Regression & AR model") 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(fst.2level.tot, col = "blue", lwd = 2)
lines(fst.2level.tot.ar, col = "blue", lty =5, lwd = 2)
legend(2000,11000000, legend = c("Revenue", "2-Level Fst for training",
                                 "2-Level Fst for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2016, 2016), c(0, 13500000))
lines(c(2020, 2020), c(0, 13500000))
text(2006, 13000000, "Training")
text(2017.5, 13000000, "Validation")
text(2022, 13000000, "Future")
arrows(2000, 12500000, 2016, 12500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2016.3, 12500000, 2020, 12500000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020, 12600000, 2022.5, 12600000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy((naive(unitedrev.ts))$fitted, unitedrev.ts), 3)
round(accuracy((snaive(unitedrev.ts))$fitted, unitedrev.ts), 3)
round(accuracy(tot.trend.seas$fitted, unitedrev.ts), 3)
round(accuracy(fst.2level.ma.tot, unitedrev.ts), 3)
round(accuracy(fst.2level.ar.tot, unitedrev.ts), 3)

