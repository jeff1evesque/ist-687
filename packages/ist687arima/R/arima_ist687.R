##
## arima_ist687.R, generate arima model, forecast, visualization, and prediction.
##
arima_ist687 <- function(data, ar, i, ma, periods) {
  ## generate timeseries
  ts.data <- ts(data, start=c(2015, 07), frequency=18)

  ##
  ## ith difference: difference between one successive month
  ##
  ## Note: this is similar to the second argument to the 'arima'
  ##       function, implemented below.
  ##
  ts.diff <- diff(ts.data, i)

  ## plot series
  plot(ts.diff)

  ##
  ## generate ARI(MA) model:
  ##
  ## Note: the arima arguments include:
  ##
  ##     AR: autoregression, use the dependent relationship between an observation
  ##         and some number of lagged observations.
  ##
  ##     I: integrated, use of differencing of raw observations, or subtracting an
  ##         observation from one at the previous time step. The goal is to attain a
  ##         time series that is stationary.
  ##
  ##     MA: moving average, uses the dependency between an observation and a residual
  ##         error from a moving average model applied to lagged observations.
  ##
  fit.ts.ar <- arima(data, order=c(ar, i, ma))

  ## generate forecast: we only have 18 periods
  fit.ts.arf <- forecast(fit.ts.ar, h=periods)

  ## visualize forecast
  plot(fit.ts.arf, include=periods)
}
