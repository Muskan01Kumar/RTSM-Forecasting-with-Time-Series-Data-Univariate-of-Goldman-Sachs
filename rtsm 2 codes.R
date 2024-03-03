# 0.1. Fetch Single Stock/Index Data
install.packages("quantmod")
library(quantmod)

getSymbols(Symbols='GS',
           src='yahoo',
           from=as.Date('2015-01-01'),
           to=as.Date('2019-12-31'),
           periodicity='daily')

gs_price = na.omit (GS$GS.Adjusted) #Adjusted Closing Price 
class(gs_price) #xts (Time-Series) object

head(gs_price)

plot(gs_price)

# Step 1 : Check for (Weak) Stationarity :: Augmented Dickey-Fuller (ADF) Test

# Install and load necessary package
if (!requireNamespace("tseries", quietly = TRUE)) {
  install.packages("tseries")
}
library(tseries)

# Custom ADF test function with handling missing values
custom_adf_test <- function(x) {
  adf.test(na.omit(x))
}

# Augmented Dickey-Fuller (ADF) Test for Goldman Sachs
adf_test_gs <- custom_adf_test(gs_price)
adf_test_gs  # Inference: Goldman Sachs Time-Series is Non-Stationary

# First Difference of Goldman Sachs Time-Series
gs_ds <- diff(gs_price)
plot(gs_ds)  # Plot of Goldman Sachs (First) Difference Time-Series

# Augmented Dickey-Fuller (ADF) Test for the Difference Time-Series
adf_test_gs_ds <- custom_adf_test(gs_ds)
adf_test_gs_ds  # Inference: Goldman Sachs Difference Time-Series is Stationary


# Step 2 : Check for Autocorrelation :: Ljung-Box Test 
# Box-Ljung Test for Autocorrelation of Goldman Sachs Difference Time-Series
lb_test_gs_ds <- Box.test(gs_ds)
lb_test_gs_ds  # Inference: Goldman Sachs Difference (Stationary) Time-Series is Autocorrelated


# Step 3 : Model for Autocorrelation :: ARIMA Models
# Autocorrelation Function (ACF) | Partial Autocorrelation Function (PACF) for Goldman Sachs

# ACF and PACF of Goldman Sachs Series
acf_gs <- acf(gs_price, main = "ACF of GS Series", na.action = na.pass)
pacf_gs <- pacf(gs_price, main = "PACF of GS Series", na.action = na.pass)

# ACF and PACF of Goldman Sachs Difference (Stationary) Series
acf_gs_ds <- acf(gs_ds, main = "ACF of GS Difference (Stationary) Series", na.action = na.pass)
pacf_gs_ds <- pacf(gs_ds, main = "PACF of GS Difference (Stationary) Series", na.action = na.pass)

install.packages("forecast")
library(forecast)

# Auto ARIMA
arma_pq_gs_ds = auto.arima(gs_ds)
arma_pq_gs_ds

# ARIMA (1, 0, 0) or AR(1)
ar1_gs = arima(gs_ds, order = c(1, 0, 0)); ar1_gs

# ARIMA (2, 0, 0) or AR(2)
ar2_gs = arima(gs_ds, order = c(2, 0, 0)); ar2_gs

# ARIMA (0, 0 , 1) or MA(1)
ma1_gs = arima(gs_ds, order = c(0, 0, 1)); ma1_gs

# ARIMA (0, 0, 2) or MA(2)
ma2_gs = arima(gs_ds, order = c(0, 0, 2)); ma2_gs

# ARIMA (0, 0, 3) or MA(3)
ma3_gs = arima(gs_ds, order = c(0, 0, 3)); ma3_gs

# ARIMA (0, 0, 4) or MA(4)
ma4_gs = arima(gs_ds, order = c(0, 0, 4)); ma4_gs

# ARIMA (1, 0, 1) or ARMA(1, 1)
arma11_gs = arima(gs_ds, order = c(1, 0, 1)); arma11_gs

# ARIMA (1, 0, 2) or ARMA(1, 2)
arma12_gs = arima(gs_ds, order = c(1, 0, 2)); arma12_gs

# ARIMA (1, 0, 3) or ARMA(1, 3)
arma13_gs = arima(gs_ds, order = c(1, 0, 3)); arma13_gs

# Auto ARIMA
arma_pq_gs_ds = auto.arima(gs_ds); arma_pq_gs_ds


# Step 4 : Check for Heteroskedasticity :: ARCH LM Test
# Install and load necessary packages
if (!requireNamespace("FinTS", quietly = TRUE)) {
  install.packages("FinTS")
}
if (!requireNamespace("rugarch", quietly = TRUE)) {
  install.packages("rugarch")
}
library(FinTS)
library(rugarch)

gs_ret = na.omit(diff(log(gs_price))) # GS Returns
plot(gs_ret)

# Test for Volatility Clustering or Heteroskedasticity: Box Test
gs_ret_sq = gs_ret^2 # Return Variance (Since Mean Returns is approx. 0)
plot(gs_ret_sq)
gs_ret_sq_box_test = Box.test(gs_ret_sq, lag = 10) # H0: Return Variance Series is Not Serially Correlated
gs_ret_sq_box_test # Inference : Return Variance Series is Heteroskedastic (Has Volatility Clustering)

# Test for Volatility Clustering or Heteroskedasticity: ARCH Test
gs_ret_arch_test = ArchTest(gs_ret, lags = 20) # H0: No ARCH Effects
gs_ret_arch_test # Inference : Return Series is Heteroskedastic (Has Volatility Clustering)

# Fit ARCH(1) model
arch_model = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0)),
                        distribution.model = "std")

arch_fit = ugarchfit(spec = arch_model, data = gs_ret)
summary(arch_fit)


# Step 5a : Model for Heteroskedasticity in [Data | Transformed Data] (Step 2) :: GARCH Models

# Test for Volatility Clustering or Heteroskedasticity: ARCH Test
gs_garch_arch_test = ArchTest(residuals(gs_ret_garch1)^2, lags = 1) # H0: No ARCH Effects
gs_garch_arch_test # Inference : Return Series is Heteroskedastic (Has Volatility Clustering)

garch_model2 <- ugarchspec(
  variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 2), include.mean = FALSE),
  fixed.pars = list(alpha1 = 0.1, beta1 = 0.8)  # Adjust initial values
)

# GARCH Forecast
gs_ret_garch_forecast1 = ugarchforecast(gs_ret_garch1, n.ahead = 500); gs_ret_garch_forecast1
gs_ret_garch_forecast2 = ugarchforecast(gs_ret_garch2, n.ahead = 500); gs_ret_garch_forecast2

# Assuming gs_ret_garch2 is your fitted GARCH model
gs_ret_garch_forecast_rolling = ugarchforecast(gs_ret_garch2, n.ahead = 10)  # Set n.ahead to a value greater than or equal to 5






