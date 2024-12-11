# Install required packages (if not installed already)
install.packages("TTR")
install.packages("forecast")
install.packages("ggplot2")
install.packages("tseries")

# Load the libraries
library(TTR)
library(forecast)
library(ggplot2)
library(tseries)

# Load the dataset
aaww_df <- read.csv("AAWW.csv")

# EDA
summary(aaww_df)

# Check for missing values
sum(is.na(aaww_df))

# Convert 'Date' column to Date format
aaww_df$Date <- as.Date(aaww_df$Date)

# Set 'Date' as the row names
rownames(aaww_df) <- aaww_df$Date

# Plot the time series data for 'Close' price
ggplot(aaww_df, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "Atlas Air Worldwide Holdings - Close Price Over Time", 
       x = "Date", y = "Close Price (USD)") +
  theme_minimal()

# Histogram of the 'Close' price
ggplot(aaww_df, aes(x = Close)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Close Price", x = "Close Price (USD)", y = "Frequency") +
  theme_minimal()

# Calculate 7-day and 30-day moving averages
aaww_df$`7_day_MA` <- filter(aaww_df$Close, rep(1/7, 7), sides = 1)  # One-sided moving average
aaww_df$`30_day_MA` <- filter(aaww_df$Close, rep(1/30, 30), sides = 1)  # One-sided moving average

# Plot the Close price along with the moving averages
ggplot(aaww_df, aes(x = Date)) +
  geom_line(aes(y = Close), color = "blue") +
  geom_line(aes(y = `7_day_MA`), color = "orange") +
  geom_line(aes(y = `30_day_MA`), color = "green") +
  labs(title = "Close Price with 7-Day and 30-Day Moving Averages", 
       x = "Date", y = "Close Price (USD)") +
  theme_minimal()

# Plot autocorrelation
acf(aaww_df$Close, main="Autocorrelation of Close Price")

# Create a time series object for the 'Close' price
ts_data <- ts(aaww_df$Close, frequency = 365)  # Assuming the data is daily with yearly seasonality

# --- Step 1: Stationarity Check using Augmented Dickey-Fuller Test ---
adf_test <- adf.test(ts_data)
print(adf_test)  # p-value > 0.05, so the series is non-stationary

# --- Step 2: Difference the series to make it stationary ---
ts_data_diff <- diff(ts_data, differences = 1)
# Plot the differenced data to visually inspect
plot.ts(ts_data_diff)

# --- Step 3: Stationarity Check after differencing ---
adf_test_diff <- adf.test(ts_data_diff)
print(adf_test_diff)  # p-value < 0.05, the differenced series is stationary

# --- Step 4: Fit the ARIMA model to the differenced data ---
fit_arima_diff <- auto.arima(ts_data_diff)
summary(fit_arima_diff)

# --- Step 5: Fit the ETS model to the differenced data ---
fit_ets_diff <- ets(ts_data_diff)
summary(fit_ets_diff)

# --- Step 6: Compare AIC and RMSE for ARIMA and ETS models ---
arima_aic_diff <- AIC(fit_arima_diff)
ets_aic_diff <- AIC(fit_ets_diff)
arima_rmse_diff <- sqrt(mean(residuals(fit_arima_diff)^2))
ets_rmse_diff <- sqrt(mean(residuals(fit_ets_diff)^2))

cat("ARIMA AIC: ", arima_aic_diff, "\n")
cat("ETS AIC: ", ets_aic_diff, "\n")
cat("ARIMA RMSE: ", arima_rmse_diff, "\n")
cat("ETS RMSE: ", ets_rmse_diff, "\n")

# --- Step 7: Forecast the next 30 days for both ARIMA and ETS models ---
forecast_arima_diff <- forecast(fit_arima_diff, h = 30)
forecast_ets_diff <- forecast(fit_ets_diff, h = 30)  # Fixed the missing parenthesis

# --- Step 8: Plot the forecasts for both ARIMA and ETS ---
par(mfrow = c(1, 2))  # Side-by-side plotting

# Plot ARIMA forecast
plot(forecast_arima_diff, main = "ARIMA Forecast for the Next 30 Days")

# Plot ETS forecast
plot(forecast_ets_diff, main = "ETS Forecast for the Next 30 Days")


# --- Step 9: Residual Diagnostics for ARIMA and ETS ---
# Ljung-Box test for ARIMA residuals
Box.test(residuals(fit_arima_diff), lag = 10, type = "Ljung-Box")

# Ljung-Box test for ETS residuals
Box.test(residuals(fit_ets_diff), lag = 10, type = "Ljung-Box")
