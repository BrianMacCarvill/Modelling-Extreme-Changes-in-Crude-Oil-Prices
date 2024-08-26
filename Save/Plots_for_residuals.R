

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
                    mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
                    distribution.model = "norm", fixed.pars = list(omega = 0))

# Initialize an empty vector to store forecasts
rolling_forecasts1 <- as.numeric(length(returns))
rolling_forecasts2 <-rolling_forecasts1
rolling_forecasts3 <- rolling_forecasts1
n.ahead <- 10
start=1000
fit <- ugarchfit(spec = spec, data = returns)
# Perform rolling forecasts
for (i in start:(length(returns) - n.ahead)) {
  # Subset data up to current point
  current_returns <- returns[(i-1000+1):i]

  forecast <- ugarchforecast(fit, data = current_returns, n.ahead = n.ahead)

  # Extract the forecasted values for the next n.ahead periods
  sigma_forecast <- sigma(forecast)[n.ahead]
  mu_forecast <- fitted(forecast)[n.ahead]
  print(sigma(forecast))
  rolling_forecasts1[i] <- mu_forecast
  rolling_forecasts2[i] <- sigma_forecast
  rolling_forecasts3[i] <- (returns[i+h]-mu_forecast)/sigma_forecast
  print(i)
}

# Combine forecasts into a data frame
rolling_forecasts_df <- do.call(rbind, lapply(rolling_forecasts, data.frame))

# Plot the rolling forecasts
plot(returns, type = "l", main = "Rolling Forecasts of Returns with GARCH(1,1)", ylab = "Returns", xlab = "Date")
lines(rolling_forecasts_df$mu_forecast, col = "red", lwd=200, type = "o")
legend("topright", legend = c("Actual Returns", "Forecasted Returns"), col = c("black", "blue"), lty = 1)


library(evd)
# Plot the rolling forecasts
plot(as.numeric(returns), type = "s", main = "Rolling Forecasts of Returns with GARCH(1,1)", ylab = "Returns", xlab = "Date", ylim=c(min(as.numeric(returns)),max(rolling_forecasts_df$sigma_forecast*qnorm(.99))))
lines(y=rolling_forecasts_df$mu_forecast,x=start:(length(returns) - 1), col = "red", type = "s")
lines(y=rolling_forecasts_df$sigma_forecast*qnorm(.99),x=start:(length(returns) - 1), col = "pink", lwd = 2, type = "s", linetype = "longdash")
lines(y=rolling_forecasts_df$sigma_forecast*qgpd(.99, scale = 0.589, shape = -0.096),x=start:(length(returns) - 1), col = "blue", lwd = 2, type = "l")

legend("topright", legend = c("Actual Returns", "Forecasted Returns"), col = c("black", "red"), lty = 1, lwd = 1)

(length(as.numeric(returns)[start:(length(returns) - 1)]))*(length(as.numeric(returns)[start:(length(returns) - 1)][returns[start:(length(returns) - 1)]>rolling_forecasts_df$mu_forecast+rolling_forecasts_df$sigma_forecast*qnorm(.95)])/length(start:(length(returns) - 1)))

length(rolling_forecasts_df$mu_forecast)
length(start:(length(returns) - 1))

residuals = as.numeric(residuals(fit))
plot(residuals(fit))
z = gpd.fit(residuals,threshold = quantile(residuals,.95))


plot(rolling_forecasts2[1000:length(rolling_forecasts)], type = 'l')
rolling_forecasts1
rolling_forecasts1[1000:length(rolling_forecasts)]












roll <- ugarchroll(
  spec = spec,
  data = returns,
  n.ahead = 1,
  forecast.length = 500,
  n.start = NULL,
  refit.every = 25,
  refit.window = "recursive",
  window.size = NULL,
  solver = "hybrid",
  fit.control = list(),
  solver.control = list(trace = 0, maxit = 1000),
  calculate.VaR = TRUE,
  VaR.alpha = c(0.01, 0.05),
  cluster = NULL,
  keep.coef = TRUE
)

# Print the summary of the rolling forecasts
summary(roll)
plot(roll)









# Set the parameters for the rolling forecast
window_size <- 1000
forecast_length <- 5



# Perform rolling forecasts with a window size of 1000
roll <- ugarchroll(
  spec = spec,
  data = returns,
  n.ahead = 2,
  forecast.length = window_size,
  n.start = length(returns) - window_size,
  refit.every = 25,
  refit.window = "moving",
  window.size = window_size,
  solver = "hybrid",
  fit.control = list(),
  solver.control = list(trace = 0, maxit = 1000),
  calculate.VaR = TRUE,
  VaR.alpha = c(0.01, 0.05),
  cluster = NULL,
  keep.coef = TRUE
)

# Print the summary of the rolling forecasts
summary(roll)

# Extract the forecasts
forecasts <- as.data.frame(roll@forecast)
head(forecasts)

# Compare the forecasted values with actual returns for the 5 steps ahead
forecast_horizon <- 5
actual_values <- tail(returns, forecast_horizon)
forecasted_values <- tail(forecasts$Mu, forecast_horizon)

# Print forecasted and actual values for comparison
cat("Forecasted Values (5 Steps Ahead):\n")
print(forecasted_values)
cat("\nActual Values:\n")
print(actual_values)

# Optional: Visualize the comparison
plot(actual_values, type = 'l', col = 'red', lwd = 2, ylab = "Returns", xlab = "Time",
     main = "Forecasted vs Actual Values")
lines(forecasted_values, col = 'blue', lwd = 2)
legend("topright", legend = c("Actual", "Forecasted"), col = c("red", "blue"), lty = 1, lwd = 2)


















# Specify the GARCH model with ARMA(1,1)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

# Parameters for rolling window forecast
window_size <- 1000
forecast_horizon <- 5
n_rolls <- length(returns) - window_size - forecast_horizon + 1

# Initialize matrices to store forecasts and actual values
forecasts <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)
sigma_forecasts <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)
actual_values <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)

# Perform rolling window forecasts
for (i in 1:n_rolls) {
  start_index <- i
  end_index <- start_index + window_size - 1
  forecast_start_index <- end_index + 1
  forecast_end_index <- forecast_start_index + forecast_horizon - 1


  fit <- ugarchfit(spec = spec, data = returns[start_index:end_index], solver = "hybrid",
                   solver.control = list(trace = 0, maxit = 3, tol = 1e-8, reltol = 1e-8))


  # Generate forecasts
  forecast <- ugarchforecast(fit, n.ahead = forecast_horizon)

  # Store forecasts and actual values
  forecasts[i, ] <- as.numeric(fitted(forecast))
  sigma_forecasts[i, ] <- as.numeric(sigma(forecast))
  actual_values[i, ] <- returns[forecast_start_index:forecast_end_index]
  print(i)
}

forecast_errors <- forecasts - actual_values

residuals = forecast_errors[,2]/sigma_forecasts[,2]
mean_forecast_error <- colMeans(forecast_errors, na.rm = TRUE)
plot(forecast_errors[,]/sigma_forecasts[,5],type='l')
plot(sigma_forecasts[,1], type = 'l')
plot(returns)
# Print the forecast errors
cat("Mean Forecast Error for each step ahead:\n")
print(mean_forecast_error)
acf(abs(residuals))
acf(abs(forecast_errors[,3]))

# Compare the last 5-step ahead forecast to the actual values
last_forecast <- forecasts[n_rolls, ]
last_sigma_forecast <- sigma_forecasts[n_rolls, ]
last_actual <- actual_values[n_rolls, ]

cat("\nLast 5-step Ahead Forecast:\n")
print(last_forecast)
cat("\nLast 5-step Sigma Forecast:\n")
print(last_sigma_forecast)
cat("\nLast 5-step Actual Values:\n")
print(last_actual)

# Optional: Visualize the comparison for the last forecast
plot(last_actual, type = 'l', col = 'red', lwd = 2, ylab = "Returns", xlab = "Steps Ahead",
     main = "Last 5-step Ahead Forecast vs Actual Values")
lines(last_forecast, col = 'blue', lwd = 2)
legend("topright", legend = c("Actual", "Forecasted"), col = c("red", "blue"), lty = 1, lwd = 2)

# Optional: Visualize the sigma forecasts
plot(last_sigma_forecast, type = 'l', col = 'green', lwd = 2, ylab = "Sigma", xlab = "Steps Ahead",
     main = "Last 5-step Ahead Sigma Forecasts")



































# Specify the GARCH model with ARMA(1,1)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

# Fit the model on the entire dataset initially
initial_fit <- ugarchfit(spec = spec, data = returns)

mu <- as.numeric(initial_params["mu"])
ar1 <- as.numeric(initial_params["ar1"])
ma1 <- as.numeric(initial_params["ma1"])
omega <- as.numeric(initial_params["omega"])
alpha1 <- as.numeric(initial_params["alpha1"])
beta1 <- as.numeric(initial_params["beta1"])

# Extract the initial model parameters
fixed_params <- list(
  mu = mu,
  ar1 = ar1,
  ma1 = ma1,
  omega = omega,
  alpha1 = alpha1,
  beta1 = beta1
)
# Set the parameters in the specification to speed up convergence
fixed_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = initial_params
)

# Parameters for rolling window forecast
window_size <- 1000
forecast_horizon <- 5
n_rolls <- length(returns) - window_size - forecast_horizon + 1

# Initialize matrices to store forecasts and actual values
forecasts <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)
sigma_forecasts <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)
actual_values <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)

fixed_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = fixed_params
)
# Perform rolling window forecasts
for (i in 1:n_rolls) {
  start_index <- i
  end_index <- start_index + window_size - 1
  forecast_start_index <- end_index + 1
  forecast_end_index <- forecast_start_index + forecast_horizon - 1
  print(i)

  print(i)
  # Fit the model on the rolling window using fixed parameters
  print(i)
  print(i)
  # Generate forecasts
  forecast <- ugarchforecast(fixed_spec, data = returns[start_index:end_index]$daily.returns, n.ahead = forecast_horizon)
  print(i)
  # Store forecasts and actual values
  forecasts[i, ] <- as.numeric(fitted(forecast))
  sigma_forecasts[i, ] <- as.numeric(sigma(forecast))
  actual_values[i, ] <- returns[forecast_start_index:forecast_end_index]
}

# Calculate the mean forecast error for the 5-step ahead forecasts
forecast_errors <- forecasts - actual_values
mean_forecast_error <- colMeans(forecast_errors, na.rm = TRUE)

# Print the forecast errors
cat("Mean Forecast Error for each step ahead:\n")
print(mean_forecast_error)

# Compare the last 5-step ahead forecast to the actual values
last_forecast <- forecasts[n_rolls, ]
last_sigma_forecast <- sigma_forecasts[n_rolls, ]
last_actual <- actual_values[n_rolls, ]

cat("\nLast 5-step Ahead Forecast:\n")
print(last_forecast)
cat("\nLast 5-step Sigma Forecast:\n")
print(last_sigma_forecast)
cat("\nLast 5-step Actual Values:\n")
print(last_actual)

# Optional: Visualize the comparison for the last forecast
plot(last_actual, type = 'l', col = 'red', lwd = 2, ylab = "Returns", xlab = "Steps Ahead",
     main = "Last 5-step Ahead Forecast vs Actual Values")
lines(last_forecast, col = 'blue', lwd = 2)
legend("topright", legend = c("Actual", "Forecasted"), col = c("red", "blue"), lty = 1, lwd = 2)

# Optional: Visualize the sigma forecasts
plot(last_sigma_forecast, type = 'l', col = 'green', lwd = 2, ylab = "Sigma", xlab = "Steps Ahead",
     main = "Last 5-step Ahead Sigma Forecasts")















































# Specify the GARCH model with ARMA(1,1)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# Fit the model on the entire dataset initially
initial_fit <- ugarchfit(spec = spec, data = returns)

# Extract the initial model parameters
initial_params <- coef(initial_fit)

mu <- as.numeric(initial_params["mu"])
ar1 <- as.numeric(initial_params["ar1"])
ma1 <- as.numeric(initial_params["ma1"])
omega <- as.numeric(initial_params["omega"])
alpha1 <- as.numeric(initial_params["alpha1"])
beta1 <- as.numeric(initial_params["beta1"])

# Convert parameters to a named list for fixed.pars
fixed_params <- list(
  mu = mu,
  ar1 = ar1,
  omega = omega,
  alpha1 = alpha1,
  beta1 = beta1
)

# Parameters for rolling window forecast
window_size <- 1000
forecast_horizon <- 5
n_rolls <- length(returns) - window_size - forecast_horizon + 1

# Initialize matrices to store forecasts and actual values
forecasts <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)
sigma_forecasts <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)
actual_values <- matrix(NA, ncol = forecast_horizon, nrow = n_rolls)

# Fit the model on the rolling window using fixed parameters
fixed_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = fixed_params
)

# Perform rolling window forecasts
for (i in 1:n_rolls) {
  start_index <- i
  end_index <- start_index + window_size - 1
  forecast_start_index <- end_index + 1
  forecast_end_index <- forecast_start_index + forecast_horizon - 1

  # Generate forecasts
  forecast <- ugarchforecast(fixed_spec, data = returns[start_index:end_index], n.ahead = forecast_horizon)

  # Store forecasts and actual values
  forecasts[i, ] <- as.numeric(fitted(forecast))
  sigma_forecasts[i, ] <- as.numeric(sigma(forecast))
  actual_values[i, ] <- returns[forecast_start_index:forecast_end_index]

  print(i)
}

# Calculate the mean forecast error for the 5-step ahead forecasts
forecast_errors <- forecasts - actual_values
mean_forecast_error <- colMeans(forecast_errors, na.rm = TRUE)

# Print the forecast errors
cat("Mean Forecast Error for each step ahead:\n")
print(mean_forecast_error)

# Compare the last 5-step ahead forecast to the actual values
last_forecast <- forecasts[n_rolls, ]
last_sigma_forecast <- sigma_forecasts[n_rolls, ]
last_actual <- actual_values[n_rolls, ]

cat("\nLast 5-step Ahead Forecast:\n")
print(last_forecast)
cat("\nLast 5-step Sigma Forecast:\n")
print(last_sigma_forecast)
cat("\nLast 5-step Actual Values:\n")
print(last_actual)

# Optional: Visualize the comparison for the last forecast
plot(last_actual, type = 'l', col = 'red', lwd = 2, ylab = "Returns", xlab = "Steps Ahead",
     main = "Last 5-step Ahead Forecast vs Actual Values")
lines(last_forecast, col = 'blue', lwd = 2)
legend("topright", legend = c("Actual", "Forecasted"), col = c("red", "blue"), lty = 1, lwd = 2)

# Optional: Visualize the sigma forecasts
plot(last_sigma_forecast, type = 'l', col = 'green', lwd = 2, ylab = "Sigma", xlab = "Steps Ahead",
     main = "Last 5-step Ahead Sigma Forecasts")
par(mfrow = c(1,1))
residuals = forecast_errors[,5]/sigma_forecasts[,5]
plot(forecast_errors[,5]/sigma_forecasts[,5],type = 'l')
plot(forecast_errors[,1]/sigma_forecasts[,1],type = 'l')
# Calculate absolute residuals
abs_residuals <- abs(residuals)

# Compute the ACF
acf_data <- Acf(abs_residuals, plot = FALSE)
acf(abs(residuals))
library(forecast)
?Acf
library(ggplot2)

# Convert ACF data to a data frame
acf_df <- data.frame(
  lag = acf_data$lag,
  acf = acf_data$acf
)

# Create the ACF plot using ggplot2
ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.05, -0.05), linetype = "dashed", color = "red") +
  labs(title = "ACF of Absolute Residuals with 5 step forcast",
       x = "Lag",
       y = "ACF") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

mean_forecast_error <- colMeans(forecast_errors, na.rm = TRUE)
plot(forecast_errors[,5]/sigma_forecasts[,5],type='l')
plot(residuals, type = 'l')
# Print the forecast errors
plot(returns)
cat("Mean Forecast Error for each step ahead:\n")
print(mean_forecast_error)
acf(abs(forecast_errors[,3]))
cor(residuals[-length(residuals)],residuals[-1])
acf(abs(returns))
sd(residuals)

acf_data <- Acf(abs(data), plot = FALSE)
acf(abs(data))

# Convert ACF data to a data frame
acf_df <- data.frame(
  lag = acf_data$lag,
  acf = acf_data$acf
)

# Create the ACF plot using ggplot2
ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = c(0.05, -0.05), linetype = "dashed", color = "red") +
  labs(title = "ACF of Absolute Values",
       x = "Lag",
       y = "ACF") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


library(evir)


# Generate data for GPD numerical stability
a <- GPD_numerical_stability(residuals, thresholds = seq(quantile(residuals, .90), quantile(residuals, .99), length.out = 1000))

# Create data frames for plotting
df_scale <- data.frame(
  thresholds = a$thresholds,
  scale_mle = a$scale_mle,
  ci_upper_scale = a$scale_mle + 1.96 * a$scale_se,
  ci_lower_scale = a$scale_mle - 1.96 * a$scale_se
)

df_shape <- data.frame(
  thresholds = a$thresholds,
  shape_mle = a$shape_mle,
  ci_upper_shape = a$shape_mle + 1.96 * a$shape_se,
  ci_lower_shape = a$shape_mle - 1.96 * a$shape_se
)

# Generate data for mean past thresholds
mean_past_threshold_values <- mean_past_threshold(residuals, thresholds = seq(quantile(residuals, .90), quantile(residuals, .99999), length.out = 10000))
data_mean_past <- data.frame(
  thresholds = mean_past_threshold_values$thresholds,
  mean_past_threshold = mean_past_threshold_values$mean_past_threshold_list
)

# Create ggplot for Scale Parameter
p1 <- ggplot(df_scale, aes(x = thresholds, y = scale_mle)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = ci_lower_scale, ymax = ci_upper_scale), fill = "black", alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  labs(y = "Scale MLE", x = "Threshold", title = "Scale MLE with 95% CI and Regression Line") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )

print(p1)
data_mean_past$thresholds
model <- lm(scale_mle ~ thresholds, data = df_scale)
mean(df_shape$shape_mle)
# Create ggplot for Shape Parameter
p2 <- ggplot(df_shape, aes(x = thresholds, y = shape_mle)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = ci_lower_shape, ymax = ci_upper_shape), fill = "black", alpha = 0.2) +
  annotate("segment", x = min(df_shape$thresholds), xend = max(df_shape$thresholds), y = mean(df_shape$shape_mle), yend = mean(df_shape$shape_mle),
           color = "black", linetype = "dashed", size = 1) +
  labs(y = "Shape MLE", x = "Threshold", title = "Shape MLE with 95% CI and Mean Line") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )
print(p2)
# Create ggplot for Mean Past Thresholds
p3 <- ggplot(data_mean_past, aes(x = thresholds, y = mean_past_threshold)) +
  geom_line(color = "black", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed", size = 1) +
  labs(title = "Mean Past Thresholds with Linear Regression Line",
       x = "Thresholds",
       y = "Mean Past Threshold") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )

quantile_95 <- quantile(residuals, 0.95)
quantile_99 <- quantile(residuals, 0.99)

p4 <- ggplot(data.frame(x = seq_along(residuals), residuals = residuals), aes(x = x, y = residuals)) +
  geom_line(color = "black", size = 0.3) +  # Set line size to 0.5 for thinner lines
  annotate("segment", x = min(seq_along(residuals)), xend = max(seq_along(residuals)),
           y = quantile_95, yend = quantile_95, color = "blue", linetype = "dashed", size = .8) +
  annotate("segment", x = min(seq_along(residuals)), xend = max(seq_along(residuals)),
           y = quantile_99, yend = quantile_99, color = "red", linetype = "dashed", size = .8) +
  labs(title = "Residuals Plot with Quantile Lines", x = "Index", y = "Residuals") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )

print(p4)
# Print all plots
print(p1)
print(p2)
print(p3)
print(p4)

# Assuming 'returns' is your numeric vector of returns
# Compute absolute returns
abs_returns <- abs(residuals)

# Compute ACF
acf_result <- acf(abs_returns, plot = FALSE)  # Compute ACF without plotting

# Prepare data for ggplot
acf_data <- data.frame(
  lag = acf_result$lag,
  acf = acf_result$acf
)
p_acf <- ggplot(acf_data, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "grey70", color = "black") +  # Light grey bars with black borders
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +  # Reference line at y = 0
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed") +  # Dotted line at y = 0.05
  geom_hline(yintercept = -0.05, color = "black", linetype = "dashed") +  # Dotted line at y = -0.05
  annotate("text", x = 40, y = 0.05, label = "0.05", hjust = 1.1, vjust = -0.5, color = "black", size = 4) +  # Add text annotation at y = 0.05
  annotate("text", x = 40, y = -0.05, label = "-0.05", hjust = 1.1, vjust = 1.5, color = "black", size = 4) +  # Add text annotation at y = -0.05
  labs(title = "ACF of Absolute Returns", x = "Lag", y = "ACF") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text for better readability
    panel.border = element_blank(),  # Remove panel border
    plot.border = element_blank(),   # Remove plot border
    axis.line = element_blank(),
  )


p_acf <- ggplot(acf_data, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "grey70", color = "black") +  # Light grey bars with black borders
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +  # Reference line at y = 0
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed") +  # Dotted line at y = 0.05
  geom_hline(yintercept = -0.05, color = "black", linetype = "dashed") +  # Dotted line at y = -0.05
  labs(title = "ACF of Absolute Returns", x = "Lag", y = "ACF") +
  scale_y_continuous(breaks = c(-0.05,0,0.05,0.25,0.5,0.75,1)) +  # Add custom y-axis breaks
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text for better readability
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    panel.border = element_blank(),  # Remove panel border
    plot.border = element_blank(),   # Remove plot border
    axis.line = element_blank()      # Remove axis lines
  )

print(p_acf)
# Print the ACF plot
print(p_acf)



































































library(rugarch)
library(quantmod)

# Specify the GARCH(1,1) model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0)),
                   distribution.model = "norm")

# Fit the model
fit <- ugarchfit(spec = spec, data = returns)
print(fit)


conditional_volatility <- sigma(fit)
plot(conditional_volatility, main = "Conditional Volatility", ylab = "Volatility", xlab = "Date")

library(ggplot2)
# Extract residuals
residuals <- as.numeric((residuals(fit)))/as.numeric(sigma(fit))
#residuals <- as.numeric(residuals(fit))
abs_residuals <- abs(residuals)
plot(residuals, type = 'l')
# Plot correlograms for the raw data and their absolute values
par(mfrow = c(2, 2))
acf(returns, main = "Correlogram of Returns")
acf(abs(returns), main = "Correlogram of Absolute Returns")

# Plot correlograms for the residuals and their absolute values
acf(residuals, main = "Correlogram of Residuals")
acf(abs_residuals, main = "Correlogram of Absolute Residuals")
library(ggplot2)
# Create Q-Q plot using ggplot2
qq_plot <- ggplot(data = data.frame(residuals = residuals), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of Residuals Against Normal Distribution") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

# Print the Q-Q plot
print(qq_plot)
length(residuals)
library(ismev)
z<-gpd.fit(residuals,threshold=residuals[order(residuals,decreasing = TRUE)][110])
plot(residuals)
gpd.diag(z)




par(mfrow = c(1, 1))
residuals_ordered = residuals[order(residuals)]
# Initialize tracker as an empty list
tracker = data.frame(residuals = residuals_ordered, after_life = rep(0,length(residuals_ordered)))
# Enumerate residuals
for (i in 1:length(residuals_ordered)) {
  tracker$after_life[i] = mean(residuals_ordered[residuals_ordered>residuals_ordered[i]]-residuals_ordered[i])
}
plot(tracker,type='l')
abline(v=quantile(residuals_ordered,.90))
abline(v=quantile(residuals_ordered,.95))
abline(v=quantile(residuals_ordered,.99))

quantile_90 <- quantile(residuals_ordered, 0.90)
quantile_99 <- quantile(residuals_ordered, 0.99)

subset_tracker <- tracker[tracker$residuals > quantile_90 & tracker$residuals < quantile_99, ]
subset_tracker$index = 1:length(subset_tracker$residuals)
# Plot the subset

# Fit a linear model to the subsetted data
lm_model <- lm(subset_tracker$after_life ~ subset_tracker$residuals)

# Plot the subsetted data
plot(subset_tracker$residuals,subset_tracker$after_life, type = 'l', main = "Linear Regression on Residuals",
     xlab = "Index", ylab = "Residuals")

# Add the fitted regression line to the plot
abline(lm_model, col = "red", lwd = 2)






























library(evir)

var(residuals)
# Generate data for GPD numerical stability
a <- GPD_numerical_stability(residuals, thresholds = seq(quantile(residuals, .90), quantile(residuals, .99), length.out = 1000))

# Create data frames for plotting
df_scale <- data.frame(
  thresholds = a$thresholds,
  scale_mle = a$scale_mle,
  ci_upper_scale = a$scale_mle + 1.96 * a$scale_se,
  ci_lower_scale = a$scale_mle - 1.96 * a$scale_se
)

df_shape <- data.frame(
  thresholds = a$thresholds,
  shape_mle = a$shape_mle,
  ci_upper_shape = a$shape_mle + 1.96 * a$shape_se,
  ci_lower_shape = a$shape_mle - 1.96 * a$shape_se
)

# Generate data for mean past thresholds
mean_past_threshold_values <- mean_past_threshold(residuals, thresholds = seq(quantile(residuals, .90), quantile(residuals, .99999), length.out = 10000))
data_mean_past <- data.frame(
  thresholds = mean_past_threshold_values$thresholds,
  mean_past_threshold = mean_past_threshold_values$mean_past_threshold_list
)

# Create ggplot for Scale Parameter
p1 <- ggplot(df_scale, aes(x = thresholds, y = scale_mle)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = ci_lower_scale, ymax = ci_upper_scale), fill = "black", alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  labs(y = "Scale MLE", x = "Threshold", title = "Scale MLE with 95% CI and Regression Line")

print(p1)


# Create ggplot for Shape Parameter
p2 <- ggplot(df_shape, aes(x = thresholds, y = shape_mle)) +
  geom_line(color = "black", size = 1) +
  geom_ribbon(aes(ymin = ci_lower_shape, ymax = ci_upper_shape), fill = "black", alpha = 0.2) +
  geom_hline(yintercept = mean(df_shape$shape_mle), color = "grey40", linetype = "dashed", size = 1) +
  labs(y = "Shape MLE", x = "Threshold", title = "Shape MLE with 95% CI and Mean Line") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )
print(p2)
# Create ggplot for Mean Past Thresholds
p3 <- ggplot(data_mean_past, aes(x = thresholds, y = mean_past_threshold)) +
  geom_line(color = "black", size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed", size = 1) +
  labs(title = "Mean Past Thresholds with Linear Regression Line",
       x = "Thresholds",
       y = "Mean Past Threshold") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )

quantile_95 <- quantile(residuals, 0.95)
quantile_99 <- quantile(residuals, 0.99)

p4 <- ggplot(data.frame(x = seq_along(residuals), residuals = residuals), aes(x = x, y = residuals)) +
  geom_line(color = "black", size = 0.3) +  # Set line size to 0.5 for thinner lines
  geom_hline(yintercept = quantile_95, color = "grey50", linetype = "dashed", size = 1) +
  geom_hline(yintercept = quantile_99, color = "grey30", linetype = "dashed", size = 1) +
  labs(title = "Residuals Plot with Quantile Lines", x = "Index", y = "Residuals") +
  coord_cartesian(ylim = c(-10, 10)) +  # Adjust y-axis limits here
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )

print(p4)
# Print all plots
print(p1)
print(p2)
print(p3)
print(p4)

# Assuming 'returns' is your numeric vector of returns
# Compute absolute returns
abs_returns <- abs(residuals)

# Compute ACF
acf_result <- acf(abs_returns, plot = FALSE)  # Compute ACF without plotting

# Prepare data for ggplot
acf_data <- data.frame(
  lag = acf_result$lag,
  acf = acf_result$acf
)
p_acf <- ggplot(acf_data, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "grey70", color = "black") +  # Light grey bars with black borders
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +  # Reference line at y = 0
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed") +  # Dotted line at y = 0.05
  geom_hline(yintercept = -0.05, color = "black", linetype = "dashed") +  # Dotted line at y = -0.05
  annotate("text", x = 40, y = 0.05, label = "0.05", hjust = 1.1, vjust = -0.5, color = "black", size = 3) +  # Add text annotation at y = 0.05
  annotate("text", x = 40, y = -0.05, label = "-0.05", hjust = 1.1, vjust = 1.5, color = "black", size = 3) +  # Add text annotation at y = -0.05
  labs(title = "ACF of Absolute Returns", x = "Lag", y = "ACF") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text for better readability
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black")
  )

# Print the ACF plot
print(p_acf)
