library(forecast)
library(ggplot2)
library(evir)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

initial_fit <- ugarchfit(spec = spec, data = returns)

initial_params <- coef(initial_fit)

mu <- as.numeric(initial_params["mu"])
ar1 <- as.numeric(initial_params["ar1"])
omega <- as.numeric(initial_params["omega"])
alpha1 <- as.numeric(initial_params["alpha1"])
beta1 <- as.numeric(initial_params["beta1"])

cat("phi_0:", mu, "\n")
cat("phi_1:", ar1, "\n")
cat("alpha_0:", omega, "\n")
cat("alpha_1:", alpha1, "\n")
cat("beta_1:", beta1, "\n")

fixed_params <- list(
  mu = mu,
  ar1 = ar1,
  omega = omega,
  alpha1 = alpha1,
  beta1 = beta1
)

n <- 1000  # Replacing window_size
h <- 5     # Replacing forecast_horizon
n_rolls <- length(returns) - n - h + 1

forecasts <- matrix(NA, ncol = h, nrow = n_rolls)
sigma_forecasts <- matrix(NA, ncol = h, nrow = n_rolls)
actual_values <- matrix(NA, ncol = h, nrow = n_rolls)

fixed_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = fixed_params
)

for (i in 1:n_rolls) {
  start_index <- i
  end_index <- start_index + n - 1
  forecast_start_index <- end_index + 1
  forecast_end_index <- forecast_start_index + h - 1

  forecast <- ugarchforecast(fixed_spec, data = returns[start_index:end_index], n.ahead = h)

  forecasts[i, ] <- as.numeric(fitted(forecast))
  sigma_forecasts[i, ] <- as.numeric(sigma(forecast))
  actual_values[i, ] <- returns[forecast_start_index:forecast_end_index]

  print(i)
}

par(mfrow = c(1,1))

forecast_errors <- forecasts - actual_values
residuals <- forecast_errors[,1] / sigma_forecasts[,1]

a <- GPD_numerical_stability(residuals, thresholds = seq(quantile(residuals, .90), quantile(residuals, .99), length.out = 1000))

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

mean_past_threshold_values <- mean_past_threshold(residuals, thresholds = seq(quantile(residuals, .90), quantile(residuals, .99999), length.out = 10000))
data_mean_past <- data.frame(
  thresholds = mean_past_threshold_values$thresholds,
  mean_past_threshold = mean_past_threshold_values$mean_past_threshold_list
)

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
model <- lm(scale_mle ~ thresholds, data = df_scale)

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

quantile_95 <- quantile(residuals, 0.95)
quantile_99 <- quantile(residuals, 0.99)

p3 <- ggplot(data.frame(x = seq_along(residuals), residuals = residuals), aes(x = x, y = residuals)) +
  geom_line(color = "black", size = 0.3) +
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

print(p3)

abs_returns <- abs(residuals)

acf_result <- acf(abs_returns, plot = FALSE)

acf_data <- data.frame(
  lag = acf_result$lag,
  acf = acf_result$acf
)

p_acf <- ggplot(acf_data, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "grey70", color = "black") +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed") +
  geom_hline(yintercept = -0.05, color = "black", linetype = "dashed") +
  labs(title = "ACF of Absolute Returns", x = "Lag", y = "ACF") +
  scale_y_continuous(breaks = c(-0.05,0,0.05,0.25,0.5,0.75,1)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black")
  )

print(p_acf)
