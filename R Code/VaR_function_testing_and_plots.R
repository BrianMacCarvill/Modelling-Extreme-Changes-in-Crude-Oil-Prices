library(quantmod)
library(xts)
library(zoo)
library(dplyr)
library(ggplot2)
library(ismev)

EVT_GARCH_future_steps_prediction <- function(returns, spec, quantiles = c(.95,.99,.995), n = 1000, k = 100, h = 1, timer = TRUE, test = TRUE) {

  simple_function = function(returns=returns,quantiles,steps=1, n = 1000){
    test = 1*(returns[(n+steps):(length(returns))]>quantiles)
    breaches = sum(test)
    percenage_breach = mean(test)
    return(list(
      breaches = breaches,
      percenage_breach = percenage_breach
    ))
  }

  return_value_function = function(n,k,q,beta,xi){
    z_q = (beta/xi)*(((1-q)/(k/n))^(-xi)-1)
    return(z_q)
  }

  returns <- na.omit(returns)

  test_length <- length(returns) - n - h + 1

  num_quantiles <- length(quantiles)

  tracker <- data.frame(matrix(0, nrow = test_length, ncol = num_quantiles))

  colnames(tracker) <- paste0("quantile", quantiles)

  for (i in 1:test_length) {
    current_data <- as.numeric(returns[i:(i + n - 1)])

    fit <- ugarchfit(spec = spec, data = current_data, solver = "hybrid",
                     solver.control = list(trace = 0, maxit = 3, tol = 1e-8, reltol = 1e-8))

    forecast <- ugarchforecast(fit, n.ahead = h)

    residuals <- as.numeric((residuals(fit))) / as.numeric(sigma(fit))

    z_k1 <- sort(residuals, decreasing = TRUE)[k + 1]

    z <- gpd.fit(residuals, threshold = z_k1, show = FALSE)

    quantile_values <- numeric(num_quantiles)
    for (j in seq_along(quantiles)) {
      quantile_values[j] <- return_value_function(n = n, k = k, q = quantiles[j], beta = z$mle[1], xi = z$mle[2]) + z_k1
    }

    mu_hat <- forecast@forecast$seriesFor[h]
    sigma_hat <- forecast@forecast$sigmaFor[h]

    x_t <- numeric(num_quantiles)
    for (j in seq_along(quantiles)) {
      x_t[j] <- quantile_values[j] * sigma_hat + mu_hat
    }

    tracker[i, ] <- x_t
    if (timer){
      print(paste(i, "out of", test_length))
    }
  }
  if (test){
    breaches_tracker = numeric(num_quantiles)
    percentage_tracker = numeric(num_quantiles)
    for (i in 1:num_quantiles){
      breaches = simple_function(returns,tracker[,i],steps = h, n = n)
      breaches_tracker[i] = breaches$breaches[1]
      percentage_tracker[i] = breaches$percenage_breach[1]
    }
    return(list(
      VaR_at_quantiles = tracker,
      spec = spec,
      h = h,
      n = n,
      k = k,
      quantiles = quantiles,
      breaches = breaches_tracker,
      percentage_breaches = percentage_tracker
    ))
  } else{
    return(tracker)
  }
}

data <- as.numeric(returns)

spec_AR1_GARCH11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                    distribution.model = "norm")

spec_AR0_GARCH11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                                 mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                                 distribution.model = "norm")

WTI_AR1_GARCH11_n1000_k100_h1 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 1, timer = TRUE)
WTI_AR1_GARCH11_n1000_k100_h5 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 5, timer = TRUE)
WTI_AR1_GARCH11_n1000_k100_h10 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 10, timer = TRUE)
WTI_AR1_GARCH11_n1000_k100_h30 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 30, timer = TRUE)

WTI_AR0_GARCH11_n1000_k100_h1 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 1, timer = TRUE)
WTI_AR0_GARCH11_n1000_k100_h5 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 5, timer = TRUE)
WTI_AR0_GARCH11_n1000_k100_h10 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 10, timer = TRUE)
WTI_AR0_GARCH11_n1000_k100_h30 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 30, timer = TRUE)

WTI_AR1_GARCH11_n500_k50_h1 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 1, timer = TRUE)
WTI_AR1_GARCH11_n500_k50_h5 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 5, timer = TRUE)
WTI_AR1_GARCH11_n500_k50_h10 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 10, timer = TRUE)
WTI_AR1_GARCH11_n500_k50_h30 = EVT_GARCH_future_steps_prediction(data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 30, timer = TRUE)

WTI_AR0_GARCH11_n500_k50_h1 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 1, timer = TRUE)
WTI_AR0_GARCH11_n500_k50_h5 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 5, timer = TRUE)
WTI_AR0_GARCH11_n500_k50_h10 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 10, timer = TRUE)
WTI_AR0_GARCH11_n500_k50_h30 = EVT_GARCH_future_steps_prediction(data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 500, k = 50, h = 30, timer = TRUE)


Brent_data = as.numeric(returns_Brent)

Brent_AR1_GARCH11_n1000_k100_h1 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 1, timer = TRUE)
Brent_AR1_GARCH11_n1000_k100_h5 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 5, timer = TRUE)
Brent_AR1_GARCH11_n1000_k100_h10 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 10, timer = TRUE)
Brent_AR1_GARCH11_n1000_k100_h30 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR1_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 30, timer = TRUE)

Brent_AR0_GARCH11_n1000_k100_h1 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 1, timer = TRUE)
Brent_AR0_GARCH11_n1000_k100_h5 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 5, timer = TRUE)
Brent_AR0_GARCH11_n1000_k100_h10 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 10, timer = TRUE)
Brent_AR0_GARCH11_n1000_k100_h30 = EVT_GARCH_future_steps_prediction(Brent_data, spec_AR0_GARCH11, quantiles = c(.95, .99, .995), n = 1000, k = 100, h = 30, timer = TRUE)


returns_df <- data.frame(
  Index = index(returns),
  Value = coredata(returns)
)

start_date <- index(returns)[n + h]
quantiles_data <- data.frame(
  Index = index(returns),
  Quantile_0.95 = c(rep(NA,1000),WTI_AR1_GARCH11_n1000_k100_h1$VaR_at_quantiles$quantile0.95),
  Quantile_0.99 = c(rep(NA,1000),WTI_AR1_GARCH11_n1000_k100_h1$VaR_at_quantiles$quantile0.99)
)

plot_data <- full_join(returns_df, quantiles_data, by = "Index")

p <- ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = daily.returns), color = "black", linewidth = .5) +
  geom_line(aes(y = Quantile_0.95), color = "blue", linetype = "dashed", size = .5) +
  geom_line(aes(y = Quantile_0.99), color = "red", linetype = "dashed", size = .5) +
  labs(title = "VaR at Quantiles",
       x = "Index",
       y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

print(p)



