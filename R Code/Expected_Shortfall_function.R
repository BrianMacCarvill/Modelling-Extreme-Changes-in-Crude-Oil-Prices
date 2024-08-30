Conditional_Expectation_Backtesting <- function(returns, spec, quantiles = c(.95,.99), n = 1000, k = 100, h = 1, timer = TRUE, test = TRUE) {

  Conditional_Expectation_GPD <- function(u, Z_q, beta, xi){
    if (xi >= 1) {
      stop("xi > 1")
    }
    return(
      Z_q/(1-xi) + (beta - xi*u)/(1 - xi)
    )
  }

  return_value_function <- function(n,k,q,beta,xi){
    z_q = (beta/xi)*(((1-q)/(k/n))^(-xi)-1)
    return(z_q)
  }

  returns <- na.omit(returns)

  test_length <- length(returns) - n - h + 1

  num_quantiles <- length(quantiles)

  tracker <- data.frame(matrix(0, nrow = test_length, ncol = num_quantiles))

  Sum_values_tracker <- data.frame(matrix(0, nrow = test_length, ncol = num_quantiles))

  mu_hat <- numeric(test_length)
  sigma_hat <- numeric(test_length)

  colnames(tracker) <- paste0("quantile", quantiles)

  colnames(Sum_values_tracker) <- paste0("quantile", quantiles)

  for (i in 1:test_length) {
    current_data <- as.numeric(returns[i:(i + n - 1)])

    fit <- ugarchfit(spec = spec, data = current_data, solver = "hybrid",
                     solver.control = list(trace = 0, maxit = 3, tol = 1e-8, reltol = 1e-8))

    forecast <- ugarchforecast(fit, n.ahead = h)

    residuals <- as.numeric(residuals(fit)) / as.numeric(sigma(fit))

    z_k1 <- sort(residuals, decreasing = TRUE)[k + 1]

    z <- gpd.fit(residuals, threshold = z_k1, show = FALSE)

    quantile_values <- numeric(num_quantiles)
    for (j in seq_along(quantiles)) {
      quantile_values[j] <- return_value_function(n = n, k = k, q = quantiles[j], beta = z$mle[1], xi = z$mle[2]) + z_k1
    }

    mu_hat[i] <- forecast@forecast$seriesFor[h]
    sigma_hat[i] <- forecast@forecast$sigmaFor[h]

    x_t <- numeric(num_quantiles)
    Sum_values <- numeric(num_quantiles)
    for (j in seq_along(quantiles)) {
      x_t[j] <- quantile_values[j] * sigma_hat[i] + mu_hat[i]
      Sum_values[j] = Conditional_Expectation_GPD(z_k1, quantile_values[j], z$mle[1], z$mle[2]) * sigma_hat[i] + mu_hat[i]
    }
    tracker[i, ] <- x_t
    Sum_values_tracker[i, ] = Sum_values
    if (timer){
      print(paste(i, "out of", test_length))
    }
  }
  num_exceedances <- numeric(num_quantiles)

  exceedances_tracker <- vector("list", num_quantiles)

  names(exceedances_tracker) <- paste0("quantile_", quantiles)

  exceedances_tracker2 <- vector("list", num_quantiles)

  names(exceedances_tracker2) <- paste0("quantile_", quantiles)

  test_returns <- as.vector(returns[(n+h):(length(returns))])

  for (i in 1:num_quantiles){
    exceedances <- test_returns > tracker[, i]
    exceedances_tracker[[i]] <- (test_returns[exceedances] - Sum_values_tracker[exceedances,i]) / sigma_hat[exceedances]
    exceedances_tracker2[[i]] <- (test_returns[exceedances] - Sum_values_tracker[exceedances,i])
  }
  return(list(spec = spec,
              h = h,
              n = n,
              k = k,
              quantiles = quantiles,
              tracker = tracker,
              exceedances_tracker = exceedances_tracker,
              exceedances_tracker2 = exceedances_tracker2))

}
