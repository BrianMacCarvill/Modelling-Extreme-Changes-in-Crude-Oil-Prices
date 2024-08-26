library(ismev)

spec_AR1_GARCH11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                               mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                               distribution.model = "norm")

spec_AR0_GARCH11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                               mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                               distribution.model = "norm")



data = as.numeric(returns)

h_values <- c(1, 5, 10, 30)

WTI_AR1_GARCH11_results <- list()

for (h in h_values) {
  result_name <- paste("WTI_AR1_GARCH11_n1000_k100_h", h, "_ES", sep = "")

  WTI_AR1_GARCH11_results[[result_name]] <- Conditional_Expectation_Backtesting(
    data,
    spec_AR1_GARCH11,
    quantiles = c(.95, .99, .995),
    n = 1000,
    k = 100,
    h = h
  )
}

WTI_AR0_GARCH11_results <- list()

for (h in h_values) {
  result_name <- paste("WTI_AR0_GARCH11_n1000_k100_h", h, "_ES", sep = "")

  WTI_AR0_GARCH11_results[[result_name]] <- Conditional_Expectation_Backtesting(
    data,
    spec_AR0_GARCH11,
    quantiles = c(.95, .99, .995),
    n = 1000,
    k = 100,
    h = h
  )
}

Brent_data = as.numeric(returns_Brent)

Brent_AR1_GARCH11_results <- list()

for (h in h_values) {
  result_name <- paste("Brent_AR1_GARCH11_n1000_k100_h", h, "_ES", sep = "")

  Brent_AR1_GARCH11_results[[result_name]] <- Conditional_Expectation_Backtesting(
    Brent_data,
    spec_AR1_GARCH11,
    quantiles = c(.95, .99, .995),
    n = 1000,
    k = 100,
    h = h
  )
}


Brent_AR0_GARCH11_results <- list()

for (h in h_values) {
  result_name <- paste("Brent_AR0_GARCH11_n1000_k100_h", h, "_ES", sep = "")

  Brent_AR0_GARCH11_results[[result_name]] <- Conditional_Expectation_Backtesting(
    Brent_data,
    spec_AR0_GARCH11,
    quantiles = c(.95, .99, .995),
    n = 1000,
    k = 100,
    h = h
  )
}
