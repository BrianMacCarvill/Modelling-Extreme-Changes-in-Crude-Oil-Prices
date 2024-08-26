library(ismev)
Conditional_Expectation_Backtesting <- function(returns, spec, quantiles = c(.95,.99), n = 1000, k = 100, h = 1, timer = TRUE, test = TRUE) {

  Conditional_Expectation_GPD <- function(u, Z_q, beta, xi){
    if (xi >= 1) {
      stop("xi must be less than 1 for the expectation to be finite.")
    }
    return(
      Z_q/(1-xi) + (beta - xi*u)/(1 - xi)
    )
  }

  return_value_function = function(n,k,q,beta,xi){
    z_q = (beta/xi)*(((1-q)/(k/n))^(-xi)-1)
    return(z_q)
  }

  returns <- na.omit(returns)

  test_length <- length(returns) - n - h + 1

  num_quantiles <- length(quantiles)

  # Initialize the tracker data frame with the appropriate number of columns
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
  if (test){
    num_exceedances <- numeric(num_quantiles)

    exceedances_tracker <- vector("list", num_quantiles)

    names(exceedances_tracker) <- paste0("quantile_", quantiles)

    exceedances_tracker2 <- vector("list", num_quantiles)

    names(exceedances_tracker2) <- paste0("quantile_", quantiles)

    test_returns = as.vector(returns[(n+h):(length(returns))])

    for (i in 1:num_quantiles){
      exceedances <- test_returns > tracker[, i]
      exceedances_tracker[[i]] <- (test_returns[exceedances] - Sum_values_tracker[exceedances,i]) / sigma_hat[exceedances]
      exceedances_tracker2[[i]] <- (test_returns[exceedances] - Sum_values_tracker[exceedances,i])
    }
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

spec_AR1_GARCH11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                    distribution.model = "norm")

spec_constant_mean <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                    distribution.model = "norm")


data = as.numeric(returns)
WTI_AR1_GARCH11_n1000_k100_h1_ES = Conditional_Expectation_Backtesting(data, spec_AR1_GARCH11, quantiles = c(0.95,0.975,.99,.995), n = 1000, k = 100, h = 1)
WTI_AR1_GARCH11_n1000_k100_h5_ES = Conditional_Expectation_Backtesting(data, spec_AR1_GARCH11, quantiles = c(0.95, 0.975,.99,.995), n = 1000, k = 100, h = 5)
WTI_AR1_GARCH11_n1000_k100_h10_ES = Conditional_Expectation_Backtesting(data, spec_AR1_GARCH11, quantiles = c(0.95, 0.975,.99,.995), n = 1000, k = 100, h = 10)
Conditional_Expectation_h30 = Conditional_Expectation_Backtesting(data, spec_AR1_GARCH11, quantiles = c(0.95, 0.975,.99,.995), n = 1000, k = 50, h = 30)
WTI_Conditional_Expectation_h1_constantmean = Conditional_Expectation_Backtesting(data, spec_constant_mean, quantiles = c(0.95, 0.975,.99,.995), n = 1000, k = 100, h = 1)
paste0("WTI_AR1_GARCH11_n1000_k100_h",10,"_ES")
mean(WTI_AR1_GARCH11_n1000_k100_h5_ES$exceedances_tracker$quantile_0.95)
mean(WTI_AR1_GARCH11_n1000_k100_h5_ES$exceedances_tracker$quantile_0.99)
mean(WTI_AR1_GARCH11_n1000_k100_h5_ES$exceedances_tracker$quantile_0.995)
var(WTI_AR1_GARCH11_n1000_k100_h5_ES$exceedances_tracker$quantile_0.95)
var(WTI_AR1_GARCH11_n1000_k100_h5_ES$exceedances_tracker$quantile_0.99)
var(WTI_AR1_GARCH11_n1000_k100_h5_ES$exceedances_tracker$quantile_0.995)

mean_var = function(x){
  name = names(x$exceedances_tracker)
  for (i in 1:length(x$exceedances_tracker)){
    print(name[i])
    print(paste("mean",mean(x$exceedances_tracker[[i]])))
    print(paste("var",var(x$exceedances_tracker[[i]])))
    print("-------------------")
  }
}
mean_var(WTI_AR1_GARCH11_n1000_k100_h5_ES)
mean_var(WTI_Conditional_Expectation_h1_constantmean)

plot(Conditional_Expectation$exceedances_tracker$quantile_0.95,type='l')
acf(Conditional_Expectation$exceedances_tracker$quantile_0.99)
acf(abs(Conditional_Expectation$exceedances_tracker$quantile_0.95))



Conditional_Expectation$tracker$quantile0.95 - c$VaR_at_quantiles$quantile0.95[1:200]
# Setting up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2, 1) + 0.1, cex.lab = 1.2, cex.main = 1.4, cex.axis = 1.2)
par(mfrow = c(1,1))
mean(1*(Conditional_Expectation$tracker$quantile0.975 > data[1001:length(data)]))
# Define colors for each exceedance tracker for consistency
colors <- c("blue", "green", "orange", "purple")

# Plot each exceedances tracker with a horizontal line at y = 0
for (i in 1:4) {
  plot(a$exceedances_tracker[[i]], type = 'l', col = colors[i], lwd = 2,
       main = paste("Exceedances Tracker", i),
       ylab = "Value", xlab = "Index",
       cex.lab = 1.2, cex.main = 1.4, cex.axis = 1.2)
  abline(h = 0, col = "red", lwd = 2, lty = 2)
}

# Reset plotting area to a single plot
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2) + 0.1, cex.lab = 1.5, cex.main = 1.8, cex.axis = 1.3)

# Create a larger plot for the first exceedances tracker
plot(a$exceedances_tracker[[1]], type = 'l', col = colors[1], lwd = 2,
     main = "Exceedances Tracker 1",
     ylab = "Value", xlab = "Index",
     cex.lab = 1.5, cex.main = 1.8, cex.axis = 1.3)
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Optionally, add a legend if needed
legend("topright", legend = c("Exceedances Tracker 1"), col = colors[1], lwd = 2, lty = 1, cex = 1.3)

for (i in c(1,3)) print(paste(names(Conditional_Expectation$exceedances_tracker)[i],mean(Conditional_Expectation$exceedances_tracker[[i]]),var(Conditional_Expectation$exceedances_tracker[[i]]))
)
names(a$exceedances_tracker)
Conditional_Expectation$exceedances_tracker$quantile_0.99[Conditional_Expectation$exceedances_tracker$quantile_0.99<0]


plot(Conditional_Expectation$exceedances_tracker2$quantile_0.99)

mean(Conditional_Expectation$exceedances_tracker2$quantile_0.99)
var(Conditional_Expectation$exceedances_tracker2$quantile_0.99)


exceedances_tracker = Conditional_Expectation$exceedances_tracker

plot_data <- bind_rows(
  lapply(names(exceedances_tracker), function(name) {
    data <- exceedances_tracker[[name]]
    data_frame(Index = seq_along(data), Value = data, Tracker = name)
  })
)

lapply(names(exceedances_tracker), function(name) {
  data <- exceedances_tracker[[name]]
  data_frame(Index = seq_along(data), Value = data, Tracker = name)
})

p = ggplot(plot_data, aes(x = Index, y = Value, color = Tracker, group = Tracker)) +
  geom_line(size = .5) +  # Line width
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1.2) +  # Horizontal line
  facet_wrap(~ Tracker, scales = "free_y") +  # Arrange plots in a grid
  labs(title = "Exceedances Tracker", x = "Index", y = "Value") +  # Labels
  theme_minimal() +  # Clean theme
  theme(
    text = element_text(size = 12),  # Font size for axis and titles
    strip.text = element_text(size = 14),  # Font size for facet labels
    axis.title = element_text(size = 14),  # Font size for axis labels
    axis.text = element_text(size = 12),  # Font size for axis text
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "none"  # Remove legend
)

print(p)



# Function to create individual plots
create_plot <- function(data, tracker_name) {
  ggplot(data, aes(x = Index, y = Value)) +
    geom_line(color = "black", size = .5) +  # Line width
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1.2) +  # Horizontal line
    labs(title = tracker_name, x = "Index", y = "Value") +  # Labels
    theme_minimal() +  # Clean theme
    theme(
      text = element_text(size = 12),  # Font size for axis and titles
      axis.title = element_text(size = 14),  # Font size for axis labels
      axis.text = element_text(size = 12),  # Font size for axis text
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()  # Remove minor grid lines
    )
}

tracker_name = c("WTI ES difference (95% quantile)", "", "WTI ES difference (99% quantile)", " ")
for (i in seq_along(exceedances_tracker)) {
  data <- data.frame(Index = seq_along(exceedances_tracker[[i]]), Value = exceedances_tracker[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}
mean(exceedances_tracker$quantile_0.95)
t.test(exceedances_tracker$quantile_0.95, mu = 0, alternative = "two.sided")
t.test(exceedances_tracker$quantile_0.99, mu = 0, alternative = "two.sided")
