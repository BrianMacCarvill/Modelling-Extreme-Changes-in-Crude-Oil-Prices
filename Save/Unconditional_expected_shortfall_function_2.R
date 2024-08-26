library(ismev)
library(evd)

data = as.numeric(returns$daily.returns)
Unconditional_expected_shortfall_values = Unconditional_expected_shortfall(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 1)
Unconditional_expected_shortfall_values_5 = Unconditional_expected_shortfall(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 5)
Unconditional_expected_shortfall_values_10 = Unconditional_expected_shortfall(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 10)
Unconditional_expected_shortfall_values_30 = Unconditional_expected_shortfall(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 30)













library(zoo)

# Calculate rolling variance with a window size of, for example, 30 days
window_size <- 30
rolling_variance <- rollapply(data, width = window_size, FUN = var, by.column = TRUE, fill = NA, align = "right")

# Plot the rolling variance
plot(rolling_variance, type = "l", col = "blue",
     main = "Rolling Variance of Daily Returns",
     xlab = "Time", ylab = "Rolling Variance")

returns$daily.returns

acf(abs(data))
mean(result)
mean(result[which(result[1:(length(result)-1)]==1)+1])
mean(result2[which(result2[1:(length(result2)-2)]==1)+2])

create_plot(rolling_variance)

ggplot(rolling_variance) +
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

window_size <- 100
rolling_variance <- rollapply(data, width = window_size, FUN = var, by.column = TRUE, fill = NA, align = "right")

# Convert rolling_variance to a data frame
rolling_variance_df <- data.frame(Index = seq_along(rolling_variance), Value = rolling_variance)

# Plot with ggplot
ggplot(rolling_variance_df, aes(x = Index, y = Value)) +
  geom_line(color = "black", size = 0.5) +  # Line width
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1.2) +  # Horizontal line
  labs(title = "Rolling Variance", x = "Index", y = "Value") +  # Labels
  theme_minimal() +  # Clean theme
  theme(
    text = element_text(size = 12),  # Font size for axis and titles
    axis.title = element_text(size = 14),  # Font size for axis labels
    axis.text = element_text(size = 12),  # Font size for axis text
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()    # Remove minor grid lines
  )


min(na.omit(rolling_variance))












library(ggplot2)
library(dplyr)
library(xts)
library(zoo)

# Convert `returns` xts or zoo object to a data frame
returns_df <- data.frame(
  Index = index(returns),
  Value = coredata(returns)
)

# Assuming `n` and `h` correspond to date offsets, adjust this conversion accordingly
start_date <- index(returns)[n + h]
quantiles_data <- data.frame(
  Index = index(returns),
  Quantile_0.95 = c(rep(NA,1000),Unconditional_expected_shortfall_values$quantiles_tracker$quantile0.95),
  Quantile_0.99 = c(rep(NA,1000),Unconditional_expected_shortfall_values$quantiles_tracker$quantile0.99)
)

# Merge the data frames by Index
plot_data <- full_join(returns_df, quantiles_data, by = "Index")

plot_data = plot_data[-(1:1000),]

plot_data <- na.omit(plot_data)

# Create the plot
p <- ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = daily.returns), color = "black", linewidth = .5) +  # Main data
  geom_line(aes(y = Quantile_0.95), color = "blue", linetype = "dashed", size = .8) +  # Quantile 0.95
  geom_line(aes(y = Quantile_0.99), color = "red", linetype = "dashed", size = .8) +  # Quantile 0.99
  labs(title = "VaR at Quantiles",
       x = "Index",
       y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Print the plot
print(p)

Quantile_0.99 = na.omit(plot_data$Quantile_0.99)
print(str(plot_data))
plot_data[-(1:1000),]
length(Quantile_0.99)
plot(Quantile_0.99)
length(plot_data$Index)


plot(b$VaR_at_quantiles$quantile0.95[1:200] - c$VaR_at_quantiles$quantile0.95)
plot(c$VaR_at_quantiles$quantile0.95, type = 'l')
lines(b$VaR_at_quantiles$quantile0.95[1:200], col = 'red')


























Unconditional_expected_shortfall_real = function(returns, n = 1000, k = 100, quantiles = c(.95,.99), h = 1, test = TRUE){
  simple_function = function(returns=returns,quantiles,steps=1, n = 1000){
    test = 1*(returns[(n+steps):(length(returns))]>quantiles)
    breaches = sum(test)
    percenage_breach = mean(test)
    return(list(
      breaches = breaches,
      percenage_breach = percenage_breach
    ))
  }

  Conditional_Expectation_GPD <- function(u, Z_q, beta, xi){
    if (xi >= 1) {
      stop("xi must be less than 1 for the expectation to be finite.")
    }
    return(
      Z_q/(1-xi) + (beta - xi*u)/(1 - xi)
    )
  }


  returns <- na.omit(returns)

  test_length <- length(returns) - n - h + 1

  num_quantiles <- length(quantiles)

  quantiles_tracker <- data.frame(matrix(0, nrow = test_length, ncol = num_quantiles))

  sum_tracker <- data.frame(matrix(0, nrow = test_length, ncol = num_quantiles))

  colnames(quantiles_tracker) <- paste0("quantile", quantiles)

  parameter_tracker <- data.frame(matrix(0, nrow = test_length, ncol = 2))

  colnames(parameter_tracker) = c("scale", "shape")

  Extreme_percentage = k/n

  for (i in 1:test_length){
    current_data = returns[i:(i+n-1)]
    z_k1 = sort(current_data, decreasing = TRUE)[k+1]
    z = gpd.fit(current_data,threshold = z_k1, show = FALSE)
    parameter_tracker$scale[i] = z$mle[1]
    parameter_tracker$shape[i] = z$mle[2]
    for (q in 1:num_quantiles) {
      quantile_value <- qgpd((quantiles[q]-1+Extreme_percentage)/(Extreme_percentage), scale = z$mle[1], shape = z$mle[2]) + z_k1
      quantiles_tracker[i, q] <- quantile_value
      sum_tracker[i, q] <- Conditional_Expectation_GPD(z_k1, quantile_value, z$mle[1], z$mle[2])
    }
    print(paste(i,"out of",test_length))
  }

  breaches_tracker = numeric(num_quantiles)
  percentage_tracker = numeric(num_quantiles)

  test_returns = as.vector(returns[(n+h):(length(returns))])

  exceedances_tracker <- vector("list", num_quantiles)

  names(exceedances_tracker) <- paste0("quantile_", quantiles)

  for (i in 1:num_quantiles){
    test = test_returns>quantiles_tracker[,i]
    exceedances_tracker[[i]] <- (test_returns[test] - sum_tracker[test,i])
    breaches = simple_function(returns,quantiles_tracker[,i],steps = h, n = n)
    breaches_tracker[i] = breaches$breaches[1]
    percentage_tracker[i] = breaches$percenage_breach[1]
  }
  return(list(
    quantiles_tracker = quantiles_tracker,
    quantiles = quantiles,
    breaches = breaches_tracker,
    percentage_breaches = percentage_tracker,
    parameter_tracker = parameter_tracker,
    exceedances_tracker = exceedances_tracker
  ))

}

Unconditional_expected_shortfall_real = Unconditional_expected_shortfall_real(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 1)
Unconditional_expected_shortfall_real_h5 = Unconditional_expected_shortfall_real(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 5)
Unconditional_expected_shortfall_real_h10 = Unconditional_expected_shortfall_real(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 10)

mean(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.95)
mean(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.99)
mean(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.995)
var(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.95)
var(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.99)
var(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.995)
plot(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.95, type = 'l')
abline(h=0)
plot(Unconditional_expected_shortfall_real$exceedances_tracker$quantile_0.99, type = 'l')
abline(h=0)

mean(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.95)
mean(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.99)
mean(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.995)
var(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.95)
var(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.99)
var(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.995)
plot(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.95, type = 'l')
abline(h=0)
plot(Unconditional_expected_shortfall_real_h5$exceedances_tracker$quantile_0.99, type = 'l')
abline(h=0)

mean(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.95)
mean(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.99)
mean(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.995)
var(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.95)
var(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.99)
var(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.995)
plot(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.95, type = 'l')
abline(h=0)
plot(Unconditional_expected_shortfall_real_h10$exceedances_tracker$quantile_0.99, type = 'l')
abline(h=0)


plot(Unconditional_expected_shortfall_real$parameter_tracker$shape, type = 'l')




Unconditional_exceedances <- Unconditional_expected_shortfall_real_h5$exceedances_tracker


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
# Loop through each exceedance tracker and create plots
for (i in seq_along(Unconditional_exceedances)) {
  data <- data.frame(Index = seq_along(Unconditional_exceedances[[i]]), Value = Unconditional_exceedances[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}
mean(exceedances_tracker$quantile_0.99)
mean(Unconditional_exceedances$quantile_0.95)


test = t.test(Unconditional_exceedances$quantile_0.95, mu = 0, alternative = "two.sided")
t.test(Unconditional_exceedances$quantile_0.99, mu = 0, alternative = "two.sided")
test$p.value
