library(rugarch)
library(quantmod)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1) + 0.1)
getSymbols("CL=F", src = 'yahoo', from = '2000-01-01', to = Sys.Date())
`CL=F` = na.omit(`CL=F`)
returns <- -dailyReturn(Cl(`CL=F`),type='log')*100
returns = na.omit(returns)
length(`CL=F`)
`CL=F`
24*365
# Plot Oil Closing Prices without default axes and customize x-axis
plot(index(Cl(`CL=F`)), coredata(Cl(`CL=F`)),
     main = "Oil Closing Prices",
     ylab = "Price (USD)",
     xlab = "Date",
     col = "black",
     type = "l",
     lwd = 2,
     axes = FALSE,  # Suppress default axes
     xaxt = "n")    # Suppress default x-axis

# Customize x-axis with labels every 42 months
axis.Date(1, at = seq(min(index(Cl(`CL=F`))), max(index(Cl(`CL=F`))), by = "42 months"), format = "%d.%m.%Y", cex.axis = 0.9)
axis(2, cex.axis = 0.9)  # Add y-axis
box()


plot(index(returns), coredata(returns),
     main = "Oil Log Daily Returns",
     ylab = "Daily Returns (%)",
     xlab = "Date",
     ylim = c(min(returns) - 5, max(returns) + 5),
     col = "black",
     type = "l",
     lwd = 2,
     axes = FALSE,  # Suppress default axes
     xaxt = "n")   # Suppress default x-axis

# Customize x-axis with labels every 19 months
axis.Date(1, at = seq(min(index(returns)), max(index(returns)), by = "42 months"), format = "%d.%m.%Y", cex.axis = 0.9)
axis(2, cex.axis = 0.9)  # Add y-axis
box()

returns[is.na(returns$daily.returns)]
# Specify the GARCH(1,1) model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 0)),
                   distribution.model = "norm")

# Fit the model
fit <- ugarchfit(spec = spec, data = returns)
print(fit)

# Analyze the results
plot(fit, which = "all")
fitted_values <- fitted(fit)
conditional_volatility <- sigma(fit)
plot(conditional_volatility, main = "Conditional Volatility", ylab = "Volatility", xlab = "Date")

acf(returns[returns>quantile(returns,.95)])

library(ismev)
library(evd)
returns = na.omit(returns)
returns = as.vector(returns)
n = 1000
k = 100
list = data.frame(parameter1 = rep(0,(length(returns)-n)), parameter2 = rep(0,(length(returns)-n)))
quantiles1 = rep(0,length(returns)-n)
quantiles2 = rep(0,length(returns)-n)
quantiles3 = rep(0,length(returns)-n)


for (i in 1:(length(returns)-n)){
  current_data = returns[i:(i+n-1)]
  z_k1 = sort(current_data, decreasing = TRUE)[k+1]
  z = gpd.fit(current_data,threshold = z_k1, show = FALSE)
  list$parameter1[i] = z$mle[1]
  list$parameter2[i] = z$mle[2]
  quantiles1[i] = qgpd(.50, scale = z$mle[1], shape = z$mle[2]) + z_k1
  quantiles2[i] = qgpd(.90, scale = z$mle[1], shape = z$mle[2]) + z_k1
  quantiles3[i] = qgpd(.95, scale = z$mle[1], shape = z$mle[2]) + z_k1
  print(i)
}
# Plot the returns and quantiles
plot(y = returns, x = 1:length(returns), type = "l", ylim = c(0, max(returns)),
     main = "EVT Only Rolling Returns", xlab = "Time", ylab = "Returns")
lines(y = quantiles1, x = (n + 1):(length(returns)), col = 'red')
lines(y = quantiles2, x = (n + 1):(length(returns)), col = 'blue')
lines(y = quantiles3, x = (n + 1):(length(returns)), col = 'green')
legend("topleft", legend = c("Returns", "95th Percentile Quantile", "99th Percentile Quantile", "99.5th Percentile Quantile"),
       col = c("black", "red", "blue", "green"), lty = 1)


simple_function = function(returns=returns,quantiles,steps=1){
  test = 1*(returns[(n+steps):(length(returns))]>quantiles[steps:length(quantiles)])
  breaches = sum(test)
  percenage_breach = mean(test)
  return(list(
    breaches = breaches,
    percenage_breach = percenage_breach
  ))
}
steps = 1

(length(returns)-n)*.05
(length(returns)-n)*.01
(length(returns)-n)*.005
step = 1
simple_function(returns=returns,quantiles1, step = step)
simple_function(returns=returns,quantiles2, step = step)
simple_function(returns=returns,quantiles3, step = step)

steps_number = 1000
start = 1
end = 2000
steps = seq(from = start, to = end, length.out = steps_number)

breaches_tracker = numeric(steps_number)
percentage_tracker = numeric(steps_number)
for (i in 1:length(steps)){
  result = as.list(simple_function(returns=returns,quantiles3, step = steps[i]))
  breaches_tracker[i] = result[[1]]
  percentage_tracker[i] = result[[2]]
}
plot(y=percentage_tracker, x=steps, type = 'l')
abline(h=.005)
plot(list$parameter1, type = 'l')
plot(list$parameter2, type = 'l')






quantiles1 = rep(0,length(returns)-n)
quantiles2 = rep(0,length(returns)-n)
quantiles3 = rep(0,length(returns)-n)
Sums1 = rep(0,length(returns)-n)
Sums2 = rep(0,length(returns)-n)
Sums3 = rep(0,length(returns)-n)
tracker1 = rep(0,length(returns)-n)
tracker2 = rep(0,length(returns)-n)
tracker3 = rep(0,length(returns)-n)



Conditional_Expectation_GPD <- function(u, Z_q, beta, xi){
  if (xi >= 1) {
    stop("xi must be less than 1 for the expectation to be finite.")
  }
  return(
    Z_q/(1-xi) + (beta - xi*u)/(1 - xi)
  )
}


# Expected Shortfall
for (i in 1:(length(returns)-n)){
  current_data = returns[i:(i+n-1)]
  z_k1 = sort(current_data, decreasing = TRUE)[k+1]
  z = gpd.fit(current_data,threshold = z_k1, show = FALSE)
  list$parameter1[i] = z$mle[1]
  list$parameter2[i] = z$mle[2]
  quantiles1[i] = qgpd(.50, scale = z$mle[1], shape = z$mle[2]) + z_k1
  quantiles2[i] = qgpd(.90, scale = z$mle[1], shape = z$mle[2]) + z_k1
  quantiles3[i] = qgpd(.95, scale = z$mle[1], shape = z$mle[2]) + z_k1
  Sums1[i] = Conditional_Expectation_GPD(z_k1, quantiles1[i], z$mle[1], z$mle[2])
  Sums2[i] = Conditional_Expectation_GPD(z_k1, quantiles2[i], z$mle[1], z$mle[2])
  Sums3[i] = Conditional_Expectation_GPD(z_k1, quantiles3[i], z$mle[1], z$mle[2])
}
steps = 1
returns_shortend = returns[(n+steps):(length(returns))]
test1 = returns[(n+steps):(length(returns))]>quantiles1[steps:length(quantiles1)]
test2 = returns[(n+steps):(length(returns))]>quantiles2[steps:length(quantiles2)]
test3 = returns[(n+steps):(length(returns))]>quantiles3[steps:length(quantiles3)]
a1 = returns_shortend[test1] - Sums1[test1]
a2 = returns_shortend[test2] - Sums2[test2]
a3 = returns_shortend[test3] - Sums3[test3]
plot(a1)
abline(h=0)
plot(a2)
abline(h=0)
plot(a3)
abline(h=0)
mean(a1)
mean(a2)
mean(a3)
plot(y = returns, x = 1:length(returns), type = "l", ylim = c(0, max(returns)),
     main = "EVT Only Rolling Returns", xlab = "Time", ylab = "Returns")
lines(y = Sums1, x = (n + 1):(length(returns)), col = 'red')
lines(y = Sums2, x = (n + 1):(length(returns)), col = 'blue')
lines(y = Sums3, x = (n + 1):(length(returns)), col = 'green')

library(ggplot2)


# Plotting
plot_data <- data.frame(
  Time = 1:length(returns),
  Returns = returns,
  Sums1 = c(rep(NA, n), Sums1),
  Sums2 = c(rep(NA, n), Sums2),
  Sums3 = c(rep(NA, n), Sums3)
)

ggplot(plot_data, aes(x = Time)) +
  geom_line(aes(y = Returns), color = 'black') +
  geom_line(aes(y = Sums1), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Sums2), color = 'blue', linetype = "dashed") +
  geom_line(aes(y = Sums3), color = 'green', linetype = "dashed") +
  labs(
    title = "EVT Only Rolling Returns",
    x = "Time",
    y = "Returns"
  ) +
  theme_minimal()

# Plot differences
plot_diff <- function(a, title) {
  diff_data <- data.frame(
    Index = 1:length(a),
    Difference = a
  )
  ggplot(diff_data, aes(x = Index, y = Difference)) +
    geom_line(color = 'blue') +
    geom_hline(yintercept = 0, linetype = "dashed", color = 'red') +
    labs(
      title = title,
      x = "Index",
      y = "Difference"
    ) +
    theme_minimal()
}

plot_diff(a1, "Difference for Quantiles at 50%")
plot_diff(a2, "Difference for Quantiles at 90%")
plot_diff(a3, "Difference for Quantiles at 95%")

mean(a1)
mean(a2)
mean(a3)
