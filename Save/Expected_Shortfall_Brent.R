library(rugarch)

spec_AR1_GARCH11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                               mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                               distribution.model = "norm")

spec_AR0_GARCH11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                               mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                               distribution.model = "norm")



Brent <- read.csv("Data/Europe_Brent_Spot_Price_FOB.csv", skip = 4, header = TRUE)

Brent$Day <- as.Date(Brent$Day, format = "%m/%d/%Y")
plot(Brent,type='l')

Brent_xts <- xts(Brent$Europe.Brent.Spot.Price.FOB..Dollars.per.Barrel, order.by = Brent$Day)
returns_Brent <- -dailyReturn(Brent_xts, type = "log") * 100
returns_Brent = na.omit(returns_Brent)

Brent_data = as.numeric(returns_Brent)

h_values <- c(1, 5, 10, 30)

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

h_values <- c(1, 5, 10, 30)

Brent_AR1_GARCH11_results <- list()

for (h in h_values) {
  result_name <- paste("Brent_AR0_GARCH11_n1000_k100_h", h, "_ES", sep = "")

  Brent_AR1_GARCH11_results[[result_name]] <- Conditional_Expectation_Backtesting(
    Brent_data,
    spec_AR0_GARCH11,
    quantiles = c(.95, .99, .995),
    n = 1000,
    k = 100,
    h = h
  )
}
Brent_AR1_GARCH11_results[1]
mean_var = function(x){
  name = names(x$exceedances_tracker)
  for (i in 1:length(x$exceedances_tracker)){
    print(name[i])
    print(paste("mean",mean(x$exceedances_tracker[[i]])))
    print(paste("var",var(x$exceedances_tracker[[i]])))
    print("-------------------")
  }
}
for (i in 1:length(Brent_AR1_GARCH11_results)) mean_var(Brent_AR1_GARCH11_results[[i]])

exceedances_tracker = Brent_AR1_GARCH11_results$Brent_AR1_GARCH11_n1000_k100_h1_ES$exceedances_tracker


tracker_name = c("Brent ES difference (95% quantile)", "", "Brent ES difference (99% quantile)", " ")

for (i in seq_along(exceedances_tracker)) {
  data <- data.frame(Index = seq_along(exceedances_tracker[[i]]), Value = exceedances_tracker[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}



