library(ismev)
library(evd)
library(ggplot2)
library(dplyr)
library(xts)
library(zoo)


data = as.numeric(returns$daily.returns)
WTI_VaR_h1 = Unconditional_VaR(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 1)
WTI_VaR_h5 = Unconditional_VaR(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 5)
WTI_VaR_h10 = Unconditional_VaR(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 10)
WTI_VaR_h30 = Unconditional_VaR(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 30)


data_Brent = as.numeric(returns_Brent$daily.returns)
Brent_VaR_h1 = Unconditional_VaR(data_Brent, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 1)
Brent_VaR_h5 = Unconditional_VaR(data_Brent, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 5)
Brent_VaR_h10 = Unconditional_VaR(data_Brent, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 10)
Brent_VaR_h30 = Unconditional_VaR(data_Brent, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 30)


returns_df <- data.frame(
  Index = index(returns),
  Value = coredata(returns)
)

quantiles_data <- data.frame(
  Index = index(returns),
  Quantile_0.95 = c(rep(NA,1000),WTI_VaR_h1$quantiles_tracker$quantile0.95),
  Quantile_0.99 = c(rep(NA,1000),WTI_VaR_h1$quantiles_tracker$quantile0.99)
)

plot_data <- full_join(returns_df, quantiles_data, by = "Index")


p <- ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = daily.returns), color = "black", linewidth = .5) +
  geom_line(aes(y = Quantile_0.95), color = "blue", linetype = "dashed", size = .8) +
  geom_line(aes(y = Quantile_0.99), color = "red", linetype = "dashed", size = .8) +
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

























WTI_ES_h1 = Unconditional_expected_shortfall(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 1)
WTI_ES_h5 = Unconditional_expected_shortfall(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 5)
WTI_ES_h10 = Unconditional_expected_shortfall(data, n = 1000, k = 100, quantiles = c(.95, .99,.995), h = 10)



Unconditional_exceedances <- WTI_ES_h1$exceedances_tracker


create_plot <- function(data, tracker_name) {
  ggplot(data, aes(x = Index, y = Value)) +
    geom_line(color = "black", size = 0.5) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1.2) +
    labs(title = tracker_name, x = "Index", y = "Value") +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

tracker_name = c("WTI ES difference (95% quantile)", "WTI ES difference (99% quantile)", " ")
for (i in seq_along(Unconditional_exceedances)) {
  data <- data.frame(Index = seq_along(Unconditional_exceedances[[i]]), Value = Unconditional_exceedances[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}
mean(WTI_ES_h1$exceedances_tracker$quantile_0.99)
mean(WTI_ES_h1$exceedances_tracker$quantile_0.95)
var(WTI_ES_h1$exceedances_tracker$quantile_0.99)
var(WTI_ES_h1$exceedances_tracker$quantile_0.95)

