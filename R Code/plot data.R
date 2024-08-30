library(quantmod)
library(xts)
library(zoo)
library(dplyr)
library(ggplot2)


price_plot <- ggplot(WTI_df, aes(x = Date, y = Price)) +
  geom_line(color = "black") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white"),
  ) +
  labs(title = "WTI Crude Oil Price", x = "Date", y = "Price (USD per Barrel)")

print(price_plot)

quantile_95 <- quantile(returns_df$daily.returns, 0.95)
quantile_99 <- quantile(returns_df$daily.returns, 0.99)

start_date <- min(returns_df$Date)
end_date <- max(returns_df$Date)

returns_plot <- ggplot(returns_df, aes(x = Date, y = daily.returns)) +
  geom_line(color = "black") +
  geom_segment(aes(x = start_date, xend = end_date, y = quantile_95, yend = quantile_95),
               color = "blue", linetype = "dashed", size = .8) +
  geom_segment(aes(x = start_date, xend = end_date, y = quantile_99, yend = quantile_99),
               color = "red", linetype = "dashed", size = .8) +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white"),
  ) +
  labs(title = "WTI Negative Log Returns", x = "Date", y = "Daily Return (%)")

print(returns_plot)


price_plot <- ggplot(Brent_df, aes(x = Date, y = Price)) +
  geom_line(color = "black") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white"),
  ) +
  labs(title = "Brent Crude Oil Price", x = "Date", y = "Price (USD per Barrel)")

print(price_plot)

quantile_95 <- quantile(returns_Brent_df$daily.returns, 0.95)
quantile_99 <- quantile(returns_Brent_df$daily.returns, 0.99)

start_date <- min(returns_Brent_df$Date)
end_date <- max(returns_Brent_df$Date)

returns_plot <- ggplot(returns_Brent_df, aes(x = Date, y = daily.returns)) +
  geom_line(color = "black") +
  geom_segment(aes(x = start_date, xend = end_date, y = quantile_95, yend = quantile_95),
               color = "blue", linetype = "dashed", size = .8) +
  geom_segment(aes(x = start_date, xend = end_date, y = quantile_99, yend = quantile_99),
               color = "red", linetype = "dashed", size = .8) +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white"),
  ) +
  labs(title = "Negative Log Returns", x = "Date", y = "Daily Return (%)")

plot(returns_plot)



abs_returns <- abs(as.numeric(returns))



acfs <- acf(abs_returns, plot = TRUE)

acf_data <- data.frame(
  lag = acfs$lag,
  acf = acfs$acf
)

acf_p <- ggplot(acf_data, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "grey70", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed") +
  annotate("text", x = 40, y = 0.05, label = "0.05", hjust = 1.1, vjust = -0.5, color = "black", size = 3) +  # Add text annotation at y = 0.05
  annotate("text", x = 40, y = -0.05, label = "-0.05", hjust = 1.1, vjust = 1.5, color = "black", size = 3) +  # Add text annotation at y = -0.05
  labs(title = "ACF of Absolute Returns", x = "Lag", y = "ACF") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5)

  )

print(acf_p)
