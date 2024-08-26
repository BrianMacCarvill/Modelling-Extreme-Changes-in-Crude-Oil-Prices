# Means past a threshold in Figures 5 and 6
mean_past_threshold = function(data, thresholds = seq(quantile(data,.05),quantile(data,.99),length.out = 80)){

  mean_past_threshold_list = c()
  n = length(thresholds)
  for (u in 1:n) {
    mean_past_threshold_list[u] <- mean(data[data > thresholds[u]] - thresholds[u])
  }


  ecdf_function <- ecdf(data)
  quantile_value <- ecdf_function(thresholds)


  return(data.frame(thresholds = thresholds,
           mean_past_threshold_list = mean_past_threshold_list,
           quantile_value = quantile_value))
}

# WTI data read in in "Read and plot data" file
data = as.numeric(returns$daily.returns)

mean_past_threshold_WTI = mean_past_threshold(data, thresholds = seq(quantile(data,.5),quantile(data,.99),length.out = 10000))


library(ggplot2)

quantiles <- quantile(returns, probs = c(0.5, 0.8, 0.9))
quantile_list <- c("50%", "80%", "90%")

ggplot(mean_past_threshold_WTI, aes(x = thresholds, y = mean_past_threshold_list)) +
  geom_line(color = "black", size = 1) +
  geom_vline(xintercept = quantiles, color = "red", linetype = "dashed") +
  annotate("text", x = quantiles, y = c(3,3,3),
           label = quantile_list, color = "red", vjust = -0.5, hjust = -0.1, size = 3.5) +
  labs(y = "Mean Excess", x = "Threshold (u)", title = "WTI Mean Excesses") +  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90")
  )


# Brent data readin in "Read and plot data" file
data = as.numeric(returns_Brent$daily.returns)


mean_past_threshold_Brent = mean_past_threshold(data, thresholds = seq(quantile(data,.5),quantile(data,.99),length.out = 10000))

quantiles <- quantile(returns_Brent, probs = c(0.5, 0.8, 0.9))
ggplot(mean_past_threshold_Brent, aes(x = thresholds, y = mean_past_threshold_list)) +
  geom_line(color = "black", size = 1) +
  geom_vline(xintercept = quantiles, color = "red", linetype = "dashed") +
  annotate("text", x = quantiles, y = c(3,3,3),
           label = quantile_list, color = "red", vjust = -0.5, hjust = -0.1, size = 2.5) +
  labs(y = "Mean Excess", x = "Threshold (u)", title = "Brent Mean Excesses") +  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90")
  )




















