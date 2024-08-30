library(ismev)
library(ggplot2)
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



library(ggplot2)

create_plot <- function(data, tracker_name) {
  ggplot(data, aes(x = Index, y = Value)) +
    geom_line(color = "black", size = .5) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
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

exceedances_tracker = WTI_AR1_GARCH11_results[[1]]$exceedances_tracker

tracker_name = c("WTI ES difference (95% quantile)", "WTI ES difference (99% quantile)", "WTI ES difference (99.5% quantile)")
for (i in length(exceedances_tracker)) {
  data <- data.frame(Index = 1:length(exceedances_tracker[[i]]), Value = exceedances_tracker[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}

exceedances_tracker = Brent_AR1_GARCH11_results[[1]]$exceedances_tracker

tracker_name = c("Brent ES difference (95% quantile)", "Brent ES difference (99% quantile)", "Brent ES difference (99.5% quantile)")
for (i in 1:length(exceedances_tracker)) {
  data <- data.frame(Index = 1:length(exceedances_tracker[[i]]), Value = exceedances_tracker[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}






















mean_var_all <- function(results_list) {
  mean_var_list <- list()
  names <- names(results_list[[1]]$exceedances_tracker)
  for (i in 1:length(results_list)) {
    for (j in 1:length(results_list[[i]]$exceedances_tracker)) {
      var_val <- var(results_list[[i]]$exceedances_tracker[[j]]) / length(results_list[[i]]$exceedances_tracker[[j]])
      var_val2 <- var(results_list[[i]]$exceedances_tracker[[j]])
      mean_val <- mean(results_list[[i]]$exceedances_tracker[[j]])

      mean_var_list[[names[j]]]$mean <- c(mean_var_list[[names[j]]]$mean, mean_val)
      mean_var_list[[names[j]]]$var <- c(mean_var_list[[names[j]]]$var, var_val)
      mean_var_list[[names[j]]]$var2 <- c(mean_var_list[[names[j]]]$var2, var_val2)
    }
  }

  return(mean_var_list)
}


# Replace with code below to get the results from a AR(0)-GARCH(1,1) model
#means_vars_Brent_AR1 = mean_var_all(Brent_AR0_GARCH11_results)
#means_vars_WTI_AR1 = mean_var_all(WTI_AR0_GARCH11_results)

means_vars_Brent_AR1 = mean_var_all(Brent_AR1_GARCH11_results)
means_vars_WTI_AR1 = mean_var_all(WTI_AR1_GARCH11_results)






















x_pos<- 1:length(h_values)

means_Brent <- means_vars_Brent_AR1$quantile_0.95$mean
vars_Brent <- means_vars_Brent_AR1$quantile_0.95$var

means_WTI <- means_vars_WTI_AR1$quantile_0.95$mean
vars_WTI <- means_vars_WTI_AR1$quantile_0.95$var

se_Brent <- sqrt(vars_Brent)
ci_lower_Brent <- means_Brent - 1.96 * se_Brent
ci_upper_Brent <- means_Brent + 1.96 * se_Brent

se_WTI <- sqrt(vars_WTI)
ci_lower_WTI <- means_WTI - 1.96 * se_WTI
ci_upper_WTI <- means_WTI + 1.96 * se_WTI

data_Brent <- data.frame(
  x_position = x_pos,
  Mean = means_Brent,
  CI_Lower = ci_lower_Brent,CI_Upper = ci_upper_Brent
  ,Data = "1")

data_WTI <- data.frame(
  x_position = x_pos,
  Mean = means_WTI,
  CI_Lower = ci_lower_WTI,
  CI_Upper = ci_upper_WTI,
  Data = "2"
)

data_combined <- rbind(data_Brent, data_WTI)

ggplot(data_combined, aes(x = x_position, y = Mean, color = Data)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  scale_x_continuous(
    breaks = x_pos,
    labels = h_values
  ) +
  labs(title = "Mean and Confidence Intervals for Quantile 95% ES difference",
       x = "Forecasted steps (h)",
       y = "Mean") +
  theme_minimal() +
  theme(legend.position = "none")



















means_Brent <- means_vars_Brent_AR1$quantile_0.99$mean
vars_Brent <- means_vars_Brent_AR1$quantile_0.99$var

means_WTI <- means_vars_WTI_AR1$quantile_0.99$mean
vars_WTI <- means_vars_WTI_AR1$quantile_0.99$var

se_Brent <- sqrt(vars_Brent)
ci_lower_Brent <- means_Brent - 1.96 * se_Brent
ci_upper_Brent <- means_Brent + 1.96 * se_Brent

se_WTI <- sqrt(vars_WTI)
ci_lower_WTI <- means_WTI - 1.96 * se_WTI
ci_upper_WTI <- means_WTI + 1.96 * se_WTI
data_Brent <- data.frame(
  x_position = x_pos,
  Mean = means_Brent,
  CI_Lower = ci_lower_Brent,
  CI_Upper = ci_upper_Brent,
  Data = "1"
)

data_WTI <- data.frame(
  x_position = x_pos,
  Mean = means_WTI,
  CI_Lower = ci_lower_WTI,
  CI_Upper = ci_upper_WTI,
  Data = "2"
)

data_combined <- rbind(data_Brent, data_WTI)

ggplot(data_combined, aes(x = x_position, y = Mean, color = Data)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  scale_x_continuous(
    breaks = x_pos,
    labels = h_values
  ) +
  labs(title = "Mean and Confidence Intervals for Quantile 99% ES difference",
       x = "Forecasted steps (h)",
       y = "Mean") + theme_minimal() + theme(legend.position = "none")








means_Brent <- means_vars_Brent_AR1$quantile_0.995$mean
vars_Brent <- means_vars_Brent_AR1$quantile_0.995$var

means_WTI <- means_vars_WTI_AR1$quantile_0.995$mean
vars_WTI <- means_vars_WTI_AR1$quantile_0.995$var

se_Brent <- sqrt(vars_Brent)
ci_lower_Brent <- means_Brent - 1.96 * se_Brent
ci_upper_Brent <- means_Brent + 1.96 * se_Brent

se_WTI <- sqrt(vars_WTI)
ci_lower_WTI <- means_WTI - 1.96 * se_WTI
ci_upper_WTI <- means_WTI + 1.96 * se_WTI

data_Brent <- data.frame(
  x_position = x_pos,
  Mean = means_Brent,
  CI_Lower = ci_lower_Brent,
  CI_Upper = ci_upper_Brent,
  Data = "1"
)

data_WTI <- data.frame(
  x_position = x_pos,
  Mean = means_WTI,
  CI_Lower = ci_lower_WTI,
  CI_Upper = ci_upper_WTI,
  Data = "2"
)

data_combined <- rbind(data_Brent, data_WTI)

ggplot(data_combined, aes(x = x_position, y = Mean, color = Data)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  scale_x_continuous(
    breaks = x_pos,
    labels = h_values
  ) + labs(title = "Mean and Confidence Intervals for Quantile 99.5% ES difference",
       x = "Forecasted steps (h)",
       y = "Mean") + theme_minimal() + theme(legend.position = "none")

