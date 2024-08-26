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



library(ggplot2)

create_plot <- function(data, tracker_name) {
  ggplot(data, aes(x = Index, y = Value)) +
    geom_line(color = "black", size = .5) +
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

exceedances_tracker = WTI_AR1_GARCH11_results[[1]]$exceedances_tracker

tracker_name = c("WTI ES difference (95% quantile)", "WTI ES difference (99% quantile)", " ")
for (i in seq_along(exceedances_tracker)) {
  data <- data.frame(Index = seq_along(exceedances_tracker[[i]]), Value = exceedances_tracker[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}

exceedances_tracker = Brent_AR1_GARCH11_results[[1]]$exceedances_tracker

tracker_name = c("Brent ES difference (95% quantile)", "Brent ES difference (99% quantile)", " ")
for (i in seq_along(exceedances_tracker)) {
  data <- data.frame(Index = seq_along(exceedances_tracker[[i]]), Value = exceedances_tracker[[i]])
  p <- create_plot(data, tracker_name[i])
  print(p)
}






















mean_var_all = function(results_list) {
  mean_var_list = list()

  for (i in 1:length(results_list)) {
    result = results_list[[i]]
    name = names(result$exceedances_tracker)

    for (j in 1:length(result$exceedances_tracker)) {
      quantile_name = name[j]
      var_val = var(result$exceedances_tracker[[j]]) / length(result$exceedances_tracker[[j]])
      var_val2 = var(result$exceedances_tracker[[j]])
      mean_val = mean(result$exceedances_tracker[[j]])

      if (is.null(mean_var_list[[quantile_name]])) {
        mean_var_list[[quantile_name]] = list(mean = c(), var = c(), var2 = c())
      }

      mean_var_list[[quantile_name]]$mean = c(mean_var_list[[quantile_name]]$mean, mean_val)
      mean_var_list[[quantile_name]]$var = c(mean_var_list[[quantile_name]]$var, var_val)
      mean_var_list[[quantile_name]]$var2 = c(mean_var_list[[quantile_name]]$var2, var_val2)
    }
  }

  return(mean_var_list)
}

means_vars_Brent_AR1 = mean_var_all(Brent_AR1_GARCH11_results)
means_vars_WTI_AR1 = mean_var_all(WTI_AR1_GARCH11_results)






















h_values <- c(1, 5, 10, 30)

x_positions <- 1:length(h_values)

means_95_Brent = means_vars_Brent_AR1$quantile_0.95$mean
vars_95_Brent = means_vars_Brent_AR1$quantile_0.95$var

means_95_WTI = means_vars_WTI_AR1$quantile_0.95$mean
vars_95_WTI = means_vars_WTI_AR1$quantile_0.95$var

se_95_Brent = sqrt(vars_95_Brent)
ci_lower_95_Brent = means_95_Brent - 1.96 * se_95_Brent
ci_upper_95_Brent = means_95_Brent + 1.96 * se_95_Brent

se_95_WTI = sqrt(vars_95_WTI)
ci_lower_95_WTI = means_95_WTI - 1.96 * se_95_WTI
ci_upper_95_WTI = means_95_WTI + 1.96 * se_95_WTI

plot_data_Brent = data.frame(
  x_position = x_positions,
  Mean = means_95_Brent,
  CI_Lower = ci_lower_95_Brent,
  CI_Upper = ci_upper_95_Brent,
  Dataset = "Brent_AR1"
)

plot_data_WTI = data.frame(
  x_position = x_positions,
  Mean = means_95_WTI,
  CI_Lower = ci_lower_95_WTI,
  CI_Upper = ci_upper_95_WTI,
  Dataset = "WTI_AR1"
)

plot_data_combined = rbind(plot_data_Brent, plot_data_WTI)

ggplot(plot_data_combined, aes(x = x_position, y = Mean, color = Dataset, group = Dataset)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  scale_x_continuous(
    breaks = x_positions,
    labels = h_values,
    limits = c(0.5, length(h_values) + 0.5),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(title = "Mean and Confidence Intervals for Quantile 95% ES difference",
       x = "Forcested steps (h)",
       y = "Mean") +
  theme_minimal() +
  theme(legend.position = "none")


















means_99_Brent = means_vars_Brent_AR1$quantile_0.99$mean
vars_99_Brent = means_vars_Brent_AR1$quantile_0.99$var

means_99_WTI = means_vars_WTI_AR1$quantile_0.99$mean
vars_99_WTI = means_vars_WTI_AR1$quantile_0.99$var

se_99_Brent = sqrt(vars_99_Brent)
ci_lower_99_Brent = means_99_Brent - 1.96 * se_99_Brent
ci_upper_99_Brent = means_99_Brent + 1.96 * se_99_Brent

se_99_WTI = sqrt(vars_99_WTI)
ci_lower_99_WTI = means_99_WTI - 1.96 * se_99_WTI
ci_upper_99_WTI = means_99_WTI + 1.96 * se_99_WTI

plot_data_Brent_99 = data.frame(
  x_position = x_positions,
  Mean = means_99_Brent,
  CI_Lower = ci_lower_99_Brent,
  CI_Upper = ci_upper_99_Brent,
  Dataset = "Brent_AR1"
)

plot_data_WTI_99 = data.frame(
  x_position = x_positions,
  Mean = means_99_WTI,
  CI_Lower = ci_lower_99_WTI,
  CI_Upper = ci_upper_99_WTI,
  Dataset = "WTI_AR1"
)

plot_data_combined_99 = rbind(plot_data_Brent_99, plot_data_WTI_99)

ggplot(plot_data_combined_99, aes(x = x_position, y = Mean, color = Dataset, group = Dataset)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  scale_x_continuous(
    breaks = x_positions,
    labels = h_values,
    limits = c(0.5, length(h_values) + 0.5),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(title = "Mean and Confidence Intervals for Quantile 99% ES difference",
       x = "Forcested steps (h)",
       y = "Mean") +
  theme_minimal() +
  theme(legend.position = "none")


















