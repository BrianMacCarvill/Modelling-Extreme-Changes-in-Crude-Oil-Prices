GPD_numerical_stability <- function(data, thresholds = seq(quantile(data, .05), quantile(data, .99), length.out = 80)) {

  n <- length(thresholds)
  parameters <- list(thresholds = thresholds,
                     scale_mle = numeric(n),
                     shape_mle = numeric(n),
                     scale_se = numeric(n),
                     shape_se = numeric(n))

  for (i in 1:n) {
    fit <- gpd.fit(data, thresholds[i], show = FALSE)
    parameters$scale_mle[i] <- fit$mle[1]
    parameters$shape_mle[i] <- fit$mle[2]
    parameters$scale_se[i] <- fit$se[1]
    parameters$shape_se[i] <- fit$se[2]
  }

  return(parameters)
}
