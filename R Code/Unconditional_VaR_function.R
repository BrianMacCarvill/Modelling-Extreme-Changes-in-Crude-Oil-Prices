Unconditional_VaR = function(returns, n = 1000, k = 100, quantiles = c(.95,.99), h = 1){
  simple_function = function(returns=returns,quantiles,steps=1, n = 1000){
    test = 1*(returns[(n+steps):(length(returns))]>quantiles)
    breaches = sum(test)
    percenage_breach = mean(test)
    return(list(
      breaches = breaches,
      percenage_breach = percenage_breach
    ))
  }

  returns <- na.omit(returns)

  test_length <- length(returns) - n - h + 1

  num_quantiles <- length(quantiles)

  quantiles_tracker <- data.frame(matrix(0, nrow = test_length, ncol = num_quantiles))

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
      quantile_value <- evd::qgpd((quantiles[q]-1+Extreme_percentage)/(Extreme_percentage), scale = z$mle[1], shape = z$mle[2]) + z_k1
      quantiles_tracker[i, q] <- quantile_value
    }
    print(paste(i,"out of",test_length))
  }

  breaches_tracker = numeric(num_quantiles)
  percentage_tracker = numeric(num_quantiles)
  for (i in 1:num_quantiles){
    breaches = simple_function(returns,quantiles_tracker[,i],steps = h, n = n)
    breaches_tracker[i] = breaches$breaches[1]
    percentage_tracker[i] = breaches$percenage_breach[1]
  }
  return(list(
    quantiles_tracker = quantiles_tracker,
    quantiles = quantiles,
    breaches = breaches_tracker,
    percentage_breaches = percentage_tracker,
    parameter_tracker = parameter_tracker
  ))

}
