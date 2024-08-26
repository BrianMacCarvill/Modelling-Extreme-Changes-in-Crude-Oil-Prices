library(rugarch)

ts_data <- WTI_data
aic_bic_results <- data.frame(p = integer(), q = integer(), r = integer(), AIC = numeric(), BIC = numeric())

ps = 2
qs = 3
rs = 3
for (p in 1:ps) {
  for (q in 1:qs) {
    for (r in 1:rs) {
      spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q, r)),
                         mean.model = list(armaOrder = c(p,0)))

      fit <- ugarchfit(spec = spec, data = ts_data, solver = "hybrid",
                    solver.control = list(trace = 0, maxit = 3, tol = 1e-8, reltol = 1e-8))


      aic_value <- infocriteria(fit)[1]
      bic_value <- infocriteria(fit)[2]
      aic_bic_results <- rbind(aic_bic_results,
                               data.frame(p = p, q = q, r = r, AIC = aic_value, BIC = bic_value))
    }
  }
}

print(aic_bic_results)











ts_data <- Brent_data
aic_bic_results <- data.frame(p = integer(), q = integer(), r = integer(), AIC = numeric(), BIC = numeric())


for (p in 1:ps) {
  for (q in 1:qs) {
    for (r in 1:rs) {
      spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q, r)),
                         mean.model = list(armaOrder = c(p,0)))

      fit <- ugarchfit(spec = spec, data = ts_data, solver = "hybrid",
                    solver.control = list(trace = 0, maxit = 3, tol = 1e-8, reltol = 1e-8))
      aic_value <- infocriteria(fit)[1]
      bic_value <- infocriteria(fit)[2]
      aic_bic_results <- rbind(aic_bic_results,
                               data.frame(p = p, q = q, r = r, AIC = aic_value, BIC = bic_value))
    }
  }
}

print(aic_bic_results)
