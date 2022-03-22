#!/usr/bin/env Rscript
## Covariate balancing generalized propensity score (CBGPS) - CBPS exact

library(aciccomp2016)
source("../tools.R")

library(CBPS)
script <- "cbps_exact"

run_cbps <- function(i.par, j.sim, variant) {
  sim <- dgp_2016(input_2016, i.par, j.sim)
  df <- input_2016  # directly supports factors
  df$y <- sim$y
  df$z <- sim$z
  status <- try({
    time <- system.time({
      if (variant == "over") {
        cbps.out <- CBPS(z ~ . - y, data=df, method="over")  # Imai2014
      } else if (variant == "exact") {
        cbps.out <- CBPS(z ~ . - y, data=df, method="exact")  # Imai2014
      }
    })
  })
  if (is.error(status)) {
    time <- NA
    weights0 <- NA
    weights1 <- NA
    satt_hat <- NA
    satt_hat_std <- NA
  } else {
    # get weights
    weights <- cbps.out$weights
    weights0 <- weights[df$z == 0] / sum(weights[df$z == 0])
    weights1 <- rep(1 / sum(df$z == 1), sum(df$z == 1))
    # estimate satt and standardized satt
    satt_hat <- weighted.mean(df[df$z == 1, "y"], weights1) -
      weighted.mean(df[df$z == 0, "y"], weights0)
    satt_hat_std <- satt_hat / sd(df$y)
  }
  # save results
  res <- list("time"=time, "weights0"=weights0, "weights1"=weights1,
              "satt_hat"=satt_hat, "satt_hat_std"=satt_hat_std)
  filename <- paste0(paste(script, iter, i.par, j.sim, sep="_"),
                     "_res.rds")
  saveRDS(res, file=filename)
}

for (iter in 1:7700) {
  params <- getParametersForIter(iter)
  run_cbps(params[1], params[2], "exact")
}
