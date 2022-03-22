#!/usr/bin/env Rscript
## Entropy balancing (EBal) - ebal

library(aciccomp2016)
source("../tools.R")

library(ebal)
script <- "ebal"

run_ebal <- function(iter, i.par, j.sim) {
  sim <- dgp_2016(input_2016, i.par, j.sim)
  dummy_input_2016 <- get_dummy_input_2016()
  df <- dummy_input_2016  # contains k-1 dummies (as suggested by authors)
  df$y <- sim$y
  df$z <- sim$z
  status <- try({
    time <- system.time({
      ebal.out <- ebalance(Treatment=df$z, X=dummy_input_2016)
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
    weights0 <- ebal.out$w / sum(ebal.out$w)
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
  run_ebal(iter, params[1], params[2])
}
