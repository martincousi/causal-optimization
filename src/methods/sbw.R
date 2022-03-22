#!/usr/bin/env Rscript
## Stable balancing weights (SBW) - sbw

library(aciccomp2016)
source("../tools.R")

library(sbw)
script <- "sbw"

run_sbw <- function(i.par, j.sim) {
  sim <- dgp_2016(input_2016, i.par, j.sim)
  dummy_input_2016 <- get_dummy_input_2016()
  df <- dummy_input_2016  # contains k-1 dummies (as suggested by authors)
  df$y <- sim$y
  df$z <- sim$z
  # parameters
  bal = list()
  bal$bal_cov <- colnames(dummy_input_2016)  # no higher moments
  bal$bal_tol <- 0.02  # irrelevant if bal_alg = TRUE
  bal$bal_alg <- TRUE # defaults to TRUE, takes a long time
  sol = list()
  sol$sol_name <- "quadprog"  # default, appears faster than cplex and gurobi
  sol$sol_dis <- FALSE
  status <- try({
    time <- system.time({
      sbw.out <- sbw(dat=df, ind='z', out='y', bal=bal, sol=sol)
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
    weights <- sbw.out$dat_weights$sbw_weights
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
  run_sbw(params[1], params[2])
}
