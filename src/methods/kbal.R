#!/usr/bin/env Rscript
## KBAL

library(aciccomp2016)
source("../tools.R")

library(kbal)
script <- "kbal"

run_kbal <- function(iter) {
  params <- getParametersForIter(iter)
  sim <- dgp_2016(input_2016, params[1], params[2])
  dummy_input_2016 <- get_dummy_input_2016()
  df <- dummy_input_2016  # contains k-1 dummies (as suggested by author)
  df$y <- sim$y
  df$z <- sim$z
  status <- try({
    my_time <- system.time({
      kbal.out <- kbal(allx=df[,1:(ncol(df)-2)], treatment=df$z)
    })
  })
  if (is.error(status)) {
    my_time <- as.double(NA)
    satt_hat <- as.double(NA)
    satt_hat_std <- as.double(NA)
  } else {
    weights <- kbal.out$w
    weights0 <- weights[df$z == 0] / sum(weights[df$z == 0])
    weights1 <- rep(1 / sum(df$z == 1), sum(df$z == 1))
    satt_hat <- weighted.mean(df[df$z == 1, "y"], weights1) -
      weighted.mean(df[df$z == 0, "y"], weights0)
    satt_hat_std <- satt_hat / sd(df$y)
  }
  res <- list("time"=my_time, "weights0"=weights0, "weights1"=weights1,
              "satt_hat"=satt_hat, "satt_hat_std"=satt_hat_std)
  filename <- paste0(paste(script, iter, params[1], params[2], sep="_"),
                     "_res.rds")
  saveRDS(res, file=filename)
}

for (i.par in 1:77) {
  set.seed(99)
  for (j.sim in 1:100) {
    iter <- getIterForParameters(i.par, j.sim)
    run_kbal(iter)
  }
}

