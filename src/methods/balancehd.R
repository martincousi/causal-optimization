#!/usr/bin/env Rscript
## Requires the installation of mosek and Rmosek package

library(aciccomp2016)
source("../tools.R")

library(balanceHD)
script <- "balancehd"

set.seed(99)

run_balancehd <- function(iter) {
  params <- getParametersForIter(iter)
  sim <- dgp_2016(input_2016, params[1], params[2])
  onehot_input_2016 <- get_onehot_input_2016()
  df <- onehot_input_2016  # contains k dummies (as suggested by author)
  df$y <- sim$y
  df$z <- sim$z
  status <- try({
    my_time <- system.time({
      res.out <- residualBalance.ate(X=df[,1:(ncol(df)-2)], Y=df$y, W=df$z,
                                     target.pop=1, verbose=FALSE, estimate.se=FALSE)
    })
  })
  if (is.error(status)) {
    my_time <- as.double(NA)
    satt_hat <- as.double(NA)
    satt_hat_std <- as.double(NA)
  } else {
    weights0 <- as.double(NA)
    weights1 <- as.double(NA)
    satt_hat <- res.out
    satt_hat_std <- satt_hat / sd(df$y)
  }
  res <- list("time"=my_time, "weights0"=weights0, "weights1"=weights1,
              "satt_hat"=satt_hat, "satt_hat_std"=satt_hat_std)
  filename <- paste0(paste(script, iter, params[1], params[2], sep="_"),
                     "_res.rds")
  saveRDS(res, file=filename)
}

for (iter in 1:7700) {
  run_balancehd(iter)
}
