#!/usr/bin/env Rscript
## BART

library(aciccomp2016)
source("../tools.R")

library(BART)
script <- "bart"

run_bart <- function(i.par, j.sim) {
  sim <- dgp_2016(input_2016, i.par, j.sim)
  xt <- input_2016
  xt$z <- sim$z
  y <- sim$y
  xp <- xt[sim$z == 1,]
  xp[, ncol(xp)] <- 0
  status <- try({
    my_time <- system.time({
      bart.tot <- wbart(x.train=xt, y.train=y, x.test=xp)
    })
  })
  if (is.error(status)) {
    my_time <- as.double(NA)
    satt_hat <- as.double(NA)
    satt_hat_std <- as.double(NA)
  } else {
    diffs <- bart.tot$yhat.train[,sim$z == 1] - bart.tot$yhat.test
    mndiffs <- apply(diffs, 1, mean)
    satt_hat <-  mean(mndiffs)
    satt_hat_std <- satt_hat / sd(y)
  }
  res <- list("my_time"=my_time, "satt_hat"=satt_hat, "satt_hat_std"=satt_hat_std)
  res
}


for (i.par in 1:77) {
  nb.rows <- 7700
  bart_res <- data.frame(satt=rep(as.double(NA), nb.rows),
                         satt.lower=rep(as.double(NA), nb.rows),
                         satt.upper=rep(as.double(NA), nb.rows),
                         satt.std=rep(as.double(NA), nb.rows),
                         run.time=rep(as.double(NA), nb.rows),
                         stringsAsFactors=FALSE)
  for (j.sim in 1:100) {
    set.seed(99)
    iter <- getIterForParameters(i.par, j.sim)
    print(paste(iter, i.par, j.sim))
    res <- run_bart(i.par, j.sim)
    bart_res[iter, "satt"] <- res$satt_hat
    bart_res[iter, "satt.std"] <- res$satt_hat_std
    bart_res[iter, "run.time"] <- res$my_time[3]
  }
  
  filename <- paste0(paste(script, i.par, sep="_"), "_res.rds")
  saveRDS(bart_res, file=filename)
}