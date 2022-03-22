#!/usr/bin/env Rscript
## Genetic Matching (GenMatch) - genmatch

library(aciccomp2016)
source("../tools.R")

library(Matching)
script <- "genmatch"

run_genmatch <- function(i.par, j.sim) {
  filename <- paste0(paste(script, iter, i.par, j.sim, sep="_"),
                     "_res.rds")
  if (!file.exists(filename)) {  # check that results do not exist yet
    sim <- dgp_2016(input_2016, i.par, j.sim)
    onehot_input_2016 <- get_onehot_input_2016()
    df <- onehot_input_2016  # contains k binaries (as suggested by author)
    df$y <- sim$y
    df$z <- sim$z
    status <- try({
      time <- system.time({
        gen.out <- GenMatch(Tr=df$z, X=onehot_input_2016, print.level=1) 
        m.out <- Match(Y=df$y, Tr=df$z, X=onehot_input_2016, estimand="ATT", Weight.matrix=gen.out)
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
      weights0 <- NA
      weights1 <- NA
      # estimate satt and standardized satt
      satt_hat <- m.out$est
      satt_hat_std <- satt_hat / sd(df$y)
    }
    # save results
    res <- list("time"=time, "weights0"=weights0, "weights1"=weights1,
                "satt_hat"=satt_hat, "satt_hat_std"=satt_hat_std)
    saveRDS(res, file=filename)
    }
}

for (iter in 1:7700) {
  params <- getParametersForIter(iter)
  run_genmatch(params[1], params[2])
}
