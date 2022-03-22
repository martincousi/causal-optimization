library(cobalt)
library(ggplot2)
library(stringr)
library(WeightIt)
library(MatchIt)
library(aciccomp2016)
library(tidyverse)
library(data.table)

source("tools.R")

# get empty df
get_empty_df <- function(nb.rows=7700){
  data.frame(satt=rep(as.double(NA), nb.rows),
             satt.lower=rep(as.double(NA), nb.rows),
             satt.upper=rep(as.double(NA), nb.rows),
             satt.std=rep(as.double(NA), nb.rows),
             run.time=rep(as.double(NA), nb.rows),
             stringsAsFactors=FALSE)
}
# compute
compute_std_bias <- function(df, satt_true_std, weights){
  weights0 <- weights[df$z == 0] / sum(weights[df$z == 0])
  weights1 <- rep(1 / sum(df$z == 1), sum(df$z == 1))
  satt_hat <- weighted.mean(df[df$z == 1, "y"], weights1) -
    weighted.mean(df[df$z == 0, "y"], weights0)
  satt_hat_std <- satt_hat / sd(df$y)
  abs_bias_std <- abs(satt_hat_std - satt_true_std)
  abs_bias_std
}

my.results.raw <- list()


# Import results KBal
my.dir <- ""  # need to adjust this directory
zfile <- "kbal_results_R4.zip"
print(paste("OPENNING", zfile))
zfile.path <- paste0(my.dir, zfile)
files <- unzip(zfile.path, exdir=str_sub(my.dir, end=-2))
for (file.path in files) {
  file <- tail(unlist(strsplit(file.path, "[/]")), n=1)
  file.tmp <- unlist(strsplit(file, "[_]"))
  if (file.tmp[1] == "cbps") {
    algo <- paste(file.tmp[1], file.tmp[2], sep="_")
    iter <- as.integer(file.tmp[3])
  } else {
    algo <- file.tmp[1]
    iter <- as.integer(file.tmp[2])
  }
  tmp <- readRDS(file.path)
  my.results.raw[[algo]][[iter]] <- tmp
}
file.remove(files)


# Compute absolute std bias for KBal to find min and max
satt_true_std_arr <- readRDS(file="satt_true_std.rds")  # obtained with analyze_res.R
abs_bias_std <- rep(NULL, 7700)
for (iter in 1:7700) {
  satt_hat_std <- my.results.raw[["kbal"]][[iter]]$satt_hat_std
  abs_bias_std[iter] <- abs(satt_hat_std - satt_true_std_arr[iter])
}
print(paste("Min", min(abs_bias_std), which.min(abs_bias_std)))
print(paste("Max", max(abs_bias_std), which.max(abs_bias_std)))
iter_min <- which.min(abs_bias_std)
iter_max <- which.max(abs_bias_std)


set.cobalt.options(binary = "std")

# Display results for iter_min
iter <- iter_min
params <- getParametersForIter(iter) 
sim <- dgp_2016(input_2016, params[1], params[2])
df <- input_2016
df$y <- sim$y
df$z <- sim$z
covs <- subset(df, select = -c(y, z))
kbal_weights <- rep(1, length(sim$z))
kbal_weights[!sim$z] <- my.results.raw[["kbal"]][[iter]]$weights0
ps.out <- weightit(df$z ~ covs, data = df,
                   method = "ps", estimand = "ATT") 
match.out <- matchit(f.build("z", covs), data = df, method = "nearest", ratio = 1, 
                     replace = TRUE, estimand = "ATT") 
tab <- bal.tab(df$z ~ covs, data = df, 
               weights= list(KBal = kbal_weights,
                             IPTW = ps.out,
                             Matching = match.out),
               estimand="ATT", quick=FALSE)
love.plot(tab, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE,
          threshold = c(m = .1),
          colors = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
          shapes = c("triangle filled", "circle filled", "square filled", "diamond filled"),
          position = c(.75, .25),
          title = "") +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))
print(paste("Iter:", iter))
print(paste("KBal Abs Std Bias:", compute_std_bias(df, satt_true_std_arr[iter], kbal_weights)))
print(paste("IPTW Abs Std Bias:", compute_std_bias(df, satt_true_std_arr[iter], ps.out$weights)))
print(paste("Matching Abs Std Bias:", compute_std_bias(df, satt_true_std_arr[iter], match.out$weights)))


# Display results for iter_max
iter <- iter_max
params <- getParametersForIter(iter) 
sim <- dgp_2016(input_2016, params[1], params[2])
df <- input_2016
df$y <- sim$y
df$z <- sim$z
covs <- subset(df, select = -c(y, z))
kbal_weights <- rep(1, length(sim$z))
kbal_weights[!sim$z] <- my.results.raw[["kbal"]][[iter]]$weights0
ps.out <- weightit(df$z ~ covs, data = df,
                   method = "ps", estimand = "ATT") 
match.out <- matchit(f.build("z", covs), data = df, method = "nearest", ratio = 1, 
                     replace = TRUE, estimand = "ATT") 
tab <- bal.tab(df$z ~ covs, data = df, 
               weights= list(KBal = kbal_weights,
                             IPTW = ps.out,
                             Matching = match.out),
               estimand="ATT", quick=FALSE)
love.plot(tab, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE,
          threshold = c(m = .1),
          colors = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
          shapes = c("triangle filled", "circle filled", "square filled", "diamond filled"),
          position = c(.75, .25),
          title = "") +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))
print(paste("Iter:", iter))
print(paste("KBal Abs Std Bias:", compute_std_bias(df, satt_true_std_arr[iter], kbal_weights)))
print(paste("IPTW Abs Std Bias:", compute_std_bias(df, satt_true_std_arr[iter], ps.out$weights)))
print(paste("Matching Abs Std Bias:", compute_std_bias(df, satt_true_std_arr[iter], match.out$weights)))
