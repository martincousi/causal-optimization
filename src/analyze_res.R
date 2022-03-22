library(aciccomp2016)
library(tidyverse)
library(data.table)
library(writexl)
library(stringr)
source("tools.R")


# If true then reload all raw results,
# else reload final results to save time.
OVERWRITE_MY <- FALSE

# get number of NAs
get_na_nb <- function(x) {
  x.names <- names(x)
  na.nb <- list()
  for (name in x.names) {
    na.nb[[name]] <- sum(is.na(x[[name]]$satt))
  }
  unlist(na.nb)
}

# get empty df
get_empty_df <- function(nb.rows=7700){
  data.frame(satt=rep(as.double(NA), nb.rows),
             satt.lower=rep(as.double(NA), nb.rows),
             satt.upper=rep(as.double(NA), nb.rows),
             satt.std=rep(as.double(NA), nb.rows),
             run.time=rep(as.double(NA), nb.rows),
             stringsAsFactors=FALSE)
}

# import my results (that are in zip files)
import_myresults <- function(my.dir, my.file, my.ext) {
  zfiles <- list.files(my.dir, pattern=my.ext, ignore.case=TRUE)  # get all results files in this dir
  my.results.raw <- list()
  for (zfile in zfiles) {
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
  }
  # rearrange results as data.frame
  print("PROCESSING RESULTS")
  my.results <- list()
  for (algo in names(my.results.raw)) {
    df <- get_empty_df()
    for (iter in 1:7700) {
      tmp <- my.results.raw[[algo]][[iter]]
      if (!is.null(tmp)) {
        df[iter, 'satt'] <- tmp$satt_hat
        df[iter, 'satt.std'] <- tmp$satt_hat_std
        df[iter, 'run.time'] <- tmp$time[3]  # take total time
      } else {
        df[iter, 'satt'] <- NA
        df[iter, 'satt.std'] <- NA
        df[iter, 'run.time'] <- NA
      }
    }
    my.results[[algo]] <- df
  }
  rm(my.results.raw)
  saveRDS(my.results, paste0(my.dir, my.file))
  my.results
}


## load ACIC results
acic.dir <- ""  # see README.md
files <- list.files(acic.dir, pattern="\\.rds$", ignore.case=TRUE)  # get all rds files in this dir
acic.results.raw <- list()
for (file in files) {
  file.splitted <- unlist(strsplit(file, "[.]"))[1]
  file.path <- paste0(acic.dir, file)
  tmp <- readRDS(file.path)
  acic.results.raw[[file.splitted]] <- tmp
}
# rearrange results as data.frame
acic.results <- list()
for (algo in names(acic.results.raw)) {
  df <- get_empty_df()
  df[, 'satt'] <- acic.results.raw[[algo]][, 'satt']
  df[, 'satt.lower'] <- acic.results.raw[[algo]][, 'satt.lower']
  df[, 'satt.upper'] <- acic.results.raw[[algo]][, 'satt.upper']
  acic.results[[algo]] <- df
}
rm(acic.results.raw)
# get sd(sim$y) to compute satt.std
file <- "sd_y.rds"
if (file.exists(file)) {
  sd.y <- readRDS(file=file)
} else {
  sd.y <- rep(as.double(NA), 7700)
  for (iter in 1:7700) {
    params <- getParametersForIter(iter)
    sim <- dgp_2016(input_2016, params[1], params[2])
    sd.y[iter] <- sd(sim$y)
  }
  saveRDS(sd.y, file=file)
}
# compute satt.std
for (algo in names(acic.results)) {
  df <- acic.results[[algo]]
  df[, 'satt.std'] <- df[, 'satt'] / sd.y
  acic.results[[algo]] <- df
}  
acic.na.nb <- get_na_nb(acic.results)


## load my results
# import all results
my.dir <- "./"  # may need to adjust this directory
my.file <- "my_results.rds"
if (!file.exists(paste0(my.dir, my.file))) {
  OVERWRITE_MY <- TRUE
}
if (OVERWRITE_MY == FALSE) {
  my.results <- readRDS(paste0(my.dir, my.file))
} else {
  my.results <- import_myresults(my.dir, my.file, "results_R4\\.zip$")  # obtained with the different method scripts
}
my.na.nb <- get_na_nb(my.results)


## do analyses
# get satt_true.std
satt_true_file <- "satt_true_std.rds"
if (file.exists(satt_true_file)) {
  satt_true.std <- readRDS(file=satt_true_file)
} else {
  satt_true.std <- rep(as.double(NA), 7700)
  for (iter in 1:7700) {
    params <- getParametersForIter(iter)
    sim <- dgp_2016(input_2016, params[1], params[2])
    satt_true.std[iter] <- with(sim, mean(y.1[z == 1] - y.0[z == 1])) / sd(sim$y)
  }
  saveRDS(satt_true.std, file=satt_true_file)
}
# compute bias
compute_bias <- function(my.list, satt_true.std) {
  for (algo in names(my.list)) {
    tmp <- my.list[[algo]][['satt.std']]
    bias <- tmp - satt_true.std
    my.list[[algo]][['bias']] <- bias
  }
  my.list
}
my.results <- compute_bias(my.results, satt_true.std)
acic.results <- compute_bias(acic.results, satt_true.std)
# compute coverage
compute_coverage <- function(my.list, satt_true.std, sd.y) {
  # if lower and upper are NA, then returns NA
  for (algo in names(my.list)) {
    my.lower <- my.list[[algo]][['satt.lower']] / sd.y
    my.upper <- my.list[[algo]][['satt.upper']] / sd.y
    cover <- (my.lower <= satt_true.std) & (satt_true.std <= my.upper)
    my.list[[algo]][['cover']] <- cover
  }
  my.list
}
my.results <- compute_coverage(my.results, satt_true.std, sd.y)
acic.results <- compute_coverage(acic.results, satt_true.std, sd.y)
# get final results
cols <- c("n", "bias_mean", "bias_sd", "rmse", "time", "coverage_mean")
rows <- c(names(my.results), names(acic.results))
final.df <- setNames(data.frame(matrix(ncol=length(cols), nrow=length(rows))),
                    cols)
row.names(final.df) <- rows
compute_final <- function(final.df, my.list) {
  for (algo in names(my.list)) {
    tmp <- my.list[[algo]][['bias']]
    final.df[algo, "n"] <- sum(!is.na(tmp))
    final.df[algo, "bias_mean"] <- mean(tmp, na.rm=TRUE)
    final.df[algo, "bias_sd"] <- sd(tmp, na.rm=TRUE)
    final.df[algo, "rmse"] <- sqrt(mean(tmp^2, na.rm=TRUE))
    final.df[algo, "time"] <- mean(my.list[[algo]][['run.time']], na.rm=TRUE)
    final.df[algo, "coverage_mean"] <- mean(my.list[[algo]][['cover']], na.rm=TRUE)
  }
  final.df
}
final.df <- compute_final(final.df, my.results)
final.df <- compute_final(final.df, acic.results)
setDT(final.df, keep.rownames="method")
write_xlsx(final.df, "./final.xlsx")


## Additionnal results (BART)
bart.dir <- "./results_bart/"  # need to adjust this directory
zfile <- "bart_results_R4.zip"  # obtained with bart.R script
print(paste("OPENNING", zfile))
zfile.path <- paste0(bart.dir, zfile)
files <- unzip(zfile.path, exdir=str_sub(bart.dir, end=-2))
bart.results <- get_empty_df()
for (file.path in files) {
  file <- tail(unlist(strsplit(file.path, "[/]")), n=1)
  file.tmp <- unlist(strsplit(file, "[_]"))
  i.par <- as.integer(file.tmp[2])
  tmp <- readRDS(file.path)
  for (j.sim in 1:100) {
    iter <- getIterForParameters(i.par, j.sim)
    bart.results[iter, 'satt'] <- tmp[iter, 'satt']
    bart.results[iter, 'satt.std'] <- tmp[iter, 'satt.std']
    bart.results[iter, 'run.time'] <- tmp[iter, 'run.time']
  }
}
file.remove(files)
tmp <- is.na(bart.results[,'satt'])
if (sum(tmp) > 0) {  # making sure no iteration is missing
  library(BART)
  for (iter in which(tmp)) {
    params <- getParametersForIter(iter)
    sim <- dgp_2016(input_2016, params[1], params[2])
    xt <- input_2016
    xt$z <- sim$z
    y <- sim$y
    xp <- xt[sim$z == 1,]
    xp[, ncol(xp)] <- 0
    status <- try({
      time <- system.time({
        bart.tot <- wbart(x.train=xt, y.train=y, x.test=xp)
      })
    })
    if (is.error(status)) {
      time <- NA
      satt_hat <- NA
      satt_hat_std <- NA
    } else {
      diffs <- bart.tot$yhat.train[,sim$z == 1] - bart.tot$yhat.test
      mndiffs <- apply(diffs, 1, mean)
      satt_hat <-  mean(mndiffs)
      satt_hat_std <- satt_hat / sd(y)
    }
    bart.results[iter, 'satt'] <- satt_hat
    bart.results[iter, 'satt.std'] <- satt_hat_std
    bart.results[iter, 'run.time'] <- time[3]
  }
}
bart.results[, 'bias'] <- bart.results[, 'satt.std'] - satt_true.std
# get final results
cols <- c("n", "bias_mean", "bias_sd", "rmse", "time")
rows <- c("bart")
bart_final.df <- setNames(data.frame(matrix(ncol=length(cols), nrow=length(rows))),
                     cols)
compute_bart_final <- function(final.df, results.df) {
  tmp <- results.df[,'bias']
  final.df["bart", "n"] <- sum(!is.na(tmp))
  final.df["bart", "bias_mean"] <- mean(tmp, na.rm=TRUE)
  final.df["bart", "bias_sd"] <- sd(tmp, na.rm=TRUE)
  final.df["bart", "rmse"] <- sqrt(mean(tmp^2, na.rm=TRUE))
  final.df["bart", "time"] <- mean(results.df[,'run.time'], na.rm=TRUE)
  final.df
}
bart_final.df <- compute_bart_final(bart_final.df, bart.results)
bart_final.df
