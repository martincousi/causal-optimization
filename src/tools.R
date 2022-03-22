library(aciccomp2016)


is.error <- function(x) inherits(x, "try-error")


## get parameters from iter, and vice-versa
getParametersForIter <- function(i.row) {
  c((i.row - 1L) %%  nrow(aciccomp2016::parameters_2016) + 1L,
    (i.row - 1L) %/% nrow(aciccomp2016::parameters_2016) + 1L)
}
getIterForParameters <- function(i.par, i.sim)
{
  i.par + (i.sim - 1L) * nrow(aciccomp2016::parameters_2016)
}


## create dummy vars for x_2, x_21, x_24
# generate onehot_input_2016 (k dummies)
get_onehot_input_2016 <- function() {
  onehot_input_2016 <- cbind(input_2016)
  onehot_input_2016 <- onehot_input_2016[, -c(2, 21, 24)]
  temp <- model.matrix(~ x_2 + 0, input_2016)
  onehot_input_2016 <- cbind(onehot_input_2016, temp)
  temp <- model.matrix(~ x_21 + 0, input_2016)
  onehot_input_2016 <- cbind(onehot_input_2016, temp)
  temp <- model.matrix(~ x_24 + 0, input_2016)
  onehot_input_2016 <- cbind(onehot_input_2016, temp)
  cols <- c("x_1","x_2A","x_2B","x_2C","x_2D","x_2E","x_2F","x_3","x_4","x_5",
            "x_6","x_7","x_8","x_9","x_10","x_11","x_12","x_13","x_14","x_15",
            "x_16","x_17","x_18","x_19","x_20","x_21A","x_21B","x_21C","x_21D",
            "x_21E","x_21F","x_21G","x_21H","x_21I","x_21J","x_21K","x_21L",
            "x_21M","x_21N","x_21O","x_21P","x_22","x_23","x_24A","x_24B","x_24C",
            "x_24D","x_24E","x_25","x_26","x_27","x_28","x_29","x_30","x_31",
            "x_32","x_33","x_34","x_35","x_36","x_37","x_38","x_39","x_40","x_41",
            "x_42","x_43","x_44","x_45","x_46","x_47","x_48","x_49","x_50","x_51",
            "x_52","x_53","x_54","x_55","x_56","x_57","x_58")
  onehot_input_2016 <- onehot_input_2016[,cols]  # reorder columns
}
# generate dummy_input_2016 (k-1 dummies)
get_dummy_input_2016 <- function() {
  dummy_input_2016 <- cbind(input_2016)
  dummy_input_2016 <- dummy_input_2016[, -c(2, 21, 24)]
  temp <- model.matrix(~ x_2 + 0, input_2016)
  dummy_input_2016 <- cbind(dummy_input_2016, temp[,-1])
  temp <- model.matrix(~ x_21 + 0, input_2016)
  dummy_input_2016 <- cbind(dummy_input_2016, temp[,-1])
  temp <- model.matrix(~ x_24 + 0, input_2016)
  dummy_input_2016 <- cbind(dummy_input_2016, temp[,-1])
  cols <- c("x_1","x_2B","x_2C","x_2D","x_2E","x_2F","x_3","x_4","x_5",
            "x_6","x_7","x_8","x_9","x_10","x_11","x_12","x_13","x_14","x_15",
            "x_16","x_17","x_18","x_19","x_20","x_21B","x_21C","x_21D",
            "x_21E","x_21F","x_21G","x_21H","x_21I","x_21J","x_21K","x_21L",
            "x_21M","x_21N","x_21O","x_21P","x_22","x_23","x_24B","x_24C",
            "x_24D","x_24E","x_25","x_26","x_27","x_28","x_29","x_30","x_31",
            "x_32","x_33","x_34","x_35","x_36","x_37","x_38","x_39","x_40","x_41",
            "x_42","x_43","x_44","x_45","x_46","x_47","x_48","x_49","x_50","x_51",
            "x_52","x_53","x_54","x_55","x_56","x_57","x_58")
  dummy_input_2016 <- dummy_input_2016[,cols]  # reorder columns
}


##