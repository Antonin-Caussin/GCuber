equations <- read.csv2("C:/Users/anton/Desktop/TFE/Ecriture/Equations.csv",
                       stringsAsFactors = FALSE,
                       na.strings = c("NA", ""))

cols_numeriques <- c("A0", "Code", "b0", "b1", "b2", "b3", "b4", "b5",
                     "NumEquation", "HV", "IV", "ID", "D130_Min", "D130_Max",
                     "sigma", "SCE_D130", "x_mean_D130", "x_mean_C130", "SCE_C130")

equations[cols_numeriques] <- lapply(equations[cols_numeriques], function(x) as.numeric(as.character(x)))

usethis::use_data(equations, internal = TRUE, overwrite = TRUE)
