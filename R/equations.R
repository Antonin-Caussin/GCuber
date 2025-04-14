equations_allometriques <- read.csv2("C:/Users/anton/Desktop/TFE/Ecriture/Equations.csv",
                                    stringsAsFactors = FALSE,
                                    na.strings = c("NA", ""))
usethis::use_data(equations_allometriques, internal = TRUE, overwrite = TRUE)
