#' Convertir un data.frame en objet carbofor_data
#' @export
as.carbofor_data <- function(df) {
  class(df) <- c("carbofor_data", class(df))
  return(df)
}

#' Prétraitement des données pour le calcul de volume
#' @export
preprocess_data <- function(df, specimens = NULL, C130 = "C130", C150 = "C150",
                            D130 = "D130", D150 = "D150", HTOT = "HTOT", HDOM = "HDOM", ...) {
  df <- establish_species_correspondence(df, specimens = specimens)
  df <- diameter_conversions(df, C130 = C130, C150 = C150, D130 = D130, D150 = D150)
  df <- convert_circumference(df, C130 = C130, C150 = C150)
  df <- calculate_basal_areas(df, C130 = C130, C150 = C150)
  return(df)
}

#' Correspondance des espèces à partir des codes ou abréviations
#' @export
establish_species_correspondence <- function(df, specimens) {
  if (is.null(specimens) || !specimens %in% names(df)) {
    stop("Colonne d'identification des espèces non spécifiée ou absente.")
  }

  specimens_type <- detect_specimens_type(df[[specimens]])

  if (!"Species" %in% names(df)) {
    mapping <- unique(equations[, c("Species", specimens_type)])
    names(mapping) <- c("Species", specimens)
    df <- merge(df, mapping, by = specimens, all.x = TRUE)
  }

  if (any(is.na(df$Species))) {
    warning("Certaines correspondances d'espèces sont manquantes.")
  }

  return(df)
}

#' Détection automatique du type d'identifiant d'espèce
#' @export
detect_specimens_type <- function(values) {
  values <- na.omit(values)
  if (is.numeric(values)) return("Code")
  if (is.character(values) || is.factor(values)) {
    if (mean(nchar(as.character(values))) <= 4) return("Abr")
    else return("Species")
  }
  stop("Type de données non reconnu pour l'identification des espèces.")
}

#' Conversion diamètre <-> circonférence
#' @export
diameter_conversions <- function(df, C130, C150, D130, D150) {
  pi_val <- pi
  if (D130 %in% names(df) && !C130 %in% names(df)) df[[C130]] <- df[[D130]] * pi_val
  if (D150 %in% names(df) && !C150 %in% names(df)) df[[C150]] <- df[[D150]] * pi_val
  if (C130 %in% names(df) && !D130 %in% names(df)) df[[D130]] <- df[[C130]] / pi_val
  if (C150 %in% names(df) && !D150 %in% names(df)) df[[D150]] <- df[[C150]] / pi_val
  return(df)
}

#' Conversion générique entre C130 et C150
#' @export
convert_circumference <- function(df, C130, C150) {
  if (!"Species" %in% names(df)) return(df)
  coefs <- unique(equations[, c("Species", "HV", "IV")])
  for (i in seq_len(nrow(df))) {
    sp <- df$Species[i]
    row_coef <- coefs[coefs$Species == sp, ]
    if (nrow(row_coef) == 0) next
    if (is.na(df[[C130]][i]) && !is.na(df[[C150]][i])) {
      df[[C130]][i] <- row_coef$HV * df[[C150]][i] + row_coef$IV
    } else if (is.na(df[[C150]][i]) && !is.na(df[[C130]][i])) {
      df[[C150]][i] <- (df[[C130]][i] - row_coef$IV) / row_coef$HV
    }
  }
  return(df)
}

#' Calcul des surfaces basales G130 et G150
#' @export
calculate_basal_areas <- function(df, C130, C150) {
  if (C130 %in% names(df)) {
    df$G130 <- (df[[C130]]^2) / (4 * pi * 10000)
  }
  if (C150 %in% names(df)) {
    df$G150 <- (df[[C150]]^2) / (4 * pi * 10000)
  }
  return(df)
}
