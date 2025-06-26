#' Valide les paramètres d'entrée pour la fonction carbofor
#' @param df Le data.frame d'entrée
#' @param volume_type Type de volume à calculer
#' @param equation_id Identifiant de l'équation
#' @param source Source des équations ("Dagnellie", "Aflan", "Vallet")
#' @param specimens Nom de la colonne d'identification des espèces
#' @param C130, C150, D130, D150, HTOT, HDOM Noms des colonnes dendrométriques
#' @export
validate_parameters <- function(df,
                                volume_type = "V22",
                                equation_id = 1,
                                source = "Dagnellie",
                                specimens = NULL,
                                C130 = "C130", C150 = "C150",
                                D130 = "D130", D150 = "D150",
                                HTOT = "HTOT", HDOM = "HDOM") {
  valid_sources <- c("Dagnellie", "Aflan", "Vallet")
  if (!source %in% valid_sources) {
    stop(paste("Source invalide:", source, "\nSources valides:", paste(valid_sources, collapse = ", ")))
  }

  valid_volume_types <- c("V22", "V22B", "V22_HA", "E")
  if (!volume_type %in% valid_volume_types) {
    stop(paste("Type de volume invalide:", volume_type, "\nTypes valides:", paste(valid_volume_types, collapse = ", ")))
  }

  if (volume_type == "V22" && !equation_id %in% 1:3) {
    stop("Pour le type de volume 'V22', l'identifiant d'équation doit être 1, 2 ou 3.")
  }
  if (volume_type == "V22B" && equation_id != 1) {
    stop("Pour le type de volume 'V22B', seul l'identifiant 1 est valide.")
  }
  if (volume_type == "V22_HA" && equation_id != 1) {
    stop("Pour le type de volume 'V22_HA', seul l'identifiant 1 est valide.")
  }

  required_columns <- c(C130, C150, D130, D150, HTOT, HDOM)
  if (!is.null(specimens)) required_columns <- c(required_columns, specimens)
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    warning(paste("Colonnes manquantes dans les données:", paste(missing_columns, collapse = ", ")))
  }

  if (!any(c(C130, C150, D130, D150) %in% names(df))) {
    stop("Aucune colonne de circonférence ou de diamètre trouvée dans les données.")
  }
}
