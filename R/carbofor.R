#' Calculate Tree Volumes Using Allometric Equations
#'
#' This is a generic function for calculating tree volumes using species-specific allometric equations.
#' See method documentation for details.
#'
#' @param x An object (usually a data.frame) containing tree data.
#' @param ... Additional arguments passed to methods.
#' @export
carbofor <- function(x, ...) {
  UseMethod("carbofor")
}

#' @export
#' @method carbofor data.frame
carbofor.data.frame <- function(x, ...) {
  class(x) <- c("carbofor_data", class(x))
  carbofor(x, ...)
}

#' @export
#' @method carbofor carbofor_data
carbofor.carbofor_data <- function(x,
                                   volume_type = "V22",
                                   equation_id = 1,
                                   carbon = FALSE,
                                   bark = FALSE,
                                   remove_na = FALSE,
                                   source = "Dagnellie",
                                   specimens = NULL,
                                   D130 = "D130",
                                   C130 = "C130",
                                   HTOT = "HTOT",
                                   HDOM = "HDOM",
                                   D150 = "D150",
                                   C150 = "C150",
                                   ...) {

  # 1. Validation des paramètres
  validate_parameters(x, volume_type, equation_id, source, specimens, C130, C150, D130, D150, HTOT, HDOM)

  # 2. Prétraitement des données
  x <- preprocess_data(x, specimens = specimens, C130 = C130, C150 = C150,
                        D130 = D130, D150 = D150, HTOT = HTOT, HDOM = HDOM)

  # 3. Calcul des volumes
  x <- calculate_volume(x, volume_type = volume_type, equation_id = equation_id, source = source)

  # 4. Calcul de l'écorce si demandé
  if (bark) {
    x <- calculate_bark_thickness(x, total_volume_col = volume_type)
  }

  # 5. Calcul de la biomasse et du carbone si demandé
  if (carbon) {
    x <- calculate_biomass(x)
    x <- calculate_carbon(x)
  }

  # 6. Calcul de la variance individuelle et des intervalles de prédiction
  # x <- calculate_individual_variance(x, volume_type = volume_type, equation_id = equation_id)

  # 7. Nettoyage final si demandé
  if (remove_na) {
    x <- x[!is.na(x[[volume_type]]), ]
  }

  return(x)
}

