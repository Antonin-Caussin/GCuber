# =====================================================================
#                    validate_parameters()
# =====================================================================

#' Validate Input Parameters for Allometric Volume Calculation
#'
#' @description
#' This function checks the validity of the input arguments provided to functions
#' using allometric equations (e.g., for volume, biomass, or carbon estimations).
#' It ensures that:
#' - The specified `source` and `volume_type` are among the accepted values.
#' - The selected `equation_id` is allowed for the given `volume_type`.
#' - All required columns (diameter, height, species) are present in the input data.
#'
#' The function is primarily used internally before launching model predictions,
#' to catch user errors and provide informative messages.
#'
#' @param x A `data.frame` containing the tree-level inventory data. Must include
#' at least one column related to diameter or circumference at a defined height.
#' @param volume_type Character. Type of volume to be computed.
#' Accepted values include:
#' \itemize{
#'   \item \code{"V22"}: Commercial volume over bark from 0 to 22 cm diameter.
#'   \item \code{"V22B"}: Commercial volume under bark from 0 to 22 cm diameter.
#'   \item \code{"V22_HA"}: Commercial volume over bark extrapolated to hectare scale.
#'   \item \code{"Sawlog"}: Sawlog volume.
#'   \item \code{"Aboveground"}: Total aboveground volume.
#'   \item \code{"Merchantable"}: Merchantable volume.
#' }
#' @param equation_id Integer. Identifier of the equation to use (typically from an internal database).
#' Some volume types only accept specific values for this argument.
#' @param source Character. Source of the allometric equations.
#' Must be one of: \code{"Dagnellie"}, \code{"Algan"}, \code{"Vallet"}, \code("Bouvard"), \code("Rondeu") or \code("Courbet")
#' @param specimens Optional character. Name of the column containing species identifiers
#' (codes, abbreviations, or full names), used to select the appropriate model.
#' @param C130,C150 Character. Names of the columns containing circumference at 130 cm
#' and 150 cm above ground, respectively (in cm).
#' @param D130,D150 Character. Names of the columns containing diameter at 130 cm
#' and 150 cm above ground, respectively (in cm).
#' @param HTOT Character. Name of the column with total tree height (in meters).
#' @param HDOM Character. Name of the column with dominant height, used for plot-level scaling (in meters).
#'
#' @return
#' The function returns the input `data.frame` invisibly, after validating the parameters.
#' It throws an error if a parameter is invalid (e.g., unknown volume type or source),
#' and issues a warning if some required columns are missing from the dataset.
#'
#' @details
#' This function does not modify the dataset. It is intended to be used as a safeguard
#' before applying volume, biomass, or carbon prediction models. At least one of the
#' following columns must be present in `x`: `C130`, `C150`, `D130`, or `D150`.
#'
#' @examples
#' \dontrun{
#' # Minimal working example
#' df <- data.frame(
#'   C130 = c(100, 120),
#'   D130 = c(31.8, 38.2),
#'   HTOT = c(20, 25),
#'   HDOM = c(22, 26),
#'   Species = c("FASY", "PCSY")
#' )
#'
#' validate_parameters(
#'   x = df,
#'   volume_type = "V22",
#'   equation_id = 1,
#'   source = "Dagnellie",
#'   specimens = "Species"
#' )
#' }
#'
#' @seealso
#' \code{\link{calculate_volumes}}, \code{\link{carbofor_species}},
#' \code{\link{calculate_prediction_interval}} for downstream use.
#'
#' @export

validate_parameters <- function(x,
                                volume_type = "V22",
                                equation_id = 1,
                                source = "Dagnellie",
                                specimens = NULL,
                                C130 = "C130", C150 = "C150",
                                D130 = "D130", D150 = "D150",
                                HTOT = "HTOT", HDOM = "HDOM") {
  valid_sources <- c("Dagnellie", "Algan", "Vallet", "Bouvard","Courbet","Rondeu")
  if (!source %in% valid_sources) {
    stop(paste("Invalid source:", source, "\nValid sources:", paste(valid_sources, collapse = ", ")))
  }

  valid_volume_types <- c("V22", "V22B", "V22_HA", "Sawlog", "Aboveground" ,"Merchantable" )
  if (!volume_type %in% valid_volume_types) {
    stop(paste("Invalid volume type:", volume_type, "\nValid types:", paste(valid_volume_types, collapse = ", ")))
  }

  if (volume_type == "V22" && !equation_id %in% 1:3) {
    stop("For volume type 'V22', equation_id must be 1, 2, or 3.")
  }
  if (volume_type == "V22B" && equation_id != 1) {
    stop("For volume type 'V22B', only equation_id = 1 is allowed.")
  }
  if (volume_type == "V22_HA" && equation_id != 1) {
    stop("For volume type 'V22_HA', only equation_id = 1 is allowed.")
  }

  required_columns <- c(C130, C150, D130, D150, HTOT, HDOM)
  if (!is.null(specimens)) {
    required_columns <- c(required_columns, specimens)
  }

  missing_columns <- setdiff(required_columns, names(x))
  if (length(missing_columns) > 0) {
    warning(paste("Missing columns in data:", paste(missing_columns, collapse = ", ")))
  }

  if (!any(c(C130, C150, D130, D150) %in% names(x))) {
    stop("No diameter or circumference column found in the data.")
  }

  return(x)
}
