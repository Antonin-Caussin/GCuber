#' Main entry point to compute forest inventory metrics
#'
#' @description
#' This is the generic method for computing tree volume, bark thickness,
#' aboveground biomass, and carbon content from forest inventory data.
#' It delegates to specific methods depending on the class of the input `x`.
#'
#' @param x An object containing tree inventory data. Typically a data.frame.
#' @param ... Additional arguments passed to class-specific methods.
#'
#' @return A data.frame or object containing the computed variables, depending on the method used.
#'
#' @seealso
#' \code{\link{calculate_volume}} — Core function for volume computation using selected allometric models.
#'
#' \code{\link{carbofor.carbofor_data}} for the actual computation.
#'
#' \code{\link{calculate_biomass}} — Computes aboveground biomass from volume  and species coefficients but also using selected allometric models.
#'
#' \code{\link{calculate_carbon}} — Converts biomass into estimated carbon stock.
#'
#' \code{\link{calculate_bark_thickness}} — Estimates bark thickness based on empirical rules.
#'
#' \code{\link{preprocess_data}} — Internal preprocessing including species matching and conversion of dendrometric variables.
#'
#' \code{\link{validate_parameters}} — Checks consistency and validity of input parameters and required columns.

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


#' Compute tree volume, bark thickness, biomass and carbon from a data.frame
#'
#' @description
#' This method calculates tree volume, bark thickness, aboveground biomass and associated
#' carbon stock from individual tree data using species-specific allometric equations.
#' It includes internal validation and preprocessing steps such as species mapping,
#' harmonization of diameter/circumference measurements, and height variable consistency.
#'
#' The function supports several volume types and flexible configurations based on the
#' source and structure of allometric models stored internally.
#'
#' @method carbofor carbofor_data
#' @export
#'
#' @param x A `data.frame` of class `carbofor_data` containing individual tree measurements.
#' Must include at least species identification and diameter data.
#' @param volume_type Character. The type of volume to compute (e.g. , "V22" , "V22B" , "V22_HA"  )
#' @param equation_id Integer. Index of the equation to apply per species when multiple models are available.
#' Default is 1.
#' @param carbon Logical. If `TRUE`, biomass and carbon content are computed.
#' @param bark Logical. If `TRUE`, bark thickness and bark-adjusted volume are computed.
#' @param remove_na Logical. If `TRUE`, rows with missing computed volume are removed from the output.
#' @param source Character. Identifier of the source for the equations (e.g., "Dagnelie", "Vallet", "Bouvard").
#' @param specimens Character. Name of the column in `x` used to identify species (e.g., code, abbreviation, or full name).
#' @param D130 Character. Column name for diameter at 130 cm height (default = "D130").
#' @param C130 Character. Column name for circumference at 130 cm height (default = "C130").
#' @param D150 Character. Column name for diameter at 150 cm height (default = "D150").
#' @param C150 Character. Column name for circumference at 150 cm height (default = "C150").
#' @param HTOT Character. Column name for total tree height (default = "HTOT").
#' @param HDOM Character. Column name for dominant height (used for plot-level scaling).
#' @param H Character. Column name for the distance separating the felling cut level from a reference point.
#' @param biomass_method Character. Method used to compute biomass if `carbon = TRUE`.
#' Options are "volume" (volume × density) or "equation" (species-specific biomass model).
#' @param ... Additional arguments passed to internal helper functions (not used directly here).
#'
#' @return A `data.frame` identical to `x` but enriched with additional columns depending on options:
#' \itemize{
#'   \item \code{<volume_type>} — Computed volume in m³ (e.g., `V22`, `V22B`, etc.)
#'   \item \code{Validity Status} — Indicates if the DHB falls within the validity domain of the equation.
#'   \item \code{Biomass Aboveground}, \code{Biomass Root}, \code{Biomass Total} — Biomass components [kg] (if `carbon = TRUE`).
#'   \item \code{Carbon} — Total carbon content in kilograms (if `carbon = TRUE`).
#'   \item \code{Relative_Width} — Relative width of the prediction interval, expressed as a percentage of the predicted volume.
#'   \item \code{Reliability} — Qualitative interpretation of the interval width (e.g., "High", "Medium", "Low").
#' }
#'
#' @seealso
#' \code{\link{calculate_volume}}, \code{\link{calculate_biomass}}, \code{\link{calculate_carbon}},
#' \code{\link{calculate_bark_thickness}}, \code{\link{validate_parameters}}, \code{\link{preprocess_data}}
#'
#' @examples
#' \dontrun{
#' # Example with European beech and Sessile oak
#' df <- data.frame(
#'   Species = c("Birch", "Silver fir"),
#'   D130 = c(32, 40),   # diameter at 1.30 m
#'   C130 = c(100, 126), # circumference at 1.30 m
#'   HTOT = c(25, 28),   # total height
#'   HDOM = c(27, 30)    # dominant height
#' )
#'
#' result <- carbofor(
#'   x = df,
#'   volume_type = "V22",
#'   carbon = TRUE,
#'   bark = TRUE,
#'   specimens = "Species",
#'   biomass_method = "volume"
#' )
#'
#' head(result)
#' }
carbofor.carbofor_data <- function(x,
                                   volume_type = "V22",
                                   equation_id = 1,
                                   carbon = FALSE,
                                   bark = FALSE,
                                   remove_na = FALSE,
                                   source = "Dagnelie",
                                   specimens = NULL,
                                   D130 = "D130",
                                   C130 = "C130",
                                   HTOT = "HTOT",
                                   HDOM = "HDOM",
                                   D150 = "D150",
                                   C150 = "C150",
                                   H = "H",
                                   biomass_method = "volume",
                                   ...) {

  validate_parameters(x, volume_type, equation_id, source, specimens, C130, C150, D130, D150, HTOT, HDOM)

  x <- preprocess_data(x, specimens = specimens, C130 = C130, C150 = C150,
                       D130 = D130, D150 = D150, HTOT = HTOT, HDOM = HDOM)

  x <- calculate_volume(x, volume_type = volume_type, equations = equations,
                        equation_id = equation_id, source = source)

  if (bark) {
    x <- calculate_bark_thickness(x, equations = equations, volume_type = volume_type,
                                  source = source, total_volume_col = NULL)
  }

  if (carbon) {
    x <- calculate_biomass(x, equations = equations, method = biomass_method, bark = bark)
    x <- calculate_carbon(x)
  }


  x <- calculate_prediction_interval(
    x,
    equations = equations,
    volume_type = volume_type,
    source = source,
    equation_id = equation_id,
    confidence_level = 0.95
  )

  if (remove_na) {
    x <- x[!is.na(x[[volume_type]]), ]
  }

  return(x)
}

