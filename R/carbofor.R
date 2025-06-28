#' Compute tree volumes, bark thickness, biomass, and carbon content
#'
#' @description
#' This function is the main interface to calculate tree volume, optional bark thickness,
#' biomass, and carbon stock using species-specific allometric equations.
#' It supports different volume types, handles data preprocessing (including species mapping
#' and diameter-circumference conversions), and offers flexible options for source models.
#'
#' The function is designed to work with a `data.frame` of tree measurements and integrates
#' several steps of forest mensuration into a single call.
#'
#' @param x A data.frame containing tree-level dendrometric measurements.
#' @param volume_type Character. Type of volume to compute. One of `"V22"`, `"V22B"`, `"E"`, or `"V22_HA"`.
#' @param equation_id Integer. Identifier for the equation to be used within a given volume type.
#' @param carbon Logical. If `TRUE`, calculates aerial biomass and associated carbon content.
#' @param bark Logical. If `TRUE`, estimates bark thickness and adjusts the total volume accordingly.
#' @param remove_na Logical. If `TRUE`, rows with missing computed volume are removed from the output.
#' @param source Character. The source of the allometric equations. Options include `"Dagnellie"`, `"Aflan"`, and `"Vallet"`.
#' @param specimens Character. Column name in `x` indicating species identification (code, abbreviation, or full name).
#'                  Required for matching with internal equations.
#' @param D130, D150 Character. Column names for tree diameter at 130 cm and 150 cm.
#' @param C130, C150 Character. Column names for tree circumference at 130 cm and 150 cm.
#' @param HTOT, HDOM Character. Column names for total tree height and dominant height.
#' @param ... Additional arguments passed to internal methods.
#'
#' @return A data.frame similar to the input with added columns depending on selected options:
#' \itemize{
#'   \item `volume_type` (e.g., "V22") — computed volume.
#'   \item `G130`, `G150` — basal areas at 130 cm and 150 cm (if applicable).
#'   \item `Ecorce_mm` — bark thickness (if `bark = TRUE`).
#'   \item `Biomass_kg` — aerial biomass (if `carbon = TRUE`).
#'   \item `Carbon_kg` — carbon content (if `carbon = TRUE`).
#' }
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   C130 = c(50, 60),
#'   HTOT = c(20, 25),
#'   Species = c("Hetre", "Epicea commun")
#' )
#'
#' result <- carbofor(df,
#'                    volume_type = "V22",
#'                    equation_id = 1,
#'                    carbon = TRUE,
#'                    bark = TRUE,
#'                    specimens = "Species")
#' }
#'
#' @seealso
#' \code{\link{calculate_volume}} — Core function for volume computation using selected allometric models.
#'
#' \code{\link{calculate_biomass}} — Computes aboveground biomass from volume and species coefficients.
#'
#' \code{\link{calculate_carbon}} — Converts biomass into estimated carbon stock.
#'
#' \code{\link{calculate_bark_thickness}} — Estimates bark thickness in mm based on empirical rules.
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


#' Calculate tree volume, bark, biomass and carbon from a data.frame
#'
#' @description
#' Applies species-specific allometric equations to calculate tree volume, and optionally
#' bark thickness/volume and biomass/carbon.
#'
#' @method carbofor carbofor_data
#' @export
#'
#' @param x A data.frame containing individual tree data (must include at least Species and D130).
#' @param equations A data.frame of allometric equations with fields like Y, A0, b0:b5, X1:X5, Species, etc.
#' @param volume_type Character string specifying the type of volume to calculate (e.g., "V22", "V22B").
#' @param equation_id Integer indicating which equation to apply for each species (default = 1).
#' @param carbon Logical. If TRUE, biomass and carbon will be calculated.
#' @param bark Logical. If TRUE, bark thickness and bark/wood volume will be estimated.
#' @param remove_na Logical. If TRUE, rows with NA in the volume column will be removed.
#' @param source Character string specifying the source of the equations (e.g., "Dagnellie").
#' @param specimens A character vector of specimen IDs used for mapping or metadata consistency.
#' @param D130, C130, D150, C150, HTOT, HDOM, H Column names used for diameters, circumferences, and heights.
#' @param biomass_method Character. Method used to compute biomass if `carbon = TRUE`.
#'        Options are `"volume"` (default) or `"equation"`.
#' @param ... Additional arguments passed to subfunctions.
#'
#' @return A data.frame enriched with calculated columns such as:
#' volume (e.g., V22), Validity_Status, Equation_Used, Biomass_Total, Carbon_Total, etc.
#'
#' @examples
#' trees <- data.frame(Species = "Hetre", D130 = 32, HTOT = 25)
#' specimens <- c("FASY001")
#' equations <- get_equation_database()  # User-provided function or object
#' result <- carbofor(
#'   x = trees,
#'   equations = equations,
#'   specimens = specimens,
#'   carbon = TRUE,
#'   bark = TRUE
#' )
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
                                   H = "H",
                                   biomass_method = "volume",
                                   ...) {

  flags <- list(
    C130_exists = FALSE,
    C150_exists = FALSE,
    D130_exists = FALSE,
    D150_exists = FALSE,
    HTOT_exists = FALSE,
    HDOM_exists = FALSE
  )

  # 1. Parameter validation
  validate_parameters(x, volume_type, equation_id, source, specimens, C130, C150, D130, D150, HTOT, HDOM)

  # 2. Data preprocessing
  x <- preprocess_data(x, specimens = specimens, C130 = C130, C150 = C150,
                       D130 = D130, D150 = D150, HTOT = HTOT, HDOM = HDOM)

  # 3. Volume calculation
  x <- calculate_volume(x, volume_type = volume_type, equations, equation_id = equation_id, source = source)

  # 4. Bark calculation (if requested)
  if (bark) {
    x <- calculate_bark_thickness(x, equations, source = source, total_volume_col = volume_type)
  }

  # 5. Biomass and carbon calculation (if requested)
  if (carbon) {
    x <- calculate_biomass(x, equations, method = biomass_method)
    x <- calculate_carbon(x)
  }

  # 6. Individual variance and prediction interval calculation
  x <- calculate_prediction_interval(
    x,
    equations,
    volume_type = volume_type,
    source = source,
    equation_id = 1,
    confidence_level = 0.95
  )

  # 7. Final cleanup (if requested)
  if (remove_na) {
    x <- x[!is.na(x[[volume_type]]), ]
  }

  return(x)
}
