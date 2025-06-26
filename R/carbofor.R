#' Calculate Tree Volumes Using Allometric Equations
#'
#' This function calculates various types of tree volumes ("V22", "V22B", "E", "V22_HA") using
#' species-specific allometric equations. It handles data preprocessing including species
#' identification, diameter/circumference conversions, and basal area calculations.
#'
#' @param df A data.frame containing tree measurements with columns for circumferences,
#' diameters, heights, and species identification.
#' @param volume_type Character string specifying the type of volume to calculate.
#' Must be one of: "V22", "V22B", "E", "V22_HA". Default is "V22".
#' @param equation_id Integer specifying which equation to use within the volume type.
#' Valid values depend on volume_type:
#' \itemize{
#'   \item For "V22": 1, 2, or 3
#'   \item For "V22_HA": 1 only
#'   \item For "V22B": 1 only
#'   \item For "E": automatically determined (4 or 5) based on species
#' }
#' Default is 1.
#' @param specimens Character string specifying the column name containing species
#' identification. Can be species codes, abbreviations, or full species names.
#' Default is NULL.
#' @param C130, C150, D130, D150, HTOT, HDOM Character strings specifying the column names
#' for circumference/diameter at 1.30m or 1.50m, total height, and dominant height.
#' @param remove_na Logical indicating whether to remove rows with NA volume results.
#' Default is FALSE.
#' @param carbon Logical. If \code{TRUE}, calculates carbon content in addition to volume.
#' Default is \code{FALSE}.
#' @param source Character string specifying the source of equations to use.
#' Default is \code{"Dagnellie"}.
#' @param bark Logical. If \code{TRUE}, includes bark in calculations. If \code{FALSE}, calculations are done without bark.
#' Default is \code{FALSE}.
#'
#' @return A data.frame with the original data plus additional columns:
#' \itemize{
#'   \item Species: Standardized species names
#'   \item C130/C150: Circumferences (converted from diameters if needed)
#'   \item D130/D150: Diameters (calculated from circumferences if needed)
#'   \item G130/G150: Basal areas (calculated from circumferences)
#'   \item Volume column named according to volume_type parameter
#'   \item Equation_Used: Information about which equation was applied
#'   \item Validity_Status: Whether the tree is within the equation's validity domain
#'   \item Relative_Interval_Width: Width of prediction interval relative to volume
#'   \item Interval_Interpretation: Qualitative interpretation of prediction interval
#' }
#'
#' @details
#' The function performs several preprocessing steps:
#' \enumerate{
#'   \item Parameter validation
#'   \item Species correspondence
#'   \item Unit conversions (diameter <-> circumference)
#'   \item Basal area calculation
#'   \item Volume calculation
#'   \item Optional bark and carbon calculations
#'   \item Optional removal of failed rows
#' }
#'
#' @section Dependencies:
#' Requires a global \code{equations} data.frame with columns:
#' \itemize{
#'   \item Species, Y, A0, b0-b5, X1-X5, HV, IV, Code, Abr
#' }
#'
#' @section Error Handling:
#' \itemize{
#'   \item Input validation
#'   \item Missing species correspondences
#'   \item Invalid mathematical expressions
#'   \item Missing coefficients
#' }
#'
#' @examples
#' \dontrun{
#' data <- data.frame(Species = c("Picea abies", "Fagus sylvatica"),
#'                    C130 = c(94.2, 125.6),
#'                    HTOT = c(25.5, 28.2))
#' result <- carbofor(data, volume_type = "V22", equation_id = 1)
#' }
#'
#' @author Antonin Caussin
#' @references
#' Dagnelie, P., Rondeux, J., & Thill, A. (1985). Tables de cubage des arbres et des peuplements forestiers. Gembloux, Belgique: Presses agronomiques de Gembloux.
#'
#' @keywords forest dendrometry volume allometry
#' @concept tree volume calculation
#' @concept allometric equations
#' @concept forest inventory
#'
#' @importFrom stats na.omit
#' @importFrom utils head
#' @export

carbofor <- function(x, ...) {
  UseMethod("carbofor")
}

#' @export
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
  x <- calculate_individual_variance(x, volume_type = volume_type, equation_id = equation_id)

  # 7. Nettoyage final si demandé
  if (remove_na) {
    x <- x[!is.na(x[[volume_type]]), ]
  }

  return(x)
}
