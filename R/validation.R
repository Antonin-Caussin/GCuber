# =====================================================================
#                   validate_parameters() (interne)
# =====================================================================

#' Validate input parameters and required columns (internal)
#'
#' @description
#' Internal helper that checks the consistency of user inputs and the presence
#' of required dendrometric columns prior to computation. Stops with informative
#' errors or issues warnings when inconsistencies are detected.
#'
#' @param x A data frame with tree inventory data.
#' @param volume_type Character. Target volume type to compute.
#' @param equation_id Integer. Index of the model to use when several are available.
#' @param source Character. Equation source identifier (e.g., "Dagnelie").
#' @param specimens Character or \code{NULL}. Column name used to identify species.
#' @param C130,C150,D130,D150,HTOT,HDOM Character. Column names for dendrometric variables.
#'
#' @return Invisibly returns \code{TRUE} when validation passes; otherwise throws an error or warning.
#'
#' @seealso \code{\link{preprocess_data}}, \code{\link{carbofor.carbofor_data}}
#' @keywords internal
#' @examples
#' \dontrun{
#' # Valid dataset: all required columns are present
#' df <- data.frame(
#'   Espece = "Hetre",
#'   D130   = 32,
#'   C130   = 100,
#'   HTOT   = 25,
#'   HDOM   = 27
#' )
#'
#' # Successful validation
#' validate_parameters(
#'   x = df,
#'   volume_type = "V22",
#'   equation_id = 1,
#'   source = "Dagnelie",
#'   specimens = "Espece",
#'   C130 = "C130", C150 = "C150",
#'   D130 = "D130", D150 = "D150",
#'   HTOT = "HTOT", HDOM = "HDOM"
#' )
#'
#' # Example with missing column (D130 is absent)
#' df2 <- data.frame(
#'   Espece = "Chene sessile",
#'   C130   = 120,
#'   HTOT   = 30,
#'   HDOM   = 32
#' )
#'
#' # Generates an error: missing D130 column
#' validate_parameters(
#'   x = df2,
#'   volume_type = "V22",
#'   equation_id = 1,
#'   source = "Dagnelie",
#'   specimens = "Espece",
#'   C130 = "C130", C150 = "C150",
#'   D130 = "D130", D150 = "D150",
#'   HTOT = "HTOT", HDOM = "HDOM"
#' )
#'
#' # Example with invalid volume_type
#' validate_parameters(
#'   x = df,
#'   volume_type = "InvalidType",
#'   equation_id = 1,
#'   source = "Dagnelie",
#'   specimens = "Espece",
#'   C130 = "C130", C150 = "C150",
#'   D130 = "D130", D150 = "D150",
#'   HTOT = "HTOT", HDOM = "HDOM"
#' )
#' }



validate_parameters <- function(x,
                                volume_type = "V22",
                                equation_id = 1,
                                source = "Dagnelie",
                                specimens = NULL,
                                C130 = "C130", C150 = "C150",
                                D130 = "D130", D150 = "D150",
                                HTOT = "HTOT", HDOM = "HDOM") {
  valid_sources <- c("Dagnelie", "Algan", "Vallet", "Bouvard", "Courbet", "Rondeux")
  if (!source %in% valid_sources) {
    stop(paste("Invalid source:", source, "\nValid sources:", paste(valid_sources, collapse = ", ")))
  }

  valid_volume_types <- c("V22", "V22B", "V22_HA", "Aboveground", "Merchantable")
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


    if (!(specimens %in% names(x)) && length(intersect(setdiff(required_columns, specimens), names(x))) > 0) {
      warning(paste(
        "Missing columns in data: none of the required columns found. Expected at least one of:",
        specimens
      ), call. = FALSE)
    }
  }

  if (length(intersect(required_columns, names(x))) == 0) {
    warning(paste(
      "Missing columns in data: none of the required columns found. Expected at least one of:",
      paste(required_columns, collapse = ", ")
    ), call. = FALSE)
  }

  if (!any(c(C130, C150, D130, D150) %in% names(x))) {
    stop("No diameter or circumference column found in the data.")
  }

  return(x)
}
