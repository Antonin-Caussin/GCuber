# =====================================================================
#                             as.carbofor_data()
# =====================================================================
#' Convert a data.frame into a carbofor_data object
#'
#' @description
#' Assigns the class "carbofor_data" to a data.frame to allow method dispatching.
#'
#' @param x A data.frame containing tree data.
#'
#' @return The same data.frame with an additional class "carbofor_data".
#' @export
as.carbofor_data <- function(x) {
  class(x) <- c("carbofor_data", class(x))
  return(x)
}

# =====================================================================
#                             update_flags()
# =====================================================================

#' Update Logical Flags for Key Tree Measurement Columns
#'
#' @description
#' This internal utility function checks for the presence of key dendrometric variables
#' (diameters, circumferences, heights) in a data.frame and updates a list of logical
#' flags accordingly. It helps downstream functions know which columns are available
#' for processing.
#'
#' @param x A `data.frame` containing tree-level dendrometric data.
#' @param flags A named list of logical values indicating which variables currently exist.
#'              This list typically includes: `C130_exists`, `C150_exists`, `D130_exists`,
#'              `D150_exists`, `HTOT_exists`, `HDOM_exists`.
#' @param C130 Character. Name of the column for circumference at 130 cm above ground.
#' @param C150 Character. Name of the column for circumference at 150 cm above ground.
#' @param D130 Character. Name of the column for diameter at 130 cm above ground.
#' @param D150 Character. Name of the column for diameter at 150 cm above ground.
#' @param HTOT Character. Name of the column for total tree height.
#' @param HDOM Character. Name of the column for dominant height in the plot.
#'
#' @return A list of updated logical values indicating the presence or absence of each
#'         specified column in the input data.frame.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(D130 = c(30, 32), HTOT = c(25, 27))
#' flags <- list(C130_exists = FALSE, C150_exists = FALSE,
#'               D130_exists = FALSE, D150_exists = FALSE,
#'               HTOT_exists = FALSE, HDOM_exists = FALSE)
#' flags <- update_flags(df, flags, C130 = "C130", C150 = "C150",
#'                       D130 = "D130", D150 = "D150", HTOT = "HTOT", HDOM = "HDOM")
#' print(flags)
#' }
#'
#' @seealso
#' \code{\link{validate_parameters}}, \code{\link{preprocess_data}}
#'
#' @export

update_flags <- function(x, flags, C130, C150, D130, D150, HTOT, HDOM) {
  flags$C130_exists <- C130 %in% colnames(x)
  flags$C150_exists <- C150 %in% colnames(x)
  flags$D130_exists <- D130 %in% colnames(x)
  flags$D150_exists <- D150 %in% colnames(x)
  flags$HTOT_exists <- HTOT %in% colnames(x)
  flags$HDOM_exists <- HDOM %in% colnames(x)
  return(flags)
}

# =====================================================================
#                         preprocess_data()
# =====================================================================
#' Preprocess Tree-Level Data for Allometric Calculations
#'
#' @description
#' This function performs a complete preprocessing pipeline to prepare tree inventory data
#' for volume, biomass, or carbon calculations. It includes:
#' \itemize{
#'   \item Species matching using a reference correspondence table;
#'   \item Conversion between diameter and circumference measurements at breast height;
#'   \item Harmonization between C130 and C150 measurements;
#'   \item Calculation of basal area for each tree record.
#' }
#' These steps ensure the dataset is standardized and consistent with the format expected
#' by downstream modeling functions such as \code{\link{calculate_volume}} or \code{\link{calculate_biomass}}.
#'
#' @param x A `data.frame` containing individual tree measurements. Must include at least one
#' diameter or circumference variable (D130, D150, C130, or C150) and height measurements.
#' @param specimens Optional. Character string giving the name of the column used to identify species.
#' This can be a species code, abbreviation, or full Latin name. Required for species-specific models.
#' @param C130,C150 Character. Column names storing circumference at 130 cm and 150 cm respectively (in cm).
#' @param D130,D150 Character. Column names storing diameter at 130 cm and 150 cm respectively (in cm).
#' @param HTOT Character. Column name for total tree height (in meters).
#' @param HDOM Character. Column name for dominant height (in meters).
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' A `data.frame` identical to the input but augmented with:
#' \itemize{
#'   \item A `species_model` column containing standardized species names used for model selection;
#'   \item Derived diameter or circumference variables if conversions were applied;
#'   \item A `G_m2` column for basal area (in m²) per tree, based on C130 or D130.
#' }
#' The data is guaranteed to have the variables needed for further model prediction functions.
#'
#' @details
#' The function applies the following steps in order:
#' \enumerate{
#'   \item \code{\link{establish_species_correspondence}} — assigns a standard species name;
#'   \item \code{\link{diameter_conversions}} — converts between diameter and circumference if needed;
#'   \item \code{\link{convert_circumference}} — harmonizes C130 and C150 using site coefficients;
#'   \item \code{\link{calculate_basal_areas}} — computes tree-level basal area (G).
#' }
#'
#' All conversions assume measurements are in centimeters (for diameters and circumferences)
#' and meters for heights.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   C130 = c(100, 120),
#'   HTOT = c(22, 27),
#'   HDOM = c(25, 28),
#'   Species = c("HE", "EP")
#' )
#'
#' df_preprocessed <- preprocess_data(
#'   x = df,
#'   specimens = "Species",
#'   C130 = "C130",
#'   HTOT = "HTOT",
#'   HDOM = "HDOM"
#' )
#' head(df_preprocessed)
#' }
#'
#' @seealso
#' \code{\link{validate_parameters}}, \code{\link{calculate_volume}}, \code{\link{calculate_biomass}},
#' \code{\link{calculate_prediction_interval}}, \code{\link{calculate_carbon}}.
#'
#' @export

preprocess_data <- function(x, specimens = NULL, C130 = "C130", C150 = "C150",
                            D130 = "D130", D150 = "D150", HTOT = "HTOT", HDOM = "HDOM", ...) {
  x <- establish_species_correspondence(x, specimens = specimens)
  x <- diameter_conversions(x, C130 = C130, C150 = C150, D130 = D130, D150 = D150)
  x <- convert_circumference(x, C130 = C130, C150 = C150)
  x <- calculate_basal_areas(x, C130 = C130, C150 = C150)
  return(x)
}


# =====================================================================
#               establish_species_correspondence()
# =====================================================================

#' Standardize Species Names Using Internal Correspondence Table
#'
#' @description
#' This function standardizes species names in a tree inventory dataset by mapping
#' species identifiers (codes, abbreviations, or names) to a canonical species name
#' used for model selection. The correspondence is based on the internal `equations`
#' database, which must contain a column named `"Species"` and at least one alternative
#' identification column (e.g., `"Code"`, `"Abbreviation"`).
#'
#' This step ensures consistency across datasets using heterogeneous species identifiers.
#'
#' @param x A `data.frame` containing tree-level inventory data, including a column
#' with species identifiers.
#' @param specimens A character string specifying the column name in `x` that identifies species.
#' Accepted identifiers include codes (e.g., `"FASY"`), abbreviations (e.g., `"F. sylvatica"`), or full names.
#'
#' @return
#' A `data.frame` identical to the input but with a new standardized `Species` column
#' corresponding to the canonical names used in the model database. If no match is found
#' for some records, a warning is issued and `NA` is assigned to the `Species` field.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Detects the type of specimen identifier using \code{\link{detect_specimens_type}}.
#'   \item Loads a mapping between the provided `specimens` column and the standardized `Species` column.
#'   \item Merges this mapping with the input dataset.
#'   \item Handles column name conflicts and ensures `Species` is uniquely assigned.
#' }
#' The internal object `equations` must be available in the environment and include
#' a column `"Species"` along with the corresponding identification column.
#'
#' @section Warnings:
#' If the specified `specimens` column does not exist in `x`, or if `equations` does not
#' contain the appropriate mapping columns, the function throws an error.
#' A warning is issued if any identifiers cannot be matched.
#'
#' @examples
#' \dontrun{
#' # Suppose `equations` contains: Species | Code
#' #                              ---------|------
#' #                              Hetre | HE
#' #                              Epicea commun     | EP
#'
#' df <- data.frame(
#'   TreeID = 1:3,
#'   Code = c("HE", "EP", "XXXX"),
#'   DBH = c(32.1, 28.4, 25.3)
#' )
#'
#' df <- establish_species_correspondence(df, specimens = "Code")
#' head(df)
#' }
#'
#' @seealso
#' \code{\link{detect_specimens_type}}, \code{\link{preprocess_data}}, \code{\link{validate_parameters}},
#' \code{\link{calculate_volume}}, \code{\link{carbofor_species}}
#'
#' @export

establish_species_correspondence <- function(x, specimens) {
  if (is.null(specimens)) {
    stop("No species identification column specified or found in the data.")
  }

  if (!(specimens %in% colnames(x))) {
    stop(paste("The specified column '", specimens, "' does not exist in the data.", sep = ""))
  }

  specimens_type <- detect_specimens_type(x, specimens)

  if (specimens_type != "Species" || specimens != "Species") {
    if (!all(c("Species", specimens_type) %in% colnames(equations))) {
      stop(paste("The 'equations' dataframe must contain the columns 'Species' and '",
                 specimens_type, "'", sep = ""))
    }

    mapping_df <- unique(equations[, c("Species", specimens_type)])
    names(mapping_df) <- c("Species", specimens)

    conflicts <- intersect(names(x), names(mapping_df))
    conflicts <- setdiff(conflicts, specimens)
    if (length(conflicts) > 0) {
      x <- x[, !(names(x) %in% conflicts)]
    }

    x <- merge(x, mapping_df, by = specimens, all.x = TRUE)

    if ("Species.x" %in% colnames(x)) {
      names(x)[names(x) == "Species.x"] <- "Species"
      if ("Species.y" %in% colnames(x)) {
        x$Species.y <- NULL
      }
    }

    missing_mask <- is.na(x$Species)
    if (any(missing_mask)) {
      na_values <- unique(x[[specimens]][is.na(x$Species)])
      warning(paste("No correspondence found for the following values of",
                    specimens, ":", paste(na_values, collapse = ", ")))
    }
  } else {
    names(x)[names(x) == "Species"] <- "Species"
  }

  return(x)
}


# =====================================================================
#                    detect_specimens_type()
# =====================================================================

#' Detect the Format of Species Identifiers in Tree Inventory Data
#'
#' @description
#' Automatically infers the type of species identifier provided in a given column,
#' based on the data type and the average string length. This function helps
#' distinguish between:
#' \itemize{
#'   \item \code{"Code"}: numerical species codes (e.g., \code{3} for Hetre, \code{41} for epicea commun);
#'   \item \code{"Abr"}: short alphanumeric abbreviations (e.g., \code{"HE"} for Hetre, \code{"EP"} for epicea commun);
#'   \item \code{"Species"}: full species names (e.g., \code{"Hetre"}, \code{"epicea commun"}).
#' }
#' The function is used internally to standardize species identifiers before applying
#' species-specific allometric models.
#'
#' @param x A `data.frame` containing the column to analyze.
#' @param specimens A character string indicating the name of the column to inspect for species identifiers.
#'
#' @return
#' A character string indicating the detected format of the species identifier.
#' One of: \code{"Code"}, \code{"Abr"}, or \code{"Species"}.
#'
#' @details
#' The function first removes missing values, then:
#' \itemize{
#'   \item Returns \code{"Code"} if the non-missing values are numeric;
#'   \item Returns \code{"Abr"} if the mean number of characters is ≤ 4;
#'   \item Returns \code{"Species"} otherwise.
#' }
#' If the column contains only missing values, or an unsupported data type, an error is raised.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Code = c(3, 41, 3),
#'   Abr = c("HE", "EP", "HE"),
#'   Species = c("Hetre", "epicea commun", "Hetre")
#' )
#'
#' detect_specimens_type(df, specimens = "Code")     # Returns "Code"
#' detect_specimens_type(df, specimens = "Abr")      # Returns "Abr"
#' detect_specimens_type(df, specimens = "Species")  # Returns "Species"
#' }
#'
#' @seealso
#' \code{\link{establish_species_correspondence}}, \code{\link{preprocess_data}},
#' \code{\link{validate_parameters}}
#'
#' @importFrom stats na.omit
#' @export


detect_specimens_type <- function(x, specimens) {
  sample_values <- na.omit(x[[specimens]])

  if (length(sample_values) == 0) {
    stop(paste("Column '", specimens, "' contains only missing values.", sep = ""))
  }

  if (is.numeric(sample_values)) {
    specimens_type <- "Code"
  } else if (is.character(sample_values) || is.factor(sample_values)) {
    if (is.factor(sample_values)) {
      sample_values <- as.character(sample_values)
    }
    mean_length <- mean(nchar(sample_values))
    specimens_type <- if (mean_length <= 4) "Abr" else "Species"
  } else {
    stop(paste("Data type in column '", specimens, "' is not recognized.", sep = ""))
  }

  return(specimens_type)
}


# =====================================================================
#                    diameter_conversions()
# =====================================================================

#' Convert Between Diameter and Circumference at Breast Height
#'
#' @description
#' Completes missing values of tree diameter or circumference at 130 cm and 150 cm
#' above ground using the relationship \eqn{C = \pi \cdot D}. This bidirectional conversion
#' ensures consistency of forest inventory data when either variable is missing.
#'
#' @param x A \code{data.frame} containing tree-level dendrometric measurements. Must include at least one of the columns specified in \code{C130}, \code{C150}, \code{D130}, or \code{D150}.
#' @param C130 Character. Name of the column containing circumference at 130 cm (in cm).
#' @param C150 Character. Name of the column containing circumference at 150 cm (in cm).
#' @param D130 Character. Name of the column containing diameter at 130 cm (in cm).
#' @param D150 Character. Name of the column containing diameter at 150 cm (in cm).
#' @param HTOT Character. Name of the column containing total tree height. This parameter is currently unused but retained for compatibility.
#' @param HDOM Character. Name of the column containing dominant height. This parameter is currently unused but retained for compatibility.
#'
#' @return
#' A \code{data.frame} identical to the input, with missing diameter or circumference values
#' imputed where possible using the relationship \eqn{C = \pi \cdot D} or \eqn{D = C / \pi}.
#' Columns that do not exist are created and filled with \code{NA_real_} if necessary.
#'
#' @details
#' This function does not overwrite existing values. It only fills in missing values
#' based on the complementary measurement. If a conversion is expected but the required
#' source variable is \code{NA}, a warning is issued.
#'
#' @seealso
#' \code{\link{convert_circumference}}, \code{\link{calculate_basal_areas}}, \code{\link{preprocess_data}}
#'
#' @examples
#' df <- data.frame(D130 = c(30, NA), C130 = c(NA, 94.2))
#' df <- diameter_conversions(df, C130 = "C130", C150 = "C150", D130 = "D130", D150 = "D150")
#' print(df)
#'
#' @export




diameter_conversions <- function(x, C130, C150, D130, D150, HTOT = "HTOT", HDOM = "HDOM") {
  pi_val <- pi

  # Ensure columns exist; if not, create them with NA
  if (!(C130 %in% names(x))) x[[C130]] <- NA_real_
  if (!(C150 %in% names(x))) x[[C150]] <- NA_real_
  if (!(D130 %in% names(x))) x[[D130]] <- NA_real_
  if (!(D150 %in% names(x))) x[[D150]] <- NA_real_

  # Convert D130 -> C130
  to_fill <- is.na(x[[C130]]) & !is.na(x[[D130]])
  x[[C130]][to_fill] <- x[[D130]][to_fill] * pi_val

  # Convert D150 -> C150
  to_fill <- is.na(x[[C150]]) & !is.na(x[[D150]])
  x[[C150]][to_fill] <- x[[D150]][to_fill] * pi_val

  # Convert C130 -> D130
  to_fill <- is.na(x[[D130]]) & !is.na(x[[C130]])
  x[[D130]][to_fill] <- x[[C130]][to_fill] / pi_val

  # Convert C150 -> D150
  to_fill <- is.na(x[[D150]]) & !is.na(x[[C150]])
  x[[D150]][to_fill] <- x[[C150]][to_fill] / pi_val

  # Emit warnings if some conversions were expected but not possible
  if (sum(!is.na(x[[D130]])) > 0 && sum(is.na(x[[C130]])) > 0) {
    warning("Some D130 -> C130 conversions failed (missing or unprocessed values).")
  }
  if (sum(!is.na(x[[D150]])) > 0 && sum(is.na(x[[C150]])) > 0) {
    warning("Some D150 -> C150 conversions failed (missing or unprocessed values).")
  }

  return(x)
}



# =====================================================================
#                    convert_circumference()
# =====================================================================

#' Harmonize C130 and C150 Using Species-Specific Conversion Models
#'
#' @description
#' Converts between circumference at 130 cm and 150 cm above ground using linear models
#' derived from species-specific calibration coefficients. The formula used is:
#' \eqn{C_{target} = HV \cdot C_{source} + IV}, where \eqn{HV} and \eqn{IV} are parameters
#' retrieved from an internal equations table.
#'
#' @param x A \code{data.frame} containing at least a \code{Species} column and either \code{C130} or \code{C150}.
#' @param D150 Character. Name of the column for diameter at 150 cm (default: \code{"D150"}).
#' @param D130 Character. Name of the column for diameter at 130 cm (default: \code{"D130"}).
#' @param C150 Character. Name of the column for circumference at 150 cm (default: \code{"C150"}).
#' @param C130 Character. Name of the column for circumference at 130 cm (default: \code{"C130"}).
#' @param HTOT Character. Column name for total tree height (not used here but passed for consistency).
#' @param HDOM Character. Column name for dominant height (not used here but passed for consistency).
#'
#' @return
#' A \code{data.frame} with harmonized \code{C130} and/or \code{C150} columns and updated diameters
#' \code{D130} and \code{D150} derived from the converted circumferences.
#'
#' @details
#' The direction of conversion is automatically inferred based on which of \code{C130} or \code{C150} is missing.
#' If both are available, a bidirectional check is performed. The species-specific coefficients \code{HV} and \code{IV}
#' must be available in a global object named \code{equations}.
#'
#' @seealso
#' \code{\link{diameter_conversions}}, \code{\link{calculate_basal_areas}}, \code{\link{preprocess_data}}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Species = "FASY", C150 = 105)
#' equations <- data.frame(Species = "FASY", HV = 0.97, IV = 1.5)
#' df <- convert_circumference(df)
#' head(df)
#' }
#'
#' @export

convert_circumference <- function(x, D150 = "D150", D130 = "D130",
                                  C150 = "C150", C130 = "C130",
                                  HTOT = "HTOT", HDOM = "HDOM") {
  C130_has_values <- C130 %in% names(x) && any(!is.na(x[[C130]]))
  C150_has_values <- C150 %in% names(x) && any(!is.na(x[[C150]]))

  skip_conversion <- FALSE

  if (!C130_has_values && C150_has_values) {
    direction <- "C150_to_C130"
    from_col <- C150
    to_col <- C130
  } else if (!C150_has_values && C130_has_values) {
    direction <- "C130_to_C150"
    from_col <- C130
    to_col <- C150
  } else if (C130_has_values && C150_has_values) {
    direction <- "bidirectional"
  } else {
    skip_conversion <- TRUE
  }

  if (!skip_conversion) {
    coefs_x <- unique(equations[, c("Species", "HV", "IV")])

    for (i in seq_len(nrow(x))) {
      tree_species <- x$Species[i]
      if (is.na(tree_species)) next
      coef_row <- coefs_x[coefs_x$Species == tree_species, ]

      if (nrow(coef_row) == 0 || is.na(coef_row$HV[1]) || is.na(coef_row$IV[1])) next

      HV <- coef_row$HV[1]
      IV <- coef_row$IV[1]

      # Bidirectional logic
      if (direction == "bidirectional") {
        if (is.na(x[[C130]][i]) && !is.na(x[[C150]][i])) {
          x[[C130]][i] <- HV * x[[C150]][i] + IV
        } else if (!is.na(x[[C130]][i]) && is.na(x[[C150]][i])) {
          x[[C150]][i] <- (x[[C130]][i] - IV) / HV
        }
      } else {
        from_value <- x[[from_col]][i]
        to_value <- x[[to_col]][i]

        if (is.na(to_value) && !is.na(from_value)) {
          x[[to_col]][i] <- if (direction == "C150_to_C130") {
            HV * from_value + IV
          } else {
            (from_value - IV) / HV
          }
        }
      }
    }

    # Recalculate diameters from circumferences
    pi_val <- pi
    if (!(D130 %in% colnames(x))) x[[D130]] <- NA_real_
    if (!(D150 %in% colnames(x))) x[[D150]] <- NA_real_

    for (i in seq_len(nrow(x))) {
      if (is.na(x[[D130]][i]) && !is.na(x[[C130]][i])) {
        x[[D130]][i] <- x[[C130]][i] / pi_val
      }
      if (is.na(x[[D150]][i]) && !is.na(x[[C150]][i])) {
        x[[D150]][i] <- x[[C150]][i] / pi_val
      }
    }
  }

  return(x)
}

# =====================================================================
#                    calculate_basal_areas()
# =====================================================================

#' Compute Basal Area from Tree Circumference
#'
#' @description
#' Calculates the basal area (in m²) for individual trees using circumference at breast height.
#' The formula used is: \eqn{G = C^2 / (4 \pi \cdot 10^4)}, where \eqn{C} is in cm and \eqn{G} is in m².
#'
#' @param x A \code{data.frame} containing tree-level measurements.
#' @param C130 Character. Name of the column with circumference at 130 cm (in cm).
#' @param C150 Character. Name of the column with circumference at 150 cm (in cm).
#'
#' @return
#' A \code{data.frame} identical to the input, with additional columns:
#' \itemize{
#'   \item \code{G130}: basal area computed from \code{C130} (if available);
#'   \item \code{G150}: basal area computed from \code{C150} (if available).
#' }
#' Basal areas are returned in square meters.
#'
#' @details
#' This function does not overwrite existing columns. Only \code{G130} or \code{G150}
#' not already present are computed. The circumference must be in centimeters;
#' the resulting basal area is in square meters.
#'
#' @seealso
#' \code{\link{preprocess_data}}, \code{\link{diameter_conversions}}, \code{\link{convert_circumference}}
#'
#' @examples
#' df <- data.frame(C130 = c(100, 120), C150 = c(105, 125))
#' df <- calculate_basal_areas(df, C130 = "C130", C150 = "C150")
#' head(df)
#'
#' @export

calculate_basal_areas <- function(x, C130, C150) {
  if (!"G130" %in% colnames(x) && C130 %in% colnames(x)) {
    x$G130 <- (x[[C130]]^2) / ((4 * pi) * 10000)
  }

  if (!"G150" %in% colnames(x) && C150 %in% colnames(x)) {
    x$G150 <- (x[[C150]]^2) / ((4 * pi) * 10000)
  }

  return(x)
}

