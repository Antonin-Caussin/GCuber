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
#' Update existence flags for key column names
#'
#' @description
#' Checks whether key variables (diameters, circumferences, heights) exist in the input data
#' and updates a logical flag list accordingly.
#'
#' @param x A data.frame containing tree data.
#' @param flags A named list of logicals tracking existence of key variables.
#' @param C130, C150, D130, D150, HTOT, HDOM Column names to check in the data.frame.
#'
#' @return An updated list of logical flags indicating column presence.
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
#' by downstream modeling functions such as \code{\link{calculate_volumes}} or \code{\link{calculate_biomass}}.
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
#' \code{\link{validate_parameters}}, \code{\link{calculate_volumes}}, \code{\link{calculate_biomass}},
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
#' \code{\link{calculate_volumes}}, \code{\link{carbofor_species}}
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

#' Convert Between Diameter and Circumference Measurements
#'
#' @description
#' Converts missing diameter or circumference values at breast height
#' using the formula: \code{C = pi * D}. The function works in both directions:
#' \itemize{
#'   \item If diameter is known and circumference is missing, it computes circumference;
#'   \item If circumference is known and diameter is missing, it computes diameter.
#' }
#' The function supports measurements at both 130 cm and 150 cm above ground.
#'
#' @param x A data.frame containing tree inventory data with at least one of the diameter or circumference columns.
#' @param C130 Character. Name of the column storing circumference at 130 cm above ground (in cm).
#' @param C150 Character. Name of the column storing circumference at 150 cm above ground (in cm).
#' @param D130 Character. Name of the column storing diameter at 130 cm above ground (in cm).
#' @param D150 Character. Name of the column storing diameter at 150 cm above ground (in cm).
#' @param HTOT Character (unused). Name of the column for total height, included for compatibility.
#' @param HDOM Character (unused). Name of the column for dominant height, included for compatibility.
#'
#' @return
#' A data.frame identical to the input but with missing diameter or circumference values filled when possible.
#' Columns are created if they do not exist in the original dataset.
#'
#' @details
#' The function ensures all four measurement columns exist in the dataset. If one is missing,
#' it is created with \code{NA_real_}. Then, it attempts to complete missing values using
#' the available ones with the relationships:
#' \itemize{
#'   \item \code{C = pi * D}
#'   \item \code{D = C / pi}
#' }
#'
#' If conversions are expected (i.e., the input diameter exists but circumference is missing),
#' and the output is still \code{NA}, a warning is issued.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   D130 = c(30, NA),
#'   C130 = c(NA, 94.2),
#'   D150 = c(28, NA),
#'   C150 = c(NA, NA)
#' )
#'
#' result <- diameter_conversions(
#'   x = df,
#'   C130 = "C130",
#'   C150 = "C150",
#'   D130 = "D130",
#'   D150 = "D150"
#' )
#'
#' print(result)
#' }
#'
#' @seealso
#' \code{\link{preprocess_data}}, \code{\link{calculate_basal_areas}}, \code{\link{convert_circumference}}
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

#' Harmonize C130 and C150 using species-specific conversion coefficients
#'
#' @description
#' Performs bidirectional or directional conversion between C130 and C150 using
#' species-specific linear models (C150 = HV × C130 + IV). Coefficients must be present in
#' the `equations` data.frame.
#'
#' @param x A data.frame containing at least a Species column and one of the circumference columns.
#' @param D150, D130 Column names for diameters (optional, only updated at the end).
#' @param C150, C130 Column names for circumferences.
#' @param HTOT, HDOM Column names for height variables (not used here but passed for compatibility).
#'
#' @return The input data.frame with harmonized C130/C150 and updated diameters if applicable.
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

#' Calculate Basal Area from Tree Circumference
#'
#' @description
#' Computes the basal area (in square meters) at breast height for each tree
#' using the circumference at 130 cm and/or 150 cm above ground. The formula used is:
#' \deqn{G = C^2 / (4 * pi * 10000)}
#' where \eqn{C} is the circumference in centimeters and \eqn{G} is the basal area in square meters.
#'
#' @param x A data.frame containing tree inventory data.
#' @param C130 Character. Name of the column storing circumference at 130 cm (in cm).
#' @param C150 Character. Name of the column storing circumference at 150 cm (in cm).
#'
#' @return
#' The input data.frame with one or both of the following columns added:
#' \itemize{
#'   \item \code{G130}: basal area based on \code{C130};
#'   \item \code{G150}: basal area based on \code{C150}.
#' }
#' Only columns not already present in the data are created. If the corresponding circumference column
#' is missing, the basal area is not computed.
#'
#' @details
#' The result is expressed in square meters per tree. The division by 10,000 converts from cm² to m².
#' The function is typically used in preprocessing steps before applying volume or biomass equations.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(C130 = c(100, 120), C150 = c(105, 125))
#' df <- calculate_basal_areas(df, C130 = "C130", C150 = "C150")
#' head(df)
#' }
#'
#' @seealso
#' \code{\link{preprocess_data}}, \code{\link{diameter_conversions}}, \code{\link{convert_circumference}}
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

