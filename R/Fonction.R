#' Calculate Tree Volumes Using Allometric Equations
#'
#' This function calculates various types of tree volumes (V22, V22B, E, V22_HA) using
#' species-specific allometric equations. It handles data preprocessing including species
#' identification, diameter/circumference conversions, and basal area calculations.
#'
#' @param df A data.frame containing tree measurements with columns for circumferences,
#'   diameters, heights, and species identification.
#' @param volume_type Character string specifying the type of volume to calculate.
#'   Must be one of: "V22", "V22B", "E", "V22_HA". Default is "V22".
#' @param equation_id Integer specifying which equation to use within the volume type.
#'   Valid values depend on volume_type:
#'   \itemize{
#'     \item For "V22": 1, 2, or 3
#'     \item For "V22_HA": 1 only
#'     \item For "V22B": 1 only
#'     \item For "E": automatically determined (4 or 5) based on species
#'   }
#'   Default is 1.
#' @param specimens Character string specifying the column name containing species
#'   identification. Can be species codes, abbreviations, or full species names.
#'   Default is NULL.
#' @param C130 Character string specifying the column name for circumference at 1.30m.
#'   Default is "C130".
#' @param C150 Character string specifying the column name for circumference at 1.50m.
#'   Default is "C150".
#' @param D130 Character string specifying the column name for diameter at 1.30m.
#'   Default is "D130".
#' @param D150 Character string specifying the column name for diameter at 1.50m.
#'   Default is "D150".
#' @param HTOT Character string specifying the column name for total height.
#'   Default is "HTOT".
#' @param HDOM Character string specifying the column name for dominant height.
#'   Default is "HDOM".
#' @param remove_na Logical indicating whether to remove rows with NA volume results.
#'   Default is FALSE.
#'
#' @return A data.frame with the original data plus additional columns:
#'   \itemize{
#'     \item Species: Standardized species names
#'     \item C130/C150: Circumferences (converted from diameters if needed)
#'     \item D130/D150: Diameters (calculated from circumferences if needed)
#'     \item G130/G150: Basal areas (calculated from circumferences)
#'     \item Volume column named according to volume_type parameter
#'     \item Equation_Used: Information about which equation was applied
#'   }
#'
#' @details
#' The function performs several preprocessing steps:
#' \enumerate{
#'   \item \strong{Parameter validation}: Checks input parameters and data structure
#'   \item \strong{Species correspondence}: Maps species codes/abbreviations to full names
#'   \item \strong{Unit conversions}: Converts between diameters and circumferences using pi
#'   \item \strong{Basal area calculation}: Computes G = C²/(4pi*10000)
#'   \item \strong{Volume calculation}: Applies species-specific allometric equations
#' }
#'
#' \strong{Volume Types:}
#' \itemize{
#'   \item \strong{V22}: Standard volume equations with linear combinations
#'   \item \strong{V22B}: Specific biomass-related volume calculations
#'   \item \strong{E}: Logarithmic equations (A0=4: log form, A0=5: alternative)
#'   \item \strong{V22_HA}: Height-adjusted volume calculations
#' }
#'
#' \strong{Species Identification:}
#' The function automatically detects the type of species identification:
#' \itemize{
#'   \item \strong{Code}: Numeric species codes
#'   \item \strong{Abr}: Character abbreviations (≤4 characters)
#'   \item \strong{Species}: Full species names (>4 characters)
#' }
#'
#' \strong{Equation Types (A0 parameter):}
#' \itemize{
#'   \item \strong{A0 = 1, 2, 3, 5}: Linear equations of form: Volume = b0 + Σ(bi * Xi)
#'   \item \strong{A0 = 4}: Logarithmic equations: Volume = 10^(b0 + b1 * log(C130))
#' }
#'
#' @section Dependencies:
#' This function requires access to a global \code{equations} data.frame containing:
#' \itemize{
#'   \item Species: Species names
#'   \item Y: Volume type identifier
#'   \item A0: Equation type identifier
#'   \item b0-b5: Equation coefficients
#'   \item X1-X5: Variable expressions
#'   \item HV, IV: Conversion coefficients for circumference transformations
#'   \item Code, Abr: Species identification mappings
#' }
#'
#' @section Warning Messages:
#' The function provides extensive debugging output and warnings for:
#' \itemize{
#'   \item Missing or invalid data
#'   \item Failed conversions
#'   \item Missing species correspondences
#'   \item Equation evaluation errors
#'   \item Invalid mathematical operations (e.g., log of negative values)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with circumference data
#' data <- data.frame(
#'   Species = c("Picea abies", "Fagus sylvatica", "Quercus robur"),
#'   C130 = c(94.2, 125.6, 157.1),
#'   HTOT = c(25.5, 28.2, 22.8)
#' )
#'
#' result <- calculate_volumes(data, volume_type = "V22", equation_id = 1)
#'
#' # Using diameter data with automatic conversion
#' data_diam <- data.frame(
#'   Code = c(1, 2, 3),
#'   D130 = c(30, 40, 50),
#'   HTOT = c(25, 28, 23)
#' )
#'
#' result <- calculate_volumes(data_diam,
#'                           volume_type = "V22B",
#'                           specimens = "Code")
#'
#' # Volume type E with automatic equation selection
#' result_E <- calculate_volumes(data, volume_type = "E")
#'
#' # Remove rows with failed calculations
#' result_clean <- calculate_volumes(data,
#'                                 volume_type = "V22_HA",
#'                                 remove_na = TRUE)
#' }
#'
#' @note
#' The function displays information messages during execution to facilitate debugging.
#' Warnings are issued if species correspondences are not found or if
#' volume calculation fails for certain rows.
#'
#' @seealso
#' Related functions for forest management and dendrometric calculations.
#'
#' @author Caussin Antonin
#' @references
#' Dagnelie, P., Rondeux, J., & Thill, A. (1985). Tables de cubage des arbres et des peuplements forestiers. Gembloux, Belgique: Presses agronomiques de Gembloux.
#'
#' @importFrom stats na.omit
#' @export

calculate_volumes <- function(df, volume_type = "V22",
                              equation_id = 1,
                              specimens = NULL,
                              C130 = "C130",
                              C150 = "C150",
                              D130 = "D130",
                              D150 = "D150",
                              HTOT = "HTOT",
                              HDOM = "HDOM",
                              remove_na = FALSE) {

  # =========================================================================
  # INTERNAL FUNCTIONS
  # =========================================================================

  # Input parameter validation
  validate_parameters <- function() {
    # Warning for type E with unnecessary equation_id
    if (volume_type == "E" && equation_id %in% c(1, 2, 3, 4, 5)) {
      warning(paste("WARNING: For volume type 'E', it is not necessary to specify equation_id.",
                    "You have specified equation_id =", equation_id,
                    "which might not be suitable or might not process the entire dataset."))
    }

    # Volume type verification
    valid_volume_types <- c("V22", "V22B", "E", "V22_HA")
    if (!(volume_type %in% valid_volume_types)) {
      stop(paste("Invalid volume type:", volume_type,
                 "\nValid types:", paste(valid_volume_types, collapse = ", ")))
    }

    # Correspondence between volume_type and equation_id
    if (volume_type == "V22" && !(equation_id %in% 1:3)) {
      stop("For volume type 'V22', equation_id must be between 1 and 3.")
    }
    if (volume_type == "V22_HA" && equation_id != 1) {
      stop("For volume type 'V22_HA', equation_id must be 1.")
    }
    if (volume_type == "V22B" && equation_id != 1) {
      stop("For volume type 'V22B', equation_id must be 1.")
    }

    # Check existence of columns in user dataframe
    required_columns <- c(C130, C150, D130, D150, HTOT, HDOM, specimens)
    present_columns <- required_columns[required_columns %in% colnames(df)]
    missing_columns <- setdiff(required_columns, present_columns)

    # Assignment of global flags
    assign("C130_exists", C130 %in% colnames(df), envir = .GlobalEnv)
    assign("C150_exists", C150 %in% colnames(df), envir = .GlobalEnv)
    assign("D130_exists", D130 %in% colnames(df), envir = .GlobalEnv)
    assign("D150_exists", D150 %in% colnames(df), envir = .GlobalEnv)
    assign("HTOT_exists", HTOT %in% colnames(df), envir = .GlobalEnv)
    assign("HDOM_exists", HDOM %in% colnames(df), envir = .GlobalEnv)

    # Check circumference / diameter pairs
    if (!C130_exists && !C150_exists && !D130_exists && !D150_exists) {
      stop("No circumference column (C130 or C150) or diameter column (D130 or D150) found in the data.")
    }

    if ((!C130_exists && !C150_exists) && (D130_exists || D150_exists)) {
      message("No circumference (C130 or C150) is available, but diameters are present.")
    }

    if ((C130_exists || C150_exists) && (!D130_exists && !D150_exists)) {
      message("Circumferences are present, but no diameter (D130 or D150) was found.")
    }

    if (length(missing_columns) > 0) {
      warning(paste("Missing columns in data:", paste(missing_columns, collapse = ", ")))
    }
    #debug
    cat("[DEBUG] ==================== PARAMETER VALIDATION ====================\n")
    cat("[DEBUG] Received parameters:\n")
    cat("[DEBUG]   - volume_type =", volume_type, "\n")
    cat("[DEBUG]   - equation_id =", equation_id, "\n")
    cat("[DEBUG]   - specimens =", ifelse(is.null(specimens), "NULL", specimens), "\n")
    cat("[DEBUG]   - remove_na =", remove_na, "\n")
    cat("[DEBUG] Input dataframe dimensions: [", nrow(df), "x", ncol(df), "]\n")
    cat("[DEBUG] Available columns:", paste(colnames(df), collapse = ", "), "\n")
    cat("[DEBUG] Required columns:", paste(required_columns, collapse = ", "), "\n")
    cat("[DEBUG] Column flags defined:\n")
    cat("[DEBUG]   - C130_exists =", C130_exists, "\n")
    cat("[DEBUG]   - C150_exists =", C150_exists, "\n")
    cat("[DEBUG]   - D130_exists =", D130_exists, "\n")
    cat("[DEBUG]   - D150_exists =", D150_exists, "\n")
    cat("[DEBUG]   - HTOT_exists =", HTOT_exists, "\n")
    cat("[DEBUG]   - HDOM_exists =", HDOM_exists, "\n")
    if (length(missing_columns) > 0) {
      cat("[DEBUG] [Warning]  Missing columns:", paste(missing_columns, collapse = ", "), "\n")
    }
    cat("[DEBUG] [OK]Parameter validation completed\n\n")
  }

  # Species identification type detection
  detect_specimens_type <- function(specimens) {
    sample_values <- na.omit(df[[specimens]])

    if (length(sample_values) == 0) {
      stop(paste("Column '", specimens, "' contains only missing values.", sep=""))
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
      stop(paste("Data type in column '", specimens, "' is not recognized.", sep=""))
    }

    #debug
    cat("[DEBUG] ==================== SPECIMENS TYPE DETECTION ====================\n")
    cat("[DEBUG] Analyzed specimens column:", specimens, "\n")
    cat("[DEBUG] R data type:", class(df[[specimens]]), "\n")
    cat("[DEBUG] Number of non-NA values:", length(sample_values), "\n")
    cat("[DEBUG] Sample values:", paste(head(sample_values, 3), collapse = ", "), "\n")
    if (is.character(sample_values) || is.factor(sample_values)) {
      cat("[DEBUG] Average character length:", round(mean_length, 2), "\n")
    }
    cat("[DEBUG] Detected identification type:", specimens_type, "\n")
    cat("[DEBUG] [OK]Specimens type detection completed\n\n")

    return(specimens_type)
  }

  # Establish species correspondence
  establish_species_correspondence <- function(df) {
    if (is.null(specimens)) {
      stop("No species identification column specified or found in the data.")
    }

    if (!(specimens %in% colnames(df))) {
      stop(paste("The specified column '", specimens, "' does not exist in the data.", sep=""))
    }

    specimens_type <- detect_specimens_type(specimens)

    df_result <- df

    # Add Species column if necessary
    if (specimens_type != "Species" || specimens != "Species") {
      # Check required columns in equations_df
      if (!all(c("Species", specimens_type) %in% colnames(equations_df))) {
        stop(paste("The 'equations_df' dataframe must contain the columns 'Species' and '",
                   specimens_type, "'", sep=""))
      }

      # Create mapping
      mapping_df <- unique(equations_df[, c("Species", specimens_type)])
      names(mapping_df) <- c("Species", specimens)

      # Merge
      df_result <- merge(df_result, mapping_df, by = specimens, all.x = TRUE)

      # Clean duplicated columns
      if ("Species.x" %in% colnames(df_result)) {
        names(df_result)[names(df_result) == "Species.x"] <- "Species"
        if ("Species.y" %in% colnames(df_result)) {
          df_result$Species.y <- NULL
        }
      }

      # Check for missing correspondences
      if (any(is.na(df_result$Species))) {
        na_values <- unique(df_result[[specimens]][is.na(df_result$Species)])
        warning(paste("No correspondence found for the following values of",
                      specimens, ":", paste(na_values, collapse=", ")))
      }
    } else {
      names(df_result)[names(df_result) == "Species"] <- "Species"
    }

    #debug
    cat("[DEBUG] ==================== SPECIES CORRESPONDENCE ====================\n")
    cat("[DEBUG] Used specimens column:", specimens, " (type:", specimens_type, ")\n")
    cat("[DEBUG] equations_df dimensions: [", nrow(equations_df), "x", ncol(equations_df), "]\n")
    cat("[DEBUG] equations_df columns:", paste(colnames(equations_df), collapse = ", "), "\n")
    cat("[DEBUG] Number of rows before merge:", nrow(df), "\n")
    if (exists("mapping_df")) {
      cat("[DEBUG] Correspondences created:", nrow(mapping_df), "\n")
      cat("[DEBUG] Mapping preview:\n")
      print(head(mapping_df, 3))
    }
    cat("[DEBUG] Number of rows after merge:", nrow(df_result), "\n")
    values_without_correspondence <- unique(df_result[[specimens]][is.na(df_result$Species)])
    if (length(values_without_correspondence) > 0) {
      cat("[DEBUG] [Warning]  Values without correspondence (", length(values_without_correspondence), "):",
          paste(head(values_without_correspondence, 5), collapse = ", "), "\n")
    }
    cat("[DEBUG] [OK]Species correspondence completed\n\n")

    return(df_result)
  }

  # Diameter conversions management
  diameter_conversions <- function(df_result) {
    pi_val <- pi

    # Initialize target columns if they don't exist
    if (D130_exists && (!C130 %in% names(df_result))) df_result$C130 <- NA_real_
    if (D150_exists && (!C150 %in% names(df_result))) df_result$C150 <- NA_real_

    #debug
    cat("[DEBUG] ==================== DIAMETER CONVERSIONS ====================\n")
    cat("[DEBUG] Required conversions:\n")
    if (D130_exists) {
      na_before_D130 <- sum(is.na(df_result[[D130]]))
      cat("[DEBUG]   - D130 to C130: ", nrow(df_result) - na_before_D130, " values to convert\n")
      cat("[DEBUG] D130 example:", head(df_result[[D130]], 5), "\n")
    }
    if (D150_exists) {
      na_before_D150 <- sum(is.na(df_result[[D150]]))
      cat("[DEBUG]   - D150 to C150: ", nrow(df_result) - na_before_D150, " values to convert\n")
      cat("[DEBUG] D150 example:", head(df_result[[D150]], 5), "\n")
    }
    cat("[DEBUG] pi coefficient used:", round(pi, 6), "\n")

    for (i in seq_len(nrow(df_result))) {
      # ==== D130 -> C130 ====
      if (D130_exists && !is.na(df_result[[D130]][i])) {
        df_result$C130[i] <- df_result[[D130]][i] * pi_val
      }

      # ==== D150 -> C150 ====
      if (D150_exists && !is.na(df_result[[D150]][i])) {
        df_result$C150[i] <- df_result[[D150]][i] * pi_val
      }
    }

    #debug
    if (D130_exists && "C130" %in% colnames(df_result)) {
      successful_conversions_130 <- sum(!is.na(df_result$C130) & D130_exists && !is.na(df_result[[D130]]))
      cat("[DEBUG] [OK]Successful D130 to C130 conversions:", successful_conversions_130, "\n")
    }
    if (D150_exists && "C150" %in% colnames(df_result)) {
      successful_conversions_150 <- sum(!is.na(df_result$C150) & D150_exists && !is.na(df_result[[D150]]))
      cat("[DEBUG] [OK]Successful D150 to C150 conversions:", successful_conversions_150, "\n")
    }
    cat("[DEBUG] [OK]Diameter conversions completed\n\n")

    # Warning messages if some conversions fail
    if (D130_exists && sum(is.na(df_result$C130)) > 0 && sum(!is.na(df_result[[D130]])) > 0) {
      warning("Some D130 -> C130 conversions failed (missing or unprocessed values).")
    }
    if (D150_exists && sum(is.na(df_result$C150)) > 0 && sum(!is.na(df_result[[D150]])) > 0) {
      warning("Some D150 -> C150 conversions failed (missing or unprocessed values).")
    }

    #debug
    if ("C130" %in% colnames(df_result)) {
      cat("[DEBUG] C130 result examples:", head(df_result$C130, 5), "\n")
    }
    if ("C150" %in% colnames(df_result)) {
      cat("[DEBUG] C150 result examples:", head(df_result$C150, 5), "\n")
    }
    cat("Diameter to circumference conversion completed.\n")

    # Update global flags
    assign("C130_exists", "C130" %in% colnames(df_result), envir = .GlobalEnv)
    assign("C150_exists", "C150" %in% colnames(df_result), envir = .GlobalEnv)

    return(df_result)
  }

  # Generic circumference conversion
  convert_circumference <- function(df_result, C150, C130, from_col = NULL, to_col = NULL, direction = NULL) {
    # Automatic detection of conversion direction if not specified
    if (is.null(from_col) || is.null(to_col) || is.null(direction)) {
      C130_exists_local <- "C130" %in% colnames(df_result)
      C150_exists_local <- C150 %in% colnames(df_result)

      if (!C130_exists_local && C150_exists_local) {
        from_col <- C150
        to_col <- "C130"
        direction <- "C150_to_C130"
      } else if (!C150_exists_local && C130_exists_local) {
        from_col <- "C130"
        to_col <- C150
        direction <- "C130_to_C150"
      } else {
        stop("Unable to automatically determine conversion direction. Check the presence of C130 or C150.")
      }
    }

    #debug
    cat("[DEBUG] ==================== CIRCUMFERENCE CONVERSIONS ====================\n")
    cat("[DEBUG] Detected conversion direction:", direction, "\n")
    cat("[DEBUG] Source column:", from_col, "\n")
    cat("[DEBUG] Target column:", to_col, "\n")

    # Extract conversion coefficients (HV and IV)
    coefs_df <- unique(equations_df[, c("Species", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Species"] <- "Species"

    cat("[DEBUG] Available coefficients for", nrow(coefs_df), "species\n")
    cat("[DEBUG] HV/IV coefficients preview:\n")
    print(head(coefs_df[, c("Species", "HV", "IV")], 3))

    cat("Conversion coefficients preview:\n")
    print(head(coefs_df))

    # Initialize target column
    df_result[[to_col]] <- NA_real_

    # Row-by-row application
    attempted_conversions <- 0
    for (i in seq_len(nrow(df_result))) {
      tree_species <- df_result$Species[i]

      if (!is.na(tree_species)) {
        from_value <- df_result[[from_col]][i]

        if (!is.na(from_value)) {
          attempted_conversions <- attempted_conversions + 1
          coef_row <- coefs_df[coefs_df$Species == tree_species, ]

          if (i <= 5) {
            cat("Row", i, "- Species:", tree_species, "- Coefficients found:", nrow(coef_row), "\n")
            if (nrow(coef_row) > 0) {
              cat("  HV:", coef_row$HV[1], "IV:", coef_row$IV[1], "\n")
            }
          }

          if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
            HV_coef <- coef_row$HV[1]
            IV_coef <- coef_row$IV[1]

            result_value <- if (direction == "C150_to_C130") {
              HV_coef * from_value + IV_coef
            } else {
              (from_value - IV_coef) / HV_coef
            }

            df_result[[to_col]][i] <- result_value

            if (i <= 5) {
              cat("  Conversion:", from_value, "to", result_value, "\n")
            }
          } else {
            warning(paste("Unable to convert", from_col, "to", to_col, "for species:",
                          tree_species, "at row", i, ". Missing coefficients."))
          }
        }
      }
    }

    #debug
    successful_conversions <- sum(!is.na(df_result[[to_col]]))
    cat("[DEBUG] Conversions to attempt:", attempted_conversions, "\n")
    cat("[DEBUG] [OK]Successful", from_col, to, to_col, "conversions:", successful_conversions, "\n")
    if (attempted_conversions > successful_conversions) {
      cat("[DEBUG] [Warning]  Failed conversions:", attempted_conversions - successful_conversions, "\n")
    }

    # Check for missed conversions
    failed_conversions <- sum(is.na(df_result[[to_col]]))
    if (failed_conversions > 0) {
      warning(paste(failed_conversions, paste(from_col, "to", to_col),
                    "conversions failed. Check the data."))
    }

    # Row-by-row calculation of D130
    if (!D130_exists && "C130" %in% colnames(df_result)) {
      df_result$D130 <- NA_real_
      for (i in seq_len(nrow(df_result))) {
        if (!is.na(df_result$C130[i])) {
          df_result$D130[i] <- df_result$C130[i] / pi
          if (i <= 5) cat("D130 row", i, ":", df_result$D130[i], "\n")
        }
      }
    }

    # Row-by-row calculation of D150
    if (!D150_exists && C150 %in% colnames(df_result)) {
      df_result$D150 <- NA_real_
      for (i in seq_len(nrow(df_result))) {
        if (!is.na(df_result[[C150]][i])) {
          df_result$D150[i] <- df_result[[C150]][i] / pi
          if (i <= 5) cat("D150 row", i, ":", df_result$D150[i], "\n")
        }
      }
    }

    #debug
    if ("D130" %in% colnames(df_result) && !"D130" %in% colnames(df)) {
      cat("[DEBUG] [OK]D130 column created from C130\n")
    }
    if ("D150" %in% colnames(df_result) && !"D150" %in% colnames(df)) {
      cat("[DEBUG] [OK]D150 column created from C150\n")
    }
    cat("[DEBUG] [OK]Circumference conversions completed\n\n")

    return(df_result)
  }

  # Calculate basal areas
  calculate_basal_areas <- function(df_result) {
    #debug
    cat("[DEBUG] ==================== BASAL AREA CALCULATION ====================\n")
    calculated_areas <- c()

    # Calculate G130 if necessary
    if (!"G130" %in% colnames(df_result) && "C130" %in% colnames(df_result)) {
      df_result$G130 <- (df_result$C130^2) / ((4 * pi) * 10000)
      g130_values <- sum(!is.na(df_result$G130))
      cat("[DEBUG] [OK]G130 calculated for", g130_values, "trees\n")
      cat("[DEBUG] G130 examples:", paste(round(head(df_result$G130[!is.na(df_result$G130)], 3), 6), collapse = ", "), "\n")
      calculated_areas <- c(calculated_areas, "G130")
    }

    # Calculate G150 if necessary
    C150_exists_local <- C150 %in% colnames(df_result)
    if (!"G150" %in% colnames(df_result) && C150_exists_local) {
      df_result$G150 <- (df_result[[C150]]^2) / ((4 * pi) * 10000)
      g150_values <- sum(!is.na(df_result$G150))
      cat("[DEBUG] [OK]G150 calculated for", g150_values, "trees\n")
      cat("[DEBUG] G150 examples:", paste(round(head(df_result$G150[!is.na(df_result$G150)], 3), 6), collapse = ", "), "\n")
      calculated_areas <- c(calculated_areas, "G150")
    }

    if (length(calculated_areas) == 0) {
      cat("[DEBUG] [INFO]  No basal area to calculate (already present)\n")
    } else {
      cat("[DEBUG] Calculated basal areas:", paste(calculated_areas, collapse = ", "), "\n")
    }
    cat("[DEBUG] Formula used: G = C²/(4pi*10000)\n")
    cat("[DEBUG] [OK]Basal area calculation completed\n\n")

    return(df_result)
  }

  # =========================================================================
  # MAIN EXECUTION
  # =========================================================================

  # 1. Parameter validation
  validate_parameters()

  # 2. Initialization
  equations_df <- equations
  cat("Selected volume type:", volume_type, "\n")

  # 3. Establish species correspondences
  df_result <- establish_species_correspondence(df)

  # 4. Handle diameter conversions
  df_result <- diameter_conversions(df_result)

  # 5. Convert circumferences if necessary
  if ((!C130_exists && C150_exists) || (!C150_exists && C130_exists)) {
    df_result <- convert_circumference(df_result, C150, C130)
  }

  # 6. Calculate basal areas
  df_result <- calculate_basal_areas(df_result)

  cat("[DEBUG] Beginning calculate_volumes function\n")
  cat("[DEBUG] Number of rows in input dataframe:", nrow(df), "\n")
  cat("[DEBUG] Columns in input dataframe:", paste(colnames(df), collapse = ", "), "\n")

  # =========================================================================
  # VOLUME CALCULATION
  # =========================================================================

  # Filter equations
  eqs_volume <- equations_df[equations_df$Y == volume_type, ]
  # DEBUG: Display found equations
  cat("Number of equations found for", volume_type, ":", nrow(eqs_volume), "\n")
  cat("Preview of available equations:\n")
  print(head(eqs_volume[, c("Species", "Y", "A0")]))

  if (nrow(eqs_volume) == 0) {
    stop(paste("No equation found for volume type:", volume_type))
  }

  # Ensure that coefficients b0 to b5 are numeric
  b_columns <- paste0("b", 0:5)
  eqs_volume[b_columns] <- lapply(eqs_volume[b_columns], as.numeric)

  # Initialization
  df_result$Equation_Used <- NA_character_

  # Initialize the volume column specified by the user
  if (!(volume_type %in% names(df_result))) {
    df_result[[volume_type]] <- NA_real_
  }

  # Modified evaluate_expression function with more debugging
  evaluate_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)

    # Check if the expression is syntactically valid
    if (expr_text == "/") {
      warning("Invalid expression detected: '/'")
      return(NA)
    }

    # Check that all necessary variables are present and not NA
    var_names <- all.vars(parse(text = expr_text))
    for (v in var_names) {
      if (!v %in% names(variables) || is.na(variables[[v]])) {
        warning(paste("Missing or NA variable:", v, "for expression:", expr_text))
        return(NA)
      }
    }

    env <- list2env(variables)
    tryCatch({
      eval(parse(text = expr_text), envir = env)
    }, error = function(e) {
      warning(paste("Error during expression evaluation:", expr_text, "-", e$message))
      return(NA)
    })
  }

  # Calculation for each row
  for (i in seq_len(nrow(df_result))) {
    tree_species <- df_result$Species[i]  # Using Species instead of Essence

    # Initialize volume variable for this row
    volume <- 0

    # For volume type "E", dynamically determine the equation to use
    # according to species, always equation 4 or 5 specific to each species
    if (volume_type == "E") {
      # Find all available equations for this species and volume type
      eq_candidates_E <- eqs_volume[eqs_volume$Species == tree_species, ]

      # Filter only equations with A0 = 4 or A0 = 5
      eq_candidates_E_filtered <- eq_candidates_E[eq_candidates_E$A0 %in% c(4, 5), ]

      if (nrow(eq_candidates_E_filtered) > 0) {
        # Determine which equation (4 or 5) is associated with this species
        if (any(eq_candidates_E_filtered$A0 == 4)) {
          local_equation_id <- 4
          cat(" Using equation A0 = 4 for", tree_species, "\n")
        } else {
          local_equation_id <- 5
          cat(" Using equation A0 = 5 for", tree_species, "\n")
        }
      } else {
        # If no equation 4 or 5 is found for this species, keep the original equation_id
        cat(" No equation of type 4 or 5 found for", tree_species, ". Using default equation.\n")
      }
    } else {
      # For other volume types, keep the equation specified by the user
      local_equation_id <- equation_id
    }

    eq_candidates <- eqs_volume[eqs_volume$Species == tree_species, ]

    # DEBUG: Display candidate equations for this species
    if (i <= 5) {
      cat("  Number of candidate equations:", nrow(eq_candidates), "\n")
    }

    if (nrow(eq_candidates) == 0) {
      # If no specific equation is found, warning.
      warning(paste("No equation found for species:", tree_species))
      next
    }

    # For type "E", use the equation with A0 corresponding to local_equation_id
    if (volume_type == "E" && local_equation_id %in% c(4, 5)) {
      eq_by_a0 <- eq_candidates[eq_candidates$A0 == local_equation_id, ]
      if (nrow(eq_by_a0) > 0) {
        eq <- eq_by_a0[1, , drop = FALSE]  # Take the first equation if several match
        cat("  Using equation A0 =", local_equation_id, "for", tree_species, "\n")
      } else {
        # If no equation with the specific A0 is found
        warning(paste("No equation with A0 =", local_equation_id, "found for species", tree_species))

        # Try with the other equation type (4 or 5)
        other_a0 <- if (local_equation_id == 4) 5 else 4
        eq_by_other_a0 <- eq_candidates[eq_candidates$A0 == other_a0, ]

        if (nrow(eq_by_other_a0) > 0) {
          eq <- eq_by_other_a0[1, , drop = FALSE]
          cat("  Using alternative equation A0 =", other_a0, "for", tree_species, "\n")
        } else {
          # If still no equation, use the first available
          if (nrow(eq_candidates) > 0) {
            eq <- eq_candidates[1, , drop = FALSE]
            cat("  Using default equation for", tree_species, "\n")
          } else {
            warning(paste("No equation found for species", tree_species))
            next
          }
        }
      }
    } else {
      # For other volume types, standard behavior
      if (local_equation_id > nrow(eq_candidates)) {
        warning(paste("Equation with id", local_equation_id, "does not exist for species", tree_species,
                      ". Using equation 1 instead."))
        eq <- eq_candidates[1, , drop = FALSE]
      } else {
        eq <- eq_candidates[local_equation_id, , drop = FALSE]
      }
    }

    df_result$Equation_Used[i] <- paste0(eq$Species, ":", eq$Y, ":A0=", eq$A0)

    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(df_result)) {
        variables[[v]] <- df_result[[v]][i]
      } else {
        stop(paste("Variable", v, "is used in an equation but missing from the data."))
      }
    }

    a0_value <- eq$A0[1]

    # DEBUG: Display equation details
    if (is.na(a0_value)) {
      warning(paste("Missing A0 value for species", tree_species, "at row", i))
      next
    } else if (a0_value %in% c(1, 2, 3, 5)) {
      # For standard linear equations, start with b0
      volume <- eq$b0[1]
      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)

        # Check if the X expression exists and is valid
        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          if (eq[[x_col]][1] == "/") {
            warning(paste("Invalid expression at row", i, "for X", j))
            next
          }

          # Explicitly capture the returned value
          x_val <- tryCatch({
            evaluate_expression(eq[[x_col]][1], variables)
          }, error = function(e) {
            warning(paste("Error during evaluation of X", j, ":", e$message))
            NA
          })

          # Safe check for NA
          if (length(x_val) == 0 || is.na(x_val)) {
            warning(paste("Evaluation of X", j, "failed at row", i))
            next
          }

          b_val <- eq[[b_col]][1]
          if (is.na(b_val)) {
            warning(paste("Missing coefficient b", j, "at row", i))
            next
          }

          if (i <= 5) {
            cat("  X", j, "=", x_val, "b", j, "=", b_val, "\n")
          }
          volume <- volume + b_val * x_val
        }
      }
    } else if (a0_value == 4) {
      C130 <- evaluate_expression(eq$X1[1], variables)
      if (C130 <= 0) {
        warning(paste("Negative or zero value for logarithm at row", i))
        next
      }
      volume <- 10^(1*eq$b0[1] + eq$b1[1] * log10(C130))
    } else {
      warning(paste("Unknown equation type (A0 =", a0_value, ") for row", i))
      next
    }

    if (is.na(volume) || !is.finite(volume)) {
      warning(paste("Invalid volume result at row", i, ":", volume))
      next
    }

    # DEBUG: Display calculated volume
    if (i <= 5) {
      cat("  Calculated volume:", volume, "\n")
    }

    # Store volume only for this row, in the column specified by volume_type
    df_result[[volume_type]][i] <- volume

    if (i <= 5) {
      cat("  Volume stored at row", i, ":", df_result[[volume_type]][i], "\n")
    }
  }

  # Final cleanup if requested
  if (remove_na) {
    df_result <- df_result[!is.na(df_result[[volume_type]]), ]
  }

return(df_result)
}
