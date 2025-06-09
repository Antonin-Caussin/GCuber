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
#'   \item \strong{Basal area calculation}: Computes G = C^2/(4pi*10000)
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
#'   \item \strong{Abr}: Character abbreviations (<=4 characters)
#'   \item \strong{Species}: Full species names (>4 characters)
#' }
#'
#' \strong{Equation Types (A0 parameter):}
#' \itemize{
#'   \item \strong{A0 = 1, 2, 3, 5}: Linear equations of form: Volume = b0 + sum(bi * Xi)
#'   \item \strong{A0 = 4}: Logarithmic equations: Volume = 10^(b0 + b1 * log(C130))
#' }
#'
#' \strong{Internal Functions:}
#' The function contains several internal helper functions:
#' \itemize{
#'   \item \code{update_flags}: Updates column existence flags for dynamic processing
#'   \item \code{validate_parameters}: Validates input parameters and data structure
#'   \item \code{detect_specimens_type}: Automatically detects species identification type
#'   \item \code{establish_species_correspondence}: Creates species mapping from codes/abbreviations
#'   \item \code{diameter_conversions}: Converts diameters to circumferences using pi
#'   \item \code{convert_circumference}: Generic conversion between C130 and C150 using HV/IV coefficients
#'   \item \code{calculate_basal_areas}: Computes basal areas from circumferences
#'   \item \code{evaluate_expression}: Safely evaluates mathematical expressions in equations
#' }
#'
#' \strong{Data Processing Flow:}
#' \enumerate{
#'   \item Parameter validation and column existence checking
#'   \item Species correspondence establishment using equations data
#'   \item Diameter to circumference conversions (D * pi = C)
#'   \item Circumference conversions between heights using species-specific coefficients
#'   \item Basal area calculations using formula G = C^2/(4*pi*10000)
#'   \item Volume calculations using species-specific allometric equations
#'   \item Optional removal of rows with failed calculations
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
#' @section Conversion Coefficients:
#' The function uses species-specific coefficients for circumference conversions:
#' \itemize{
#'   \item \strong{HV}: Height variation coefficient
#'   \item \strong{IV}: Intercept variation coefficient
#'   \item \strong{C150 to C130}: C130 = HV * C150 + IV
#'   \item \strong{C130 to C150}: C150 = (C130 - IV) / HV
#' }
#'
#' @section Error Handling:
#' The function implements comprehensive error handling:
#' \itemize{
#'   \item \strong{Input validation}: Checks parameter types and valid ranges
#'   \item \strong{Data validation}: Verifies required columns exist
#'   \item \strong{Species validation}: Warns about missing species correspondences
#'   \item \strong{Mathematical validation}: Handles logarithms of negative values
#'   \item \strong{Expression validation}: Safely evaluates mathematical expressions
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
#'   \item Missing coefficients for species-specific conversions
#' }
#'
#' @section Performance Considerations:
#' \itemize{
#'   \item Row-by-row processing for precise control over calculations
#'   \item Extensive debugging output (can be disabled for production use)
#'   \item Memory efficient handling of large datasets
#'   \item Automatic detection of required conversions to minimize processing
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
#'
#' # Using species abbreviations
#' data_abr <- data.frame(
#'   Abr = c("PIAB", "FASY", "QURO"),
#'   C130 = c(94.2, 125.6, 157.1),
#'   HTOT = c(25.5, 28.2, 22.8)
#' )
#'
#' result_abr <- calculate_volumes(data_abr,
#'                               volume_type = "V22",
#'                               specimens = "Abr")
#'
#' # Mixed diameter and circumference data
#' data_mixed <- data.frame(
#'   Species = c("Picea abies", "Fagus sylvatica"),
#'   D130 = c(30, NA),
#'   C150 = c(NA, 125.6),
#'   HTOT = c(25.5, 28.2)
#' )
#'
#' result_mixed <- calculate_volumes(data_mixed, volume_type = "V22")
#' }
#'
#' @note
#' The function displays information messages during execution to facilitate debugging.
#' Warnings are issued if species correspondences are not found or if
#' volume calculation fails for certain rows. For production use, consider
#' suppressing debug messages by modifying the cat() statements.
#'
#' @seealso
#' Related functions for forest management and dendrometric calculations.
#'
#' @author Caussin Antonin
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

calculate_volumes <- function(df, carbon = FALSE,
                              volume_type = "V22",
                              equation_id = 1,
                              source = "Dagnellie",
                              specimens = NULL,
                              C130 = "C130",
                              C150 = "C150",
                              D130 = "D130",
                              D150 = "D150",
                              HTOT = "HTOT",
                              HDOM = "HDOM",
                              bark = FALSE,
                              remove_na = FALSE) {

  # =========================================================================
  # INTERNAL FUNCTIONS
  # =========================================================================
  df_result <- df

  flags <- list(
    C130_exists = FALSE,
    C150_exists = FALSE,
    D130_exists = FALSE,
    D150_exists = FALSE,
    HTOT_exists = FALSE,
    HDOM_exists = FALSE
  )

  update_flags <- function(df_result, flags, C130, C150, D130, D150, HTOT, HDOM) {
    flags$C130_exists <- C130 %in% colnames(df_result)
    flags$C150_exists <- C150 %in% colnames(df_result)
    flags$D130_exists <- D130 %in% colnames(df_result)
    flags$D150_exists <- D150 %in% colnames(df_result)
    flags$HTOT_exists <- HTOT %in% colnames(df_result)
    flags$HDOM_exists <- HDOM %in% colnames(df_result)
    return(flags)
  }

  # Input parameter validation

  # Check existence of columns in user dataframe
  required_columns <- c(C130, C150, D130, D150, HTOT, HDOM, specimens)
  present_columns <- required_columns[required_columns %in% colnames(df_result)]
  missing_columns <- setdiff(required_columns, present_columns)

  validate_parameters <- function() {

    # Source validation
    valid_sources <- c("Aflan", "Dagnellie", "Valet")
    if (!(source %in% valid_sources)) {
      stop(paste("Invalid source:", source,
                 "\nValid sources:", paste(valid_sources, collapse = ", ")))
    }

    # Volume type verification
    valid_volume_types <- c("V22", "V22B", "V22_HA")
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

    # Assignment of global flags
    flags <- update_flags(df_result, flags, C130, C150, D130, D150, HTOT, HDOM)

    # Check circumference / diameter pairs
    if (!flags$C130_exists && !flags$C150_exists && !flags$D130_exists && !flags$D150_exists) {
      stop("No circumference column (C130 or C150) or diameter column (D130 or D150) found in the data.")
    }

    if ((!flags$C130_exists && !flags$C150_exists) && (flags$D130_exists || flags$D150_exists)) {
      message("No circumference (C130 or C150) is available, but diameters are present.")
    }

    if ((flags$C130_exists || flags$C150_exists) && (!flags$D130_exists && !flags$D150_exists)) {
      message("Circumferences are present, but no diameter (D130 or D150) was found.")
    }

    if (length(missing_columns) > 0) {
      warning(paste("Missing columns in data:", paste(missing_columns, collapse = ", ")))
    }
  }
    #debug
    cat("[DEBUG] ==================== PARAMETER VALIDATION ====================\n")
    cat("[DEBUG] Received parameters:\n")
    cat("[DEBUG]   - volume_type =", volume_type, "\n")
    cat("[DEBUG]   - equation_id =", equation_id, "\n")
    cat("[DEBUG]   - specimens =", ifelse(is.null(specimens), "NULL", specimens), "\n")
    cat("[DEBUG]   - remove_na =", remove_na, "\n")
    cat("[DEBUG] Input dataframe dimensions: [", nrow(df_result), "x", ncol(df_result), "]\n")
    cat("[DEBUG] Available columns:", paste(colnames(df_result), collapse = ", "), "\n")
    cat("[DEBUG] Required columns:", paste(required_columns, collapse = ", "), "\n")
    cat("[DEBUG] Column flags defined:\n")
    cat("[DEBUG]   - flags$C130_exists =", flags$C130_exists, "\n")
    cat("[DEBUG]   - flags$C150_exists =", flags$C150_exists, "\n")
    cat("[DEBUG]   - flags$D130_exists =", flags$D130_exists, "\n")
    cat("[DEBUG]   - flags$D150_exists =", flags$D150_exists, "\n")
    cat("[DEBUG]   - flags$HTOT_exists =", flags$HTOT_exists, "\n")
    cat("[DEBUG]   - flags$HDOM_exists =", flags$HDOM_exists, "\n")
    cat("[DEBUG] Checking the conditions for C130 <-> C150 conversion \n")
    cat("flags$C130_exists =", flags$C130_exists, "\n")
    cat("flags$C150_exists =", flags$C150_exists, "\n")
    if (length(missing_columns) > 0) {
      cat("[DEBUG] [Warning]  Missing columns:", paste(missing_columns, collapse = ", "), "\n")
    }
    cat("[DEBUG] [OK]Parameter validation completed\n\n")

  # Species identification type detection
  detect_specimens_type <- function(specimens) {
    sample_values <- na.omit(df_result[[specimens]])

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

    flags <- update_flags(df_result, flags, C130, C150, D130, D150, HTOT, HDOM)

    #debug
    cat("[DEBUG] ==================== SPECIMENS TYPE DETECTION ====================\n")
    cat("[DEBUG] Analyzed specimens column:", specimens, "\n")
    cat("[DEBUG] R data type:", class(df_result[[specimens]]), "\n")
    cat("[DEBUG] Number of non-NA values:", length(sample_values), "\n")
    cat("[DEBUG] Sample values:", paste(utils::head(sample_values, 3), collapse = ", "), "\n")
    if (is.character(sample_values) || is.factor(sample_values)) {
      cat("[DEBUG] Average character length:", round(mean_length, 2), "\n")
    }
    cat("[DEBUG] Detected identification type:", specimens_type, "\n")
    cat("[DEBUG] [OK]Specimens type detection completed\n\n")
    return(specimens_type)
  }

  # Establish species correspondence
  establish_species_correspondence <- function(df_result) {
    if (is.null(specimens)) {
      stop("No species identification column specified or found in the data.")
    }

    if (!(specimens %in% colnames(df_result))) {
      stop(paste("The specified column '", specimens, "' does not exist in the data.", sep=""))
    }

    specimens_type <- detect_specimens_type(specimens)

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

      conflicts <- intersect(names(df_result), names(mapping_df))
      conflicts <- setdiff(conflicts, specimens)
      if (length(conflicts) > 0) {
        df_result <- df_result[, !(names(df_result) %in% conflicts)]
      }
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
      print(utils::head(mapping_df, 3))
    }
    cat("[DEBUG] Number of rows after merge:", nrow(df_result), "\n")
    values_without_correspondence <- unique(df_result[[specimens]][is.na(df_result$Species)])
    if (length(values_without_correspondence) > 0) {
      cat("[DEBUG] [Warning]  Values without correspondence (", length(values_without_correspondence), "):",
          paste(utils::head(values_without_correspondence, 5), collapse = ", "), "\n")
    }
    cat("[DEBUG] [OK]Species correspondence completed\n\n")

    flags <- update_flags(df_result, flags, C130, C150, D130, D150, HTOT, HDOM)

    return(df_result)
  }

  # Diameter conversions management
  diameter_conversions <- function(df_result) {
    pi_val <- pi

    # Initialize target columns if they don't exist
    cat("Type de flags$D130_exists :", typeof(flags$D130_exists), "\n")
    cat("Valeur de flags$D130_exists :", flags$D130_exists, "\n")

    if (flags$D130_exists && !(C130 %in% colnames(df_result))) {
      df_result[[C130]] <- NA_real_  # Utiliser la variable utilisateur
    }
    if (flags$D150_exists && !(C150 %in% colnames(df_result))) {
      df_result[[C150]] <- NA_real_  # Utiliser la variable utilisateur
    }

    # Dans la boucle de conversion D -> C, aussi corriger :
    for (i in seq_len(nrow(df_result))) {
      # ==== D130 -> C130 ====
      if (flags$D130_exists && !is.na(df_result[[D130]][i])) {
        df_result[[C130]][i] <- df_result[[D130]][i] * pi_val  # Utiliser variable utilisateur
      }

      # ==== D150 -> C150 ====
      if (flags$D150_exists && !is.na(df_result[[D150]][i])) {
        df_result[[C150]][i] <- df_result[[D150]][i] * pi_val  # Utiliser variable utilisateur
      }
    }

    flags <- update_flags(df_result, flags, C130, C150, D130, D150, HTOT, HDOM)



    #debug
    cat("[DEBUG] ==================== DIAMETER CONVERSIONS ====================\n")
    cat("[DEBUG] Required conversions:\n")
    if (flags$D130_exists) {
      na_before_D130 <- sum(is.na(df_result[[D130]]))
      cat("[DEBUG]   - D130 to C130: ", nrow(df_result) - na_before_D130, " values to convert\n")
      cat("[DEBUG] D130 example:", utils::head(df_result[[D130]], 5), "\n")
    }
    if (flags$D150_exists) {
      na_before_D150 <- sum(is.na(df_result[[D150]]))
      cat("[DEBUG]   - D150 to C150: ", nrow(df_result) - na_before_D150, " values to convert\n")
      cat("[DEBUG] D150 example:", utils::head(df_result[[D150]], 5), "\n")
    }
    cat("[DEBUG] pi coefficient used:", round(pi, 6), "\n")

    for (i in seq_len(nrow(df_result))) {
      # ==== D130 -> C130 ====
      if (flags$D130_exists && !is.na(df_result[[D130]][i])) {
        df_result$C130[i] <- df_result[[D130]][i] * pi_val
      }

      # ==== D150 -> C150 ====
      if (flags$D150_exists && !is.na(df_result[[D150]][i])) {
        df_result$C150[i] <- df_result[[D150]][i] * pi_val
      }
    }

    #debug
    if (flags$D130_exists && "C130" %in% colnames(df_result)) {
      successful_conversions_130 <- sum(!is.na(df_result$C130) & flags$D130_exists & !is.na(df_result[[D130]]))
      cat("[DEBUG] [OK]Successful D130 to C130 conversions:", successful_conversions_130, "\n")
    }
    if (flags$D150_exists && "C150" %in% colnames(df_result)) {
      successful_conversions_150 <- sum(!is.na(df_result$C150) & flags$D150_exists & !is.na(df_result[[D150]]))
      cat("[DEBUG] [OK]Successful D150 to C150 conversions:", successful_conversions_150, "\n")
    }
    cat("[DEBUG] [OK]Diameter conversions completed\n\n")

    # Warning messages if some conversions fail
    if (flags$D130_exists && sum(is.na(df_result$C130)) > 0 && sum(!is.na(df_result[[D130]])) > 0) {
      warning("Some D130 -> C130 conversions failed (missing or unprocessed values).")
    }
    if (flags$D150_exists && sum(is.na(df_result$C150)) > 0 && sum(!is.na(df_result[[D150]])) > 0) {
      warning("Some D150 -> C150 conversions failed (missing or unprocessed values).")
    }

    #debug
    if (C130 %in% colnames(df_result)) {
      cat("[DEBUG] C130 result examples:", utils::head(df_result$C130, 5), "\n")
    }
    if (C150 %in% colnames(df_result)) {
      cat("[DEBUG] C150 result examples:", utils::head(df_result$C150, 5), "\n")
    }
    cat("Diameter to circumference conversion completed.\n")

    # Update global flags

    flags <- update_flags(df_result, flags, C130, C150, D130, D150, HTOT, HDOM)


    return(df_result)
  }

  # Generic circumference conversion
  convert_circumference <- function(df_result, from_col = NULL, to_col = NULL, direction = NULL) {
    cat("[DEBUG] ==================== GENERIC CIRCUMFERENCE CONVERSION ====================\n")

    # Mettre à jour les flags locaux
    flags <- update_flags(df_result, flags, C130, C150, D130, D150, HTOT, HDOM)
    C130_exists_local <- flags$C130_exists
    C150_exists_local <- flags$C150_exists

    cat("[DEBUG] C130_exists_local =", C130_exists_local, "\n")
    cat("[DEBUG] C150_exists_local =", C150_exists_local, "\n")

    # Détection automatique de la direction de conversion si non spécifiée
    if (is.null(from_col) || is.null(to_col) || is.null(direction)) {
      if (!C130_exists_local && C150_exists_local) {
        from_col <- C150    # Variable utilisateur pour C150
        to_col <- C130      # Variable utilisateur pour C130
        direction <- "C150_to_C130"
        cat("[DEBUG] Auto-detected:", C150, "to", C130, "conversion\n")
      } else if (!C150_exists_local && C130_exists_local) {
        from_col <- C130    # Variable utilisateur pour C130
        to_col <- C150      # Variable utilisateur pour C150
        direction <- "C130_to_C150"
        cat("[DEBUG] Auto-detected:", C130, "to", C150, "conversion\n")
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
      print(utils::head(coefs_df[, c("Species", "HV", "IV")], 3))

      cat("Conversion coefficients preview:\n")
      print(utils::head(coefs_df))

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
      cat("[DEBUG] [OK]Successful", from_col, "to", to_col, "conversions:", successful_conversions, "\n")
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
      if (!flags$D130_exists && C130 %in% colnames(df_result)) {
        df_result[[D130]] <- NA_real_  # Utiliser variable utilisateur
        for (i in seq_len(nrow(df_result))) {
          if (!is.na(df_result[[C130]][i])) {
            df_result[[D130]][i] <- df_result[[C130]][i] / pi
          }
        }
      }

      # Row-by-row calculation of D150
      if (!flags$D150_exists && C150 %in% colnames(df_result)) {
        df_result[[D150]] <- NA_real_  # Utiliser variable utilisateur
        for (i in seq_len(nrow(df_result))) {
          if (!is.na(df_result[[C150]][i])) {
            df_result[[D150]][i] <- df_result[[C150]][i] / pi
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
    if (!"G130" %in% colnames(df_result) && C130 %in% colnames(df_result)) {
      df_result$G130 <- (df_result[[C130]]^2) / ((4 * pi) * 10000)
      g130_values <- sum(!is.na(df_result$G130))
      cat("[DEBUG] [OK]G130 calculated for", g130_values, "trees\n")
      cat("[DEBUG] G130 examples:", paste(round(utils::head(df_result$G130[!is.na(df_result$G130)], 3), 6), collapse = ", "), "\n")
      calculated_areas <- c(calculated_areas, "G130")
    }

    # Calculate G150 if necessary
    if (!"G150" %in% colnames(df_result) && C150 %in% colnames(df_result)) {
      df_result$G150 <- (df_result[[C150]]^2) / ((4 * pi) * 10000)
      g150_values <- sum(!is.na(df_result$G150))
      cat("[DEBUG] [OK]G150 calculated for", g150_values, "trees\n")
      cat("[DEBUG] G150 examples:", paste(round(utils::head(df_result$G150[!is.na(df_result$G150)], 3), 6), collapse = ", "), "\n")
      calculated_areas <- c(calculated_areas, "G150")
    }

    if (length(calculated_areas) == 0) {
      cat("[DEBUG] [INFO]  No basal area to calculate (already present)\n")
    } else {
      cat("[DEBUG] Calculated basal areas:", paste(calculated_areas, collapse = ", "), "\n")
    }
    cat("[DEBUG] Formula used: G = C^2/(4pi*10000)\n")
    cat("[DEBUG] [OK]Basal area calculation completed\n\n")

    return(df_result)
  }


  calculate_bark_thickness <- function(df_result, total_volume_col) {
    cat("[DEBUG] ==================== BARK THICKNESS CALCULATION ====================\n")

    # Get bark thickness equations (type E)
    bark_eqs <- equations_df[equations_df$Y == "E", ]

    if (nrow(bark_eqs) == 0) {
      warning("No bark thickness equations found (type E)")
      return(df_result)
    }

    # Initialize thickness column
    df_result$E <- NA_real_
    df_result$Bark_Volume <- NA_real_
    df_result$Wood_Volume <- NA_real_

    for (i in seq_len(nrow(df_result))) {
      tree_species <- df_result$Species[i]
      total_vol <- df_result[[total_volume_col]][i]

      if (is.na(tree_species) || is.na(total_vol)) next

      # Find the bark equation for this species
      eq_bark <- bark_eqs[bark_eqs$Species == tree_species, ]

      if (nrow(eq_bark) == 0) {
        warning(paste("No bark thickness equation found for species:", tree_species))
        next
      }

      # Choose the appropriate equation (A0 = 4 or 5)
      if (any(eq_bark$A0 == 4)) {
        eq <- eq_bark[eq_bark$A0 == 4, ][1, ]
      } else if (any(eq_bark$A0 == 5)) {
        eq <- eq_bark[eq_bark$A0 == 5, ][1, ]
      } else {
        next
      }

      # Calculate bark thickness
      variables <- list()
      for (v in c("C130", "C150", "D130", "D150", "HTOT", "HDOM", "G130", "G150")) {
        if (v %in% names(df_result)) {
          variables[[v]] <- df_result[[v]][i]
        }
      }

      if (eq$A0[1] == 4) {
        C130_val <- variables[["C130"]]
        if (!is.null(C130_val) && C130_val > 0) {
          bark_thickness <- 10^(eq$b0[1] + eq$b1[1] * log10(C130_val))
        } else {
          next
        }
      } else {
        # For A0 = 5, use existing linear logic
        bark_thickness <- eq$b0[1]
        for (j in 1:5) {
          x_col <- paste0("X", j)
          b_col <- paste0("b", j)

          if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
            x_val <- evaluate_expression(eq[[x_col]][1], variables)
            if (!is.na(x_val)) {
              bark_thickness <- bark_thickness + eq[[b_col]][1] * x_val
            }
          }
        }
      }

      # Calculate bark volume (conical approximation)
      # Bark volume = Total volume * (external_radius^3 - internal_radius^3) / external_radius^3
      if (!is.na(bark_thickness) && bark_thickness > 0) {
        radius_external <- df_result$D130[i] / 2 # in cm
        radius_internal <- radius_external - bark_thickness # in cm

        if (radius_internal > 0) {
          bark_volume_ratio <- (radius_external^3 - radius_internal^3) / radius_external^3
          bark_volume <- total_vol * bark_volume_ratio
          wood_volume <- total_vol - bark_volume

          df_result$E[i] <- bark_thickness
          df_result$Bark_Volume[i] <- bark_volume
          df_result$Wood_Volume[i] <- wood_volume
        }
      }
    }

    cat("[DEBUG] [OK] Bark thickness calculation completed\n\n")
    return(df_result)
  }

  # =========================================================================
  # MAIN EXECUTION
  # =========================================================================

  # 1. Parameter validation
  validate_parameters()

  # 2. Initialization
  equations_df <- equations
  # Filter equations by source
  if ("Source" %in% colnames(equations_df)) {
    equations_df <- equations_df[equations_df$Source == source, ]
    if (nrow(equations_df) == 0) {
      stop(paste("No equations found for source:", source))
    }
    cat("Filtered equations by source '", source, "': ", nrow(equations_df), " equations available\n")
  } else {
    warning("Column 'Source' not found in equations_df. Source filtering will be ignored.")
  }

  cat("Selected volume type:", volume_type, "\n")
  cat("Selected source:", source, "\n")

  # 3. Establish species correspondences
  df_result <- establish_species_correspondence(df)

  # 4. Handle diameter conversions
  df_result <- diameter_conversions(df_result)

  # 5. Convert circumferences if necessary
  flags <- update_flags(df_result, flags, C130, C150, D130, D150, HTOT, HDOM)

  if (!flags$C130_exists && flags$C150_exists) {
    cat("[DEBUG] Conversion needed: C150 to C130\n")
    df_result <- convert_circumference(df_result)
  } else if (!flags$C150_exists && flags$C130_exists) {
    cat("[DEBUG] Conversion needed: C130 to C150\n")
    df_result <- convert_circumference(df_result)
  } else if (flags$C130_exists && flags$C150_exists) {
    cat("[DEBUG] Both C130 and C150 exist, no conversion needed\n")
  } else {
    cat("[DEBUG] Neither C130 nor C150 exist after diameter conversions\n")
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
  print(utils::head(eqs_volume[, c("Species", "Y", "A0")]))

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
    local_equation_id <- equation_id
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
    } else if (a0_value %in% c(1, 2, 3)) {
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

  # Calculate biomass with multiple equations per species and corresponding carbon
  calculate_biomass <- function(df_result) {
    cat("[DEBUG] ==================== BIOMASS CALCULATION ====================\n")

    cat("[DEBUG] Available equations in equations_df:", nrow(equations_df), "\n")
    cat("[DEBUG] Unique species in equations_df:", length(unique(equations_df$Species)), "\n")

    calculated_biomass <- 0
    species_without_equations <- c()

    # Filter biomass equations (assuming biomass type is "BIOMASS" or similar)
    eqs_biomass <- equations_df[equations_df$Y == "BIOMASS", ]  # Ajustez selon votre nomenclature

    cat("[DEBUG] Number of biomass equations found:", nrow(eqs_biomass), "\n")

    if (nrow(eqs_biomass) == 0) {
      warning("No biomass equations found in equations_df")
      return(df_result)
    }

    # Ensure that coefficients b0 to b5 are numeric
    b_columns <- paste0("b", 0:5)
    eqs_biomass[b_columns] <- lapply(eqs_biomass[b_columns], as.numeric)

    # Get unique species with biomass equations
    species_with_eqs <- unique(eqs_biomass$Species)

    # Create a mapping of equations per species
    eq_mapping <- list()
    biomass_columns <- c()
    carbon_columns <- c()
    equation_columns <- c()

    for (species in species_with_eqs) {
      species_eqs <- eqs_biomass[eqs_biomass$Species == species, ]
      eq_mapping[[species]] <- species_eqs

      # Create column names for each equation of this species
      for (i in 1:nrow(species_eqs)) {
        eq_info <- species_eqs[i, ]
        # Create unique column name: Species_BiomassEq_A0value_index
        biomass_col_name <- paste0(species, "_Biomass_A0", eq_info$A0, "_Eq", i)
        carbon_col_name <- paste0(species, "_Carbon_A0", eq_info$A0, "_Eq", i)
        eq_col_name <- paste0(biomass_col_name, "_Equation")

        biomass_columns <- c(biomass_columns, biomass_col_name)
        carbon_columns <- c(carbon_columns, carbon_col_name)
        equation_columns <- c(equation_columns, eq_col_name)

        # Initialize columns in df_result
        df_result[[biomass_col_name]] <- NA_real_
        df_result[[carbon_col_name]] <- NA_real_
        df_result[[eq_col_name]] <- NA_character_
      }
    }

    # Add total biomass and carbon columns
    df_result$Biomass_Total <- NA_real_
    df_result$Carbon_Total <- NA_real_

    cat("[DEBUG] Created", length(biomass_columns), "biomass columns and", length(carbon_columns), "carbon columns for", length(species_with_eqs), "species\n")
    cat("[DEBUG] Biomass columns:", paste(head(biomass_columns, 5), collapse = ", "),
        if(length(biomass_columns) > 5) "..." else "", "\n")
    cat("[DEBUG] Carbon columns:", paste(head(carbon_columns, 5), collapse = ", "),
        if(length(carbon_columns) > 5) "..." else "", "\n")

    # Row-by-row biomass calculation
    for (i in seq_len(nrow(df_result))) {
      tree_species <- df_result$Species[i]
      row_biomass_total <- 0
      row_carbon_total <- 0
      row_has_biomass <- FALSE

      if (!is.na(tree_species) && tree_species %in% names(eq_mapping)) {
        species_equations <- eq_mapping[[tree_species]]

        # Calculate biomass for each equation of this species
        for (eq_idx in 1:nrow(species_equations)) {
          eq <- species_equations[eq_idx, , drop = FALSE]

          # Generate column names
          biomass_col_name <- paste0(tree_species, "_Biomass_A0", eq$A0, "_Eq", eq_idx)
          carbon_col_name <- paste0(tree_species, "_Carbon_A0", eq$A0, "_Eq", eq_idx)
          eq_col_name <- paste0(biomass_col_name, "_Equation")

          # Track which equation is being used
          df_result[[eq_col_name]][i] <- paste0(eq$Species, ":BIOMASS:A0=", eq$A0, ":Eq", eq_idx)

          # Get expressions from X1 to X5 columns
          exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
          exprs <- exprs[!is.na(exprs) & exprs != "0"]
          vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

          # Prepare variables for evaluation
          variables <- list()
          for (v in vars_needed) {
            if (v %in% names(df_result)) {
              variables[[v]] <- df_result[[v]][i]
            }
          }

          # Check A0 value for equation type
          a0_value <- eq$A0[1]

          if (!is.na(a0_value)) {
            biomass_value <- NA

            if (a0_value %in% c(1, 2, 3, 5)) {
              # Linear equation: Biomass = b0 + b1*X1 + b2*X2 + ... + b5*X5
              biomass_value <- eq$b0[1]

              for (j in 1:5) {
                x_col <- paste0("X", j)
                b_col <- paste0("b", j)

                # Check if the X expression exists and is valid
                if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
                  # Evaluate the expression
                  x_val <- evaluate_expression(eq[[x_col]][1], variables)

                  if (!is.na(x_val)) {
                    b_val <- eq[[b_col]][1]
                    if (!is.na(b_val)) {
                      biomass_value <- biomass_value + b_val * x_val

                      if (i <= 3) {  # Reduced debug output
                        cat("[DEBUG] Row", i, "- Eq", eq_idx, "- X", j, "=", x_val, "- b", j, "=", b_val, "\n")
                      }
                    }
                  }
                }
              }

            } else if (a0_value == 4) {
              # Logarithmic equation: Biomass = 10^(b0 + b1*log10(X1) + ...)
              log_sum <- eq$b0[1]

              for (j in 1:5) {
                x_col <- paste0("X", j)
                b_col <- paste0("b", j)

                if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
                  x_val <- evaluate_expression(eq[[x_col]][1], variables)

                  if (!is.na(x_val) && x_val > 0) {
                    b_val <- eq[[b_col]][1]
                    if (!is.na(b_val)) {
                      log_sum <- log_sum + b_val * log10(x_val)
                    }
                  } else if (!is.na(x_val) && x_val <= 0) {
                    warning(paste("Cannot calculate log of non-positive value:", x_val, "at row", i))
                    log_sum <- NA
                    break
                  }
                }
              }

              if (!is.na(log_sum)) {
                biomass_value <- 10^log_sum
              }
            }

            # Store the calculated biomass and carbon for this specific equation
            if (!is.na(biomass_value) && is.finite(biomass_value)) {
              # Calculate carbon (biomass * 0.5)
              carbon_value <- biomass_value * 0.5

              # Store biomass and carbon values
              df_result[[biomass_col_name]][i] <- biomass_value
              df_result[[carbon_col_name]][i] <- carbon_value

              # Add to totals
              row_biomass_total <- row_biomass_total + biomass_value
              row_carbon_total <- row_carbon_total + carbon_value
              row_has_biomass <- TRUE
              calculated_biomass <- calculated_biomass + 1

              if (i <= 3) {
                cat("[DEBUG] Row", i, "- Species:", tree_species,
                    "- Eq", eq_idx, "- Biomass:", round(biomass_value, 4),
                    "- Carbon:", round(carbon_value, 4), "\n")
              }
            }
          }
        }

        # Store total biomass and carbon for this row
        if (row_has_biomass) {
          df_result$Biomass_Total[i] <- row_biomass_total
          df_result$Carbon_Total[i] <- row_carbon_total
        }

      } else if (!is.na(tree_species)) {
        # Track species without equations
        if (!tree_species %in% species_without_equations) {
          species_without_equations <- c(species_without_equations, tree_species)
        }

        if (i <= 3) {
          cat("[DEBUG] Row", i, "- Species:", tree_species, "- No equation found\n")
        }
      }
    }

    # Report species without equations
    if (length(species_without_equations) > 0) {
      warning(paste("No biomass equations found for the following species:",
                    paste(species_without_equations, collapse = ", ")))
      cat("[DEBUG] [Warning] Species without biomass equations (", length(species_without_equations), "):",
          paste(species_without_equations, collapse = ", "), "\n")
    }

    cat("[DEBUG] [OK] Biomass and carbon calculated for", calculated_biomass, "equation applications\n")

    # Display summary statistics
    total_biomass_values <- df_result$Biomass_Total[!is.na(df_result$Biomass_Total)]
    total_carbon_values <- df_result$Carbon_Total[!is.na(df_result$Carbon_Total)]

    if (length(total_biomass_values) > 0) {
      cat("[DEBUG] Total biomass examples:", paste(round(head(total_biomass_values, 3), 4), collapse = ", "), "\n")
      cat("[DEBUG] Total biomass range: [", round(min(total_biomass_values), 4), " - ",
          round(max(total_biomass_values), 4), "]\n")
      cat("[DEBUG] Trees with biomass calculations:", length(total_biomass_values), "\n")
    }

    if (length(total_carbon_values) > 0) {
      cat("[DEBUG] Total carbon examples:", paste(round(head(total_carbon_values, 3), 4), collapse = ", "), "\n")
      cat("[DEBUG] Total carbon range: [", round(min(total_carbon_values), 4), " - ",
          round(max(total_carbon_values), 4), "]\n")
      cat("[DEBUG] Trees with carbon calculations:", length(total_carbon_values), "\n")
    }

    # Display column summary
    biomass_cols_with_data <- sapply(biomass_columns, function(col) sum(!is.na(df_result[[col]])))
    carbon_cols_with_data <- sapply(carbon_columns, function(col) sum(!is.na(df_result[[col]])))

    cat("[DEBUG] Biomass columns with data:\n")
    for (col in names(biomass_cols_with_data[biomass_cols_with_data > 0])) {
      cat("[DEBUG]   ", col, ":", biomass_cols_with_data[col], "values\n")
    }

    cat("[DEBUG] Carbon columns with data:\n")
    for (col in names(carbon_cols_with_data[carbon_cols_with_data > 0])) {
      cat("[DEBUG]   ", col, ":", carbon_cols_with_data[col], "values\n")
    }

    cat("[DEBUG] [OK] Biomass and carbon calculation completed\n\n")

    return(df_result)
  }

  # Automatic bark thickness calculation for all volume types
  if (bark) {
    df_result <- calculate_bark_thickness(df_result, volume_type)
  }

  # Automatic biomass and carbon calculation if requested
  if (carbon) {
    df_result <- calculate_biomass(df_result)
  }

  # Final cleanup if requested
  if (remove_na) {
    df_result <- df_result[!is.na(df_result[[volume_type]]), ]
  }

  return(df_result)

}
