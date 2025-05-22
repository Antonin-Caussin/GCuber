#' Calculate different types of volumes from dendrometric data
#'
#' This function allows calculating different types of tree volumes from
#' dendrometric data using various allometric equations. It handles the diversity
#' of forest species, different measurement methods (C130, C150) and can adapt
#' to different input data structures.
#'
#' @param df A data frame containing the dendrometric data of the trees.
#' @param type_volume The type of volume to calculate. Valid values: "V22", "V22B", "E", "V22_HA". Default: "V22".
#' @param species If specified, calculations will be performed only with equations corresponding
#'        to this species. Default: NULL (uses species indicated in the data).
#' @param id_equation The identifier of the equation to use for each species, representing the entry number. Default: 1.
#' @param remove_na Boolean indicating if rows with uncalculated volumes should be removed.
#'        Default: FALSE.
#' @param C130 Name of the column containing circumference at 130 cm. Default: "C130".
#' @param C150 Name of the column containing circumference at 150 cm. Default: "C150".
#' @param HTOT Name of the column containing total height. Default: "HTOT".
#' @param HDOM Name of the column containing dominant height. Default: "HDOM".
#' @param specimens Name of the column containing species identifier (full name, code or abbreviation). Default: NULL.
#'
#' @details
#' \subsection{Supported volume types}{
#'   \itemize{
#'     \item \strong{V22} : Merchantable volume up to a 22 cm circumference cut.
#'     \item \strong{V22B} : Branch volume up to a 22cm circumference cut.
#'     \item \strong{E} : Tree bark volume.
#'     \item \strong{V22_HA} : Merchantable volume per hectare.
#'   }
#' }
#'
#' \subsection{Required input data structure}{
#'   The function requires at minimum:
#'   \itemize{
#'     \item A forest species identification column (specified via \code{specimens}).
#'     \item A diameter column (either \code{C130} or \code{C150}).
#'   }
#' }
#'
#' \subsection{Supported equation types (A0 values)}{
#'   \enumerate{
#'     \item Standard linear equation with 1 entry: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'     \item Standard linear equation with 2 entries: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'     \item Standard linear equation with 3 entries: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'     \item Logarithmic equation : Volume = 10^(b0 + b1*log10(C130))
#'     \item Standard linear equation with one entry for bark volume : Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'   }
#' }
#'
#' @return A data frame similar to \code{df} with the following additional columns:
#' \itemize{
#'   \item The column specified by \code{type_volume} containing the calculated volumes.
#'   \item \code{Equation_Used} : Information about the equation used for each row.
#'   \item If a C150 to C130 conversion was performed, a \code{C130} column is added.
#'   \item If species mapping was necessary, a \code{Species} column is added.
#' }
#'
#' @examples
#' # Basic example with standard data
#' # Suppose we have a data frame "tree_data" with C130 and Species columns
#' \dontrun{
#' results <- calculate_volumes(
#'   df = tree_data,
#'   type_volume = "V22",
#'   C130 = circ2024
#' )
#'
#' # Example with custom column names
#' results <- calculate_volumes(
#'   df = tree_data,
#'   type_volume = "E",
#'   id_equation = 2,
#'   C130 = "Circumference130",
#'   HTOT = "TotalHeight",
#'   specimens = "SpeciesName"
#' )
#'
#' # Example with C150 to C130 conversion
#' results <- calculate_volumes(
#'   df = tree_data,
#'   type_volume = "V22",
#'   C150 = "Circ150"
#' )
#' }
#'
#' @note
#' The function displays information messages during execution to facilitate debugging.
#' Warnings are issued if species matches are not found or if
#' volume calculation fails for certain rows.
#'
#' @seealso
#' Related functions for forest management and dendrometric calculations.
#'
#' @author Caussin Antonin
#' @references
#' Dagnelie, P., Rondeux, J., & Thill, A. (1985). Tables de cubage des arbres et des peuplements forestiers. Gembloux, Belgium: Presses agronomiques de Gembloux.
#'
#' @importFrom stats na.omit
#' @export

calculate_volumes <- function(df, type_volume = "V22", species = NULL,
                              id_equation = 1,
                              remove_na = FALSE,
                              C130 = "C130", C150 = "C150",
                              HTOT = "HTOT", HDOM = "HDOM",
                              specimens = NULL) {

  # Warning for type_volume = "E" with incompatible id_equation
  if (type_volume == "E" && id_equation %in% c(1, 2, 3, 4, 5)) {
    warning(paste("WARNING: For volume type 'E', it is not necessary to specify id_equation",
                  "You have specified id_equation =", id_equation,
                  "which might not be suitable or might not process the entire dataset. The function will attempt to use equation 4 and/or 5 if available."))
  }

  # Use internal equations data frame
  equations_df <- equations

  # List of valid volume types
  valid_volume_types <- c("V22", "V22B", "E", "V22_HA")

  # Check consistency between type_volume and id_equation, except for type E which will be handled specially
  if (type_volume == "V22" && !(id_equation %in% 1:3)) {
    stop("For volume type 'V22', id_equation must be between 1 and 3.")
  }

  # For type "E", we don't check consistency here as it will be handled dynamically
  # based on species in the calculation loop

  if (type_volume == "V22_ha" && id_equation != 1) {
    stop("For volume type 'V22_HA', id_equation must be 1.")
  }

  if (type_volume == "V22B" && id_equation != 1) {
    stop("For volume type 'V22B', id_equation must be 1.")
  }

  # Volume type verification
  if (!(type_volume %in% valid_volume_types)) {
    stop(paste("Invalid volume type:", type_volume,
               "\nValid types:", paste(valid_volume_types, collapse = ", ")))
  }

  # DEBUG: Display chosen volume type
  cat("Selected volume type:", type_volume, "\n")

  # Handle specimens parameter for species identification
  species_id_columns <- list()
  specimens_type <- NULL

  if (!is.null(specimens)) {
    # Check that the specified column exists in the dataframe
    if (!(specimens %in% colnames(df))) {
      stop(paste("The specified column '", specimens, "' does not exist in the data.", sep=""))
    }

    # Analyze column content to determine identifier type
    # Take a sample of non-NA values for analysis
    sample_values <- na.omit(df[[specimens]])

    if (length(sample_values) == 0) {
      stop(paste("Column '", specimens, "' contains only missing values.", sep=""))
    }

    # Determine identifier type based on data nature
    if (is.numeric(sample_values)) {
      # If values are numeric, it's probably a code
      specimens_type <- "Code"
    } else if (is.character(sample_values) || is.factor(sample_values)) {
      # Convert to character if it's a factor
      if (is.factor(sample_values)) {
        sample_values <- as.character(sample_values)
      }

      # Calculate average string length
      mean_length <- mean(nchar(sample_values))

      if (mean_length <= 4) {
        # If average length <= 4, it's probably an abbreviation
        specimens_type <- "Abr"
      } else {
        # Otherwise, it's probably the full species name
        specimens_type <- "Species"
      }
    } else {
      stop(paste("The data type in column '", specimens, "' is not recognized.", sep=""))
    }

    # Add to mapping
    species_id_columns[[specimens_type]] <- specimens
    cat("Identifier type detected in column '", specimens, "': ", specimens_type, "\n", sep="")
  }

  if (length(species_id_columns) == 0) {
    stop("No species identification column specified or found in the data.")
  }

  # Select the first available identification column
  used_species_column <- names(species_id_columns)[1]
  species_id_col <- species_id_columns[[used_species_column]]

  # DEBUG: Display identification columns
  cat("Identification column used:", used_species_column, "(", species_id_col, ")\n")

  # Create a copy of df to avoid modifying the original dataframe
  df_result <- df

  # If necessary, add a Species column to match equations
  if (used_species_column != "Species" || species_id_col != "Species") {
    # Extract unique species correspondences from equations_df
    if (!all(c("Species", used_species_column) %in% colnames(equations_df))) {
      stop(paste("The 'equations_df' dataframe must contain columns 'Species' and '",
                 used_species_column, "'", sep=""))
    }

    # DEBUG: Check existence of necessary columns
    cat("Available columns in equations_df:", paste(colnames(equations_df), collapse=", "), "\n")

    # Create a correspondence dataframe from equations_df
    mapping_df <- unique(equations_df[, c("Species", used_species_column)])
    names(mapping_df)[names(mapping_df) == "Species"] <- "Species"  # Keep as Species

    # DEBUG: Display mapping overview
    cat("Species mapping overview:\n")
    print(head(mapping_df))

    # Create a temporary Species column for calculations
    if (species_id_col != used_species_column) {
      # Temporarily rename column in mapping_df to match name in df_result
      names(mapping_df)[names(mapping_df) == used_species_column] <- species_id_col
    }

    # Keep original before merge to avoid duplications
    orig_colnames <- colnames(df_result)

    # Use merge with all.x=TRUE to keep all rows from df_result
    df_result <- merge(df_result, mapping_df, by = species_id_col, all.x = TRUE)

    # Check if merge created duplicated columns (.x, .y)
    if ("Species.x" %in% colnames(df_result)) {
      # Clean duplicated column names
      names(df_result)[names(df_result) == "Species.x"] <- "Species"

      # Remove duplicated column if it exists
      if ("Species.y" %in% colnames(df_result)) {
        df_result$Species.y <- NULL
      }
    }

    # Check if some correspondences were not found
    if (any(is.na(df_result$Species))) {
      na_values <- unique(df_result[[species_id_col]][is.na(df_result$Species)])
      warning(paste("No correspondence found for the following values of",
                    species_id_col, ":", paste(na_values, collapse=", ")))
    }
  } else {
    # If column is already called "Species", keep it
    names(df_result)[names(df_result) == "Species"] <- "Species"
  }

  # DEBUG: Check columns after mapping
  cat("Columns after mapping:", paste(colnames(df_result), collapse=", "), "\n")

  # Check if diameter columns exist
  C130_exists <- C130 %in% colnames(df)
  C150_exists <- C150 %in% colnames(df)

  if (!C130_exists && !C150_exists) {
    stop(paste("None of the specified diameter columns ('", C130, "' or '", C150, "') exist in the data.", sep=""))
  }

  # C150 to C130 conversion if necessary
  if (!C130_exists && C150_exists) {
    cat("C150 to C130 conversion necessary...\n")

    # Check that HV and IV columns exist
    if(!all(c("Species", "HV", "IV") %in% colnames(equations_df))) {
      stop("Columns 'Species', 'HV' and 'IV' must exist in the equations dataframe")
    }

    # Extract coefficients by species
    coefs_df <- unique(equations_df[, c("Species", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Species"] <- "Species"

    # DEBUG: Display coefficients
    cat("Conversion coefficients overview:\n")
    print(head(coefs_df))

    # Initialize C130 column with NA
    df_result$C130 <- NA_real_

    # Perform conversion row by row
    for (i in seq_len(nrow(df_result))) {
      tree_species <- df_result$Species[i]

      if (!is.na(tree_species)) {
        c150_value <- df_result[[C150]][i]

        if (!is.na(c150_value)) {
          # Find coefficients for this species
          coef_row <- coefs_df[coefs_df$Species == tree_species, ]

          # DEBUG: For first rows, display details
          if (i <= 5) {
            cat("Row", i, "- Species:", tree_species, "- Coefficients found:",
                nrow(coef_row), "\n")
            if (nrow(coef_row) > 0) {
              cat("  HV:", coef_row$HV[1], "IV:", coef_row$IV[1], "\n")
            }
          }

          if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
            # Get coefficients
            HV_coef <- coef_row$HV[1]
            IV_coef <- coef_row$IV[1]

            # Apply conversion formula
            df_result$C130[i] <- HV_coef * c150_value + IV_coef

            # DEBUG: Display conversion result
            if (i <= 5) {
              cat("  Conversion:", c150_value, "->", df_result$C130[i], "\n")
            }
          } else {
            warning(paste("Unable to convert C150 to C130 for species:", tree_species,
                          "at row", i, ". Missing coefficients."))
          }
        }
      }
    }

    # Check if some conversions failed
    failed_conversions <- sum(is.na(df_result$C130))
    if (failed_conversions > 0) {
      warning(paste(failed_conversions, "C150 to C130 conversions failed. Check the data."))
    }

    cat("C150 → C130 conversion completed.\n")
  }

  # C130 to C150 conversion if necessary
  if (!C150_exists && C130_exists) {
    cat("C130 to C150 conversion necessary...\n")

    # Check that HV and IV columns exist
    if(!all(c("Species", "HV", "IV") %in% colnames(equations_df))) {
      stop("Columns 'Species', 'HV' and 'IV' must exist in the equations dataframe")
    }

    # Extract coefficients by species
    coefs_df <- unique(equations_df[, c("Species", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Species"] <- "Species"

    # Initialize C150 column with NA
    df_result[[C150]] <- NA_real_

    # Perform conversion row by row
    for (i in seq_len(nrow(df_result))) {
      tree_species <- df_result$Species[i]

      if (!is.na(tree_species)) {
        c130_value <- df_result$C130[i]

        if (!is.na(c130_value)) {
          # Find coefficients for this species
          coef_row <- coefs_df[coefs_df$Species == tree_species, ]

          # DEBUG: For first rows, display details
          if (i <= 5) {
            cat("Row", i, "- Species:", tree_species, "- Coefficients found:",
                nrow(coef_row), "\n")
            if (nrow(coef_row) > 0) {
              cat("  HV:", coef_row$HV[1], "IV:", coef_row$IV[1], "\n")
            }
          }

          if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
            # Get coefficients
            HV_coef <- coef_row$HV[1]
            IV_coef <- coef_row$IV[1]

            # Apply inverse conversion formula (C130 to C150)
            # To reverse C130 = HV_coef * C150 + IV_coef
            # We get C150 = (C130 - IV_coef) / HV_coef
            df_result[[C150]][i] <- (c130_value - IV_coef) / HV_coef

            # DEBUG: Display conversion result
            if (i <= 5) {
              cat("  Inverse conversion:", c130_value, "->", df_result[[C150]][i], "\n")
            }

          } else {
            warning(paste("Unable to convert C130 to C150 for species:", tree_species,
                          "at row", i, ". Missing coefficients."))
          }
        }
      }
    }

    cat("C130 → C150 conversion completed.\n")
  }

  # Check and handle height columns
  HTOT_exists <- HTOT %in% colnames(df_result)
  HDOM_exists <- HDOM %in% colnames(df_result)

  # Copy height columns if necessary
  if (HTOT_exists && HTOT != "HTOT") {
    df_result$HTOT <- df_result[[HTOT]]
  }

  if (HDOM_exists && HDOM != "HDOM") {
    df_result$HDOM <- df_result[[HDOM]]
  }

  # Calculate basal areas if necessary
  if (!"G130" %in% colnames(df_result) && "C130" %in% colnames(df_result)) {
    df_result$G130 <- (df_result$C130^2) / ((4 * pi)*10000)
  }

  if (!"G150" %in% colnames(df_result) && C150_exists) {
    # Use the column name specified for C150
    df_result$G150 <- (df_result[[C150]]^2) / ((4 * pi)*10000)
  }

  # Filter equations
  volume_eqs <- equations_df[equations_df$Y == type_volume, ]
  # DEBUG: Display found equations
  cat("Number of equations found for", type_volume, ":", nrow(volume_eqs), "\n")
  cat("Overview of available equations:\n")
  print(head(volume_eqs[, c("Species", "Y", "A0")]))

  if (nrow(volume_eqs) == 0) {
    stop(paste("No equation found for volume type:", type_volume))
  }

  # Ensure coefficients b0 to b5 are numeric
  b_columns <- paste0("b", 0:5)
  volume_eqs[b_columns] <- lapply(volume_eqs[b_columns], as.numeric)

  # Initialization
  df_result$Equation_Used <- NA_character_

  # Initialize volume column specified by user
  if (!(type_volume %in% names(df_result))) {
    df_result[[type_volume]] <- NA_real_
  }

  # Modify evaluate_expression function for more debugging
  evaluate_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)

    # Check if expression is syntactically valid
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
      warning(paste("Error evaluating expression:", expr_text, "-", e$message))
      return(NA)
    })
  }

  # Calculation for each row
  for (i in seq_len(nrow(df_result))) {
    tree_species <- df_result$Species[i]  # Use Species instead of Essence

    # Initialize variable according to type
    if (type_volume == "E") {
      thickness <- 0  # For type E, we calculate thickness
      cat("Thickness calculation for row", i, "- Species:", tree_species, "\n")
    } else {
      volume <- 0     # For other types, we calculate volume
    }

    # For type "E" (thickness), use only equations 4 or 5
    if (type_volume == "E") {
      # Find available equations for this species and type E
      eq_candidates_E <- volume_eqs[volume_eqs$Species == tree_species, ]

      # For type E, filter only equations with A0 = 4 or A0 = 5
      eq_candidates_E_filtered <- eq_candidates_E[eq_candidates_E$A0 %in% c(4, 5), ]

      if (nrow(eq_candidates_E_filtered) > 0) {
        # Priority to equation 4, otherwise equation 5
        if (any(eq_candidates_E_filtered$A0 == 4)) {
          local_id_equation <- 4
          cat("  Using equation A0 = 4 for thickness calculation -", tree_species, "\n")
        } else {
          local_id_equation <- 5
          cat("  Using equation A0 = 5 for thickness calculation -", tree_species, "\n")
        }
      } else {
        # If no equation 4 or 5 is found for this species
        warning(paste("No equation of type 4 or 5 found for thickness calculation of species", tree_species))

        # Try with general equation if available
        eq_candidates_general <- volume_eqs[volume_eqs$Species == "General" & volume_eqs$A0 %in% c(4, 5), ]
        if (nrow(eq_candidates_general) > 0) {
          eq_candidates_E_filtered <- eq_candidates_general
          local_id_equation <- eq_candidates_general$A0[1]
          cat("  Using general equation A0 =", local_id_equation, "for", tree_species, "\n")
        } else {
          warning(paste("Unable to calculate thickness for species", tree_species, "- No appropriate equation found"))
          next
        }
      }

      eq_candidates <- eq_candidates_E_filtered
    } else {
      # For other volume types, standard behavior
      local_id_equation <- id_equation

      eq_candidates <- if (!is.null(species)) {
        volume_eqs[volume_eqs$Species == species, ]
      } else {
        volume_eqs[volume_eqs$Species == tree_species, ]
      }
    }

    # DEBUG: Display candidate equations for this species
    if (i <= 5) {
      if (type_volume == "E") {
        cat("  Number of candidate equations for thickness:", nrow(eq_candidates), "\n")
      } else {
        cat("  Number of candidate equations for volume:", nrow(eq_candidates), "\n")
      }
    }

    if (nrow(eq_candidates) == 0) {
      # If no specific equation is found, use general equation
      if (type_volume == "E") {
        eq_candidates <- volume_eqs[volume_eqs$Species == "General" & volume_eqs$A0 %in% c(4, 5), ]
      } else {
        eq_candidates <- volume_eqs[volume_eqs$Species == "General", ]
      }

      if (nrow(eq_candidates) == 0) {
        if (type_volume == "E") {
          warning(paste("No thickness equation found for species:", tree_species))
        } else {
          warning(paste("No volume equation found for species:", tree_species))
        }
        next
      }
    }

    # Selection of appropriate equation
    if (type_volume == "E") {
      # For type E, use equation with A0 corresponding to local_id_equation (4 or 5)
      eq_by_a0 <- eq_candidates[eq_candidates$A0 == local_id_equation, ]
      if (nrow(eq_by_a0) > 0) {
        eq <- eq_by_a0[1, , drop = FALSE]  # Take first equation if multiple match
        cat("  Using equation A0 =", local_id_equation, "for thickness calculation -", tree_species, "\n")
      } else {
        # If no equation with specific A0 is found
        warning(paste("No equation with A0 =", local_id_equation, "found for thickness of species", tree_species))

        # Try with other equation type (4 or 5)
        other_a0 <- if (local_id_equation == 4) 5 else 4
        eq_by_other_a0 <- eq_candidates[eq_candidates$A0 == other_a0, ]

        if (nrow(eq_by_other_a0) > 0) {
          eq <- eq_by_other_a0[1, , drop = FALSE]
          cat("  Using alternative equation A0 =", other_a0, "for thickness -", tree_species, "\n")
        } else {
          warning(paste("No appropriate thickness equation found for species", tree_species))
          next
        }
      }
    } else {
      # For other volume types, standard behavior
      if (local_id_equation > nrow(eq_candidates)) {
        warning(paste("Equation with id", local_id_equation, "does not exist for species", tree_species,
                      ". Using equation 1 instead."))
        eq <- eq_candidates[1, , drop = FALSE]
      } else {
        eq <- eq_candidates[local_id_equation, , drop = FALSE]
      }
    }

    # Record used equation
    if (type_volume == "E") {
      df_result$Equation_Used[i] <- paste0(eq$Species, ":", eq$Y, ":A0=", eq$A0, " (Thickness)")
    } else {
      df_result$Equation_Used[i] <- paste0(eq$Species, ":", eq$Y, ":A0=", eq$A0, " (Volume)")
    }

    # Prepare variables for evaluation
    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(df_result)) {
        variables[[v]] <- df_result[[v]][i]
      } else {
        stop(paste("Variable", v, "is used in an equation but absent from data."))
      }
    }

    a0_value <- eq$A0[1]

    # DEBUG: Display equation details
    if (is.na(a0_value)) {
      if (type_volume == "E") {
        warning(paste("Missing A0 value for thickness of species", tree_species, "at row", i))
      } else {
        warning(paste("Missing A0 value for volume of species", tree_species, "at row", i))
      }
      next
    }

    # Calculation according to the type of equation
    if (type_volume == "E") {
      # Thickness calculation - only for A0 = 4 or 5
      if (a0_value == 4) {
        C130 <- evaluer_expression(eq$X1[1], variables)
        if (C130 <= 0) {
          warning(paste("Negative or zero value for logarithm at line", i))
          next
        }
        thickness <- 10^(1 * eq$b0[1] + (eq$b1[1] * log10(C130)))
        if (i <= 5) {
          cat("  Calculated thickness (A0=4):", thickness, "cm\n")
        }
      } else if (a0_value == 5) {
        # For A0 = 5, standard linear equation
        thickness <- eq$b0[1]
        for (j in 1:5) {
          x_col <- paste0("X", j)
          b_col <- paste0("b", j)

          if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
            if (eq[[x_col]][1] == "/") {
              warning(paste("Invalid expression at line", i, "for X", j))
              next
            }

            x_val <- tryCatch({
              evaluer_expression(eq[[x_col]][1], variables)
            }, error = function(e) {
              warning(paste("Error evaluating X", j, ":", e$message))
              NA
            })

            if (length(x_val) == 0 || is.na(x_val)) {
              warning(paste("Evaluation of X", j, "failed at line", i))
              next
            }

            b_val <- eq[[b_col]][1]
            if (is.na(b_val)) {
              warning(paste("Missing coefficient b", j, "at line", i))
              next
            }

            if (i <= 5) {
              cat("  X", j, "=", x_val, "b", j, "=", b_val, "\n")
            }
            thickness <- thickness + b_val * x_val
          }
        }
        if (i <= 5) {
          cat("  Calculated thickness (A0=5):", thickness, "cm\n")
        }
      } else {
        warning(paste("Inappropriate equation type for thickness calculation (A0 =", a0_value, ") at line", i))
        next
      }

      # Check and store the thickness
      if (is.na(thickness) || !is.finite(thickness)) {
        warning(paste("Invalid thickness result at line", i, ":", thickness))
        next
      }

      # Store thickness in the column specified by type_volume
      df_result[[type_volume]][i] <- thickness

      if (i <= 5) {
        cat("  Thickness stored at line", i, ":", df_result[[type_volume]][i], "cm\n")
      }

    } else {
      # Volume calculation - for all other types
      if (a0_value %in% c(1, 2, 3, 5)) {
        # For standard linear equations, start with b0
        volume <- eq$b0[1]
        for (j in 1:5) {
          x_col <- paste0("X", j)
          b_col <- paste0("b", j)

          if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
            if (eq[[x_col]][1] == "/") {
              warning(paste("Invalid expression at line", i, "for X", j))
              next
            }

            x_val <- tryCatch({
              evaluer_expression(eq[[x_col]][1], variables)
            }, error = function(e) {
              warning(paste("Error evaluating X", j, ":", e$message))
              NA
            })

            if (length(x_val) == 0 || is.na(x_val)) {
              warning(paste("Evaluation of X", j, "failed at line", i))
              next
            }

            b_val <- eq[[b_col]][1]
            if (is.na(b_val)) {
              warning(paste("Missing coefficient b", j, "at line", i))
              next
            }

            if (i <= 5) {
              cat("  X", j, "=", x_val, "b", j, "=", b_val, "\n")
            }
            volume <- volume + b_val * x_val
          }
        }
      } else if (a0_value == 4) {
        C130 <- evaluer_expression(eq$X1[1], variables)
        if (C130 <= 0) {
          warning(paste("Negative or zero value for logarithm at line", i))
          next
        }
        volume <- 10^(1 * eq$b0[1] + (eq$b1[1] * log10(C130)))
      } else {
        warning(paste("Unknown equation type (A0 =", a0_value, ") at line", i))
        next
      }

      # Check and store the volume
      if (is.na(volume) || !is.finite(volume)) {
        warning(paste("Invalid volume result at line", i, ":", volume))
        next
      }

      # DEBUG: Display calculated volume
      if (i <= 5) {
        cat("  Calculated volume:", volume, "\n")
      }

      # Store volume in the column specified by type_volume
      df_result[[type_volume]][i] <- volume

      if (i <= 5) {
        cat("  Volume stored at line", i, ":", df_result[[type_volume]][i], "\n")
      }
    }

    # Final cleanup if requested
    if (remove_na) {
      df_result <- df_result[!is.na(df_result[[type_volume]]), ]
    }

    return(df_result)
  }
}
