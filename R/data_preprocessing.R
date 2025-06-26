
#' Convertir un data.frame en objet carbofor_data
#' @export
as.carbofor_data <- function(x) {
  debug_log("Conversion du data.frame en objet 'carbofor_data'")
  class(x) <- c("carbofor_data", class(x))
  return(x)
}

flags <- list(
  C130_exists = FALSE,
  C150_exists = FALSE,
  D130_exists = FALSE,
  D150_exists = FALSE,
  HTOT_exists = FALSE,
  HDOM_exists = FALSE
)

update_flags <- function(x, flags, C130, C150, D130, D150, HTOT, HDOM) {
  flags$C130_exists <- C130 %in% colnames(x)
  flags$C150_exists <- C150 %in% colnames(x)
  flags$D130_exists <- D130 %in% colnames(x)
  flags$D150_exists <- D150 %in% colnames(x)
  flags$HTOT_exists <- HTOT %in% colnames(x)
  flags$HDOM_exists <- HDOM %in% colnames(x)
  return(flags)
}

debug_log <- function(msg) {
  message(sprintf("[DEBUG] %s", msg))
}

#' Prétraitement des données pour le calcul de volume
#' @export
preprocess_data <- function(x, specimens = NULL, C130 = "C130", C150 = "C150",
                            D130 = "D130", D150 = "D150", HTOT = "HTOT", HDOM = "HDOM", ...) {
  debug_log("Début du prétraitement des données")

  x <- establish_species_correspondence(x, specimens)
  debug_log("Correspondance des espèces établie")

  x <- diameter_conversions(x, C130 = C130, C150 = C150, D130 = D130, D150 = D150)
  debug_log("Conversions diamètre/circonférence effectuées")

  x <- convert_circumference(x, C130 = C130, C150 = C150)
  debug_log("Conversion générique entre C130 et C150 effectuée")

  x <- calculate_basal_areas(x, C130 , C150)
  debug_log("Calcul des surfaces basales terminé")

  debug_log("Fin du prétraitement")
  return(x)
}

#' Correspondance des espèces à partir des codes ou abréviations
#' @export
# Establish species correspondence
establish_species_correspondence <- function(x, specimens) {
  if (is.null(specimens)) {
    stop("No species identification column specified or found in the data.")
  }

  if (!(specimens %in% colnames(x))) {
    stop(paste("The specified column '", specimens, "' does not exist in the data.", sep=""))
  }

  specimens_type <- detect_specimens_type(x, specimens)

  # Add Species column if necessary
  if (specimens_type != "Species" || specimens != "Species") {
    # Check required columns in equations
    if (!all(c("Species", specimens_type) %in% colnames(equations))) {
      stop(paste("The 'equations' dataframe must contain the columns 'Species' and '",
                 specimens_type, "'", sep=""))
    }

    # Create mapping
    mapping_df <- unique(equations[, c("Species", specimens_type)])
    names(mapping_df) <- c("Species", specimens)

    # Merge

    conflicts <- intersect(names(x), names(mapping_df))
    conflicts <- setdiff(conflicts, specimens)
    if (length(conflicts) > 0) {
      x <- x[, !(names(x) %in% conflicts)]
    }
    x <- merge(x, mapping_df, by = specimens, all.x = TRUE)

    # Clean duplicated columns
    if ("Species.x" %in% colnames(x)) {
      names(x)[names(x) == "Species.x"] <- "Species"
      if ("Species.y" %in% colnames(x)) {
        x$Species.y <- NULL
      }
    }

    # Check for missing correspondences
    missing_mask <- is.na(x$Species)
    if (any(missing_mask)) {
      na_values <- unique(x[[specimens]][is.na(x$Species)])
      warning(paste("No correspondence found for the following values of",
                    specimens, ":", paste(na_values, collapse=", ")))
    }
  } else {
    names(x)[names(x) == "Species"] <- "Species"
  }

  #debug
  cat("[DEBUG] ==================== SPECIES CORRESPONDENCE ====================\n")
  cat("[DEBUG] Used specimens column:", specimens, " (type:", specimens_type, ")\n")
  cat("[DEBUG] equations dimensions: [", nrow(equations), "x", ncol(equations), "]\n")
  cat("[DEBUG] equations columns:", paste(colnames(equations), collapse = ", "), "\n")
  cat("[DEBUG] Number of rows before merge:", nrow(df), "\n")
  if (exists("mapping_df")) {
    cat("[DEBUG] Correspondences created:", nrow(mapping_df), "\n")
    cat("[DEBUG] Mapping preview:\n")
    print(utils::head(mapping_df, 3))
  }
  cat("[DEBUG] Number of rows after merge:", nrow(x), "\n")
  values_without_correspondence <- unique(x[[specimens]][is.na(x$Species)])
  if (length(values_without_correspondence) > 0) {
    cat("[DEBUG] [Warning]  Values without correspondence (", length(values_without_correspondence), "):",
        paste(utils::head(values_without_correspondence, 5), collapse = ", "), "\n")
  }
  cat("[DEBUG] [OK]Species correspondence completed\n\n")

  return(x)
}

#' Détection automatique du type d'identifiant d'espèce
#' @export
detect_specimens_type <- function(x, specimens) {
  sample_values <- na.omit(x[[specimens]])

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
  cat("[DEBUG] R data type:", class(x[[specimens]]), "\n")
  cat("[DEBUG] Number of non-NA values:", length(sample_values), "\n")
  cat("[DEBUG] Sample values:", paste(utils::head(sample_values, 3), collapse = ", "), "\n")
  if (is.character(sample_values) || is.factor(sample_values)) {
    cat("[DEBUG] Average character length:", round(mean_length, 2), "\n")
  }
  cat("[DEBUG] Detected identification type:", specimens_type, "\n")
  cat("[DEBUG] [OK]Specimens type detection completed\n\n")
  return(specimens_type)
}

#' Conversion diamètre <-> circonférence
#' @export
diameter_conversions <- function(x, C130, C150, D130 ,D150, HTOT = "HTOT", HDOM = "HDOM") {
  pi_val <- pi

  # Initialize target columns if they don't exist
  cat("Type de flags$D130_exists :", typeof(flags$D130_exists), "\n")
  cat("Valeur de flags$D130_exists :", flags$D130_exists, "\n")

  if (!(C130 %in% colnames(x))) x[[C130]] <- NA_real_
  if (!(C150 %in% colnames(x))) x[[C150]] <- NA_real_
  if (!(D130 %in% colnames(x))) x[[D130]] <- NA_real_
  if (!(D150 %in% colnames(x))) x[[D150]] <- NA_real_

  for (i in seq_len(nrow(x))) {
    # D130 → C130 (TOUJOURS si D130 existe)
    if (!is.na(x[[D130]][i]) && is.na(x[[C130]][i])) {
      x[[C130]][i] <- x[[D130]][i] * pi_val
      cat("[DEBUG] Ligne", i, ":", x$Species[i], "- D130", x[[D130]][i], "→ C130", x[[C130]][i], "\n")
    }

    # D150 → C150 (si D150 existe)
    if (!is.na(x[[D150]][i]) && is.na(x[[C150]][i])) {
      x[[C150]][i] <- x[[D150]][i] * pi_val
      cat("[DEBUG] Ligne", i, ":", x$Species[i], "- D150", x[[D150]][i], "→ C150", x[[C150]][i], "\n")
    }

    # C130 → D130 (si C130 existe mais pas D130)
    if (!is.na(x[[C130]][i]) && is.na(x[[D130]][i])) {
      x[[D130]][i] <- x[[C130]][i] / pi_val
      cat("[DEBUG] Ligne", i, ":", x$Species[i], "- C130", x[[C130]][i], "→ D130", x[[D130]][i], "\n")
    }

    # C150 → D150 (si C150 existe mais pas D150)
    if (!is.na(x[[C150]][i]) && is.na(x[[D150]][i])) {
      x[[D150]][i] <- x[[C150]][i] / pi_val
      cat("[DEBUG] Ligne", i, ":", x$Species[i], "- C150", x[[C150]][i], "→ D150", x[[D150]][i], "\n")
    }
  }
  cat("[DEBUG] Required conversions:\n")
  if (flags$D130_exists) {
    na_before_D130 <- sum(is.na(x[[D130]]))
    cat("[DEBUG]   - D130 to C130: ", nrow(x) - na_before_D130, " values to convert\n")
    cat("[DEBUG] D130 example:", utils::head(x[[D130]], 5), "\n")
  }
  if (flags$D150_exists) {
    na_before_D150 <- sum(is.na(x[[D150]]))
    cat("[DEBUG]   - D150 to C150: ", nrow(x) - na_before_D150, " values to convert\n")
    cat("[DEBUG] D150 example:", utils::head(x[[D150]], 5), "\n")
  }
  cat("[DEBUG] pi coefficient used:", round(pi, 6), "\n")

  #debug
  if (flags$D130_exists && "C130" %in% colnames(x)) {
    successful_conversions_130 <- sum(!is.na(x$C130) & flags$D130_exists & !is.na(x[[D130]]))
    cat("[DEBUG] [OK]Successful D130 to C130 conversions:", successful_conversions_130, "\n")
  }
  if (flags$D150_exists && "C150" %in% colnames(x)) {
    successful_conversions_150 <- sum(!is.na(x$C150) & flags$D150_exists & !is.na(x[[D150]]))
    cat("[DEBUG] [OK]Successful D150 to C150 conversions:", successful_conversions_150, "\n")
  }
  cat("[DEBUG] [OK]Diameter conversions completed\n\n")

  # Warning messages if some conversions fail
  if (flags$D130_exists && sum(is.na(x$C130)) > 0 && sum(!is.na(x[[D130]])) > 0) {
    warning("Some D130 -> C130 conversions failed (missing or unprocessed values).")
  }
  if (flags$D150_exists && sum(is.na(x$C150)) > 0 && sum(!is.na(x[[D150]])) > 0) {
    warning("Some D150 -> C150 conversions failed (missing or unprocessed values).")
  }

  #debug
  if (C130 %in% colnames(x)) {
    cat("[DEBUG] C130 result examples:", utils::head(x$C130, 5), "\n")
  }
  if (C150 %in% colnames(x)) {
    cat("[DEBUG] C150 result examples:", utils::head(x$C150, 5), "\n")
  }
  cat("Diameter to circumference conversion completed.\n")

  # Update global flags

  flags <- update_flags(x, flags, C130, C150, D130, D150, HTOT , HDOM)


  return(x)
}


#' Conversion générique entre C130 et C150
#' @export
convert_circumference <- function(x, D150 ="D150", D130 ="D130", C150="C150", C130="C130", HTOT="HTOT", HDOM="HDOM") {
  cat("[DEBUG] ==================== GENERIC CIRCUMFERENCE CONVERSION ====================\n")

  skip_conversion <- FALSE

  # Définir les variables locales d'existence des colonnes
  C130_exists_local <- C130 %in% colnames(x)
  C150_exists_local <- C150 %in% colnames(x)

  cat("[DEBUG] C130_exists_local =", C130_exists_local, "\n")
  cat("[DEBUG] C150_exists_local =", C150_exists_local, "\n")

  C130_has_values <- C130_exists_local && sum(!is.na(x[[C130]])) > 0
  C150_has_values <- C150_exists_local && sum(!is.na(x[[C150]])) > 0
  cat("[DEBUG] C130_has_values =", C130_has_values, "\n")
  cat("[DEBUG] C150_has_values =", C150_has_values, "\n")

  # Determine the conversion direction
  if (!C130_has_values && C150_has_values) {
    direction <- "C150_to_C130"
    from_col <- C150
    to_col <- C130
    cat("[DEBUG] Conversion detected: C150 → C130\n")
  } else if (!C150_has_values && C130_has_values) {
    direction <- "C130_to_C150"
    from_col <- C130
    to_col <- C150
    cat("[DEBUG] Conversion detected: C130 → C150\n")
  } else if (C130_has_values && C150_has_values) {
    # CAS AJOUTÉ : Les deux colonnes ont des valeurs
    cat("[DEBUG] Both C130 and C150 have values - analyzing pattern of missing values\n")

    # Compter les valeurs manquantes dans chaque colonne
    C130_missing <- sum(is.na(x[[C130]]))
    C150_missing <- sum(is.na(x[[C150]]))

    cat("[DEBUG] C130 missing values:", C130_missing, "\n")
    cat("[DEBUG] C150 missing values:", C150_missing, "\n")

    # Analyser les patterns ligne par ligne
    both_missing <- sum(is.na(x[[C130]]) & is.na(x[[C150]]))
    both_present <- sum(!is.na(x[[C130]]) & !is.na(x[[C150]]))
    c130_only <- sum(!is.na(x[[C130]]) & is.na(x[[C150]]))
    c150_only <- sum(is.na(x[[C130]]) & !is.na(x[[C150]]))

    cat("[DEBUG] Pattern analysis:\n")
    cat("[DEBUG]   Both missing:", both_missing, "rows\n")
    cat("[DEBUG]   Both present:", both_present, "rows\n")
    cat("[DEBUG]   C130 only:", c130_only, "rows\n")
    cat("[DEBUG]   C150 only:", c150_only, "rows\n")

    if (C130_missing > 0 || C150_missing > 0) {
      direction <- "bidirectional"
      cat("[DEBUG] Using bidirectional conversion approach\n")
    } else {
      cat("[DEBUG] Both columns are complete - no conversion needed\n")
      skip_conversion <- TRUE
    }
  } else {
    cat("[DEBUG] No circumference conversion needed\n")
    skip_conversion <- TRUE
  }

  # Effectuer la conversion si nécessaire
  if (!skip_conversion) {
    cat("[DEBUG] ==================== CIRCUMFERENCE CONVERSIONS ====================\n")

    if (direction == "bidirectional") {
      cat("[DEBUG] Using bidirectional conversion approach\n")
      cat("[DEBUG] Will convert C130→C150 and C150→C130 as needed per row\n")
    } else {
      cat("[DEBUG] Detected conversion direction:", direction, "\n")
      cat("[DEBUG] Source column:", from_col, "\n")
      cat("[DEBUG] Target column:", to_col, "\n")
    }

    # Extract conversion coefficients (UNE SEULE FOIS)
    coefs_x <- unique(equations[, c("Species", "HV", "IV")])
    names(coefs_x)[names(coefs_x) == "Species"] <- "Species"

    cat("[DEBUG] Available coefficients for", nrow(coefs_x), "species\n")
    cat("[DEBUG] HV/IV coefficients preview:\n")
    print(utils::head(coefs_x[, c("Species", "HV", "IV")], 3))

    cat("Conversion coefficients preview:\n")
    print(utils::head(coefs_x))

    # Row-by-row application
    attempted_conversions <- 0
    c130_to_c150_conversions <- 0
    c150_to_c130_conversions <- 0

    for (i in seq_len(nrow(x))) {
      tree_species <- x$Species[i]

      if (!is.na(tree_species)) {
        c130_value <- x[[C130]][i]
        c150_value <- x[[C150]][i]

        # Déterminer quelle conversion effectuer pour cette ligne spécifique
        current_direction <- NULL
        current_from_col <- NULL
        current_to_col <- NULL
        current_from_value <- NULL

        if (direction == "bidirectional") {
          # Approche bidirectionnelle
          if (is.na(c130_value) && !is.na(c150_value)) {
            current_direction <- "C150_to_C130"
            current_from_col <- C150
            current_to_col <- C130
            current_from_value <- c150_value
          } else if (!is.na(c130_value) && is.na(c150_value)) {
            current_direction <- "C130_to_C150"
            current_from_col <- C130
            current_to_col <- C150
            current_from_value <- c130_value
          }
        } else {
          # Approche directionnelle classique
          current_direction <- direction
          current_from_col <- from_col
          current_to_col <- to_col
          current_from_value <- x[[from_col]][i]
          current_to_value <- x[[to_col]][i]

          # Ne convertir que si la valeur source existe ET la valeur cible est manquante
          if (is.na(current_from_value) || !is.na(current_to_value)) {
            current_direction <- NULL  # Skip cette ligne
          }
        }

        # Effectuer la conversion si les conditions sont remplies
        if (!is.null(current_direction) && !is.na(current_from_value)) {
          attempted_conversions <- attempted_conversions + 1

          if (current_direction == "C130_to_C150") {
            c130_to_c150_conversions <- c130_to_c150_conversions + 1
          } else {
            c150_to_c130_conversions <- c150_to_c130_conversions + 1
          }

          coef_row <- coefs_x[coefs_x$Species == tree_species, ]

          if (i <= 5) {
            cat("Row", i, "- Species:", tree_species, "- Direction:", current_direction, "\n")
            cat("  From value:", current_from_value, "- Coefficients found:", nrow(coef_row), "\n")
            if (nrow(coef_row) > 0) {
              cat("  HV:", coef_row$HV[1], "IV:", coef_row$IV[1], "\n")
            }
          }

          if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
            HV_coef <- coef_row$HV[1]
            IV_coef <- coef_row$IV[1]

            result_value <- if (current_direction == "C150_to_C130") {
              HV_coef * current_from_value + IV_coef
            } else {
              (current_from_value - IV_coef) / HV_coef
            }

            x[[current_to_col]][i] <- result_value

            if (i <= 5) {
              cat("  Conversion:", current_from_value, "to", result_value, "\n")
            }
          } else {
            warning(paste("Unable to convert", current_from_col, "to", current_to_col, "for species:",
                          tree_species, "at row", i, ". Missing coefficients."))
          }
        }
      }
    }

    # Messages de résultats
    if (direction == "bidirectional") {
      cat("[DEBUG] Conversions attempted:", attempted_conversions, "\n")
      cat("[DEBUG] C130→C150 conversions:", c130_to_c150_conversions, "\n")
      cat("[DEBUG] C150→C130 conversions:", c150_to_c130_conversions, "\n")

      remaining_c130_missing <- sum(is.na(x[[C130]]))
      remaining_c150_missing <- sum(is.na(x[[C150]]))
      cat("[DEBUG] After bidirectional conversion:\n")
      cat("[DEBUG]   Remaining C130 missing:", remaining_c130_missing, "\n")
      cat("[DEBUG]   Remaining C150 missing:", remaining_c150_missing, "\n")
    } else {
      successful_conversions <- sum(!is.na(x[[to_col]]))
      cat("[DEBUG] Conversions to attempt:", attempted_conversions, "\n")
      cat("[DEBUG] [OK]Successful", from_col, "to", to_col, "conversions:", successful_conversions, "\n")
      if (attempted_conversions > successful_conversions) {
        cat("[DEBUG] [Warning]  Failed conversions:", attempted_conversions - successful_conversions, "\n")
      }

      failed_conversions <- sum(is.na(x[[to_col]]))
      if (failed_conversions > 0) {
        warning(paste(failed_conversions, paste(from_col, "to", to_col),
                      "conversions failed. Check the data."))
      }
    }
  }

  # Row-by-row calculation of D130
  if (C130 %in% colnames(x)) {
    if (!(D130 %in% colnames(x))) x[[D130]] <- NA_real_
    for (i in seq_len(nrow(x))) {
      if (is.na(x[[D130]][i]) && !is.na(x[[C130]][i])) {
        x[[D130]][i] <- x[[C130]][i] / pi
      }
    }
  }

  # Row-by-row calculation of D150
  if (C150 %in% colnames(x)) {
    if (!(D150 %in% colnames(x))) x[[D150]] <- NA_real_
    for (i in seq_len(nrow(x))) {
      if (is.na(x[[D150]][i]) && !is.na(x[[C150]][i])) {
        x[[D150]][i] <- x[[C150]][i] / pi
      }
    }
  }

  if ("D130" %in% colnames(x) && !"D130" %in% colnames(x)) {
    cat("[DEBUG] [OK]D130 column created from C130\n")
  }
  if ("D150" %in% colnames(x) && !"D150" %in% colnames(x)) {
    cat("[DEBUG] [OK]D150 column created from C150\n")
  }
  cat("[DEBUG] [OK]Circumference conversions completed\n\n")
  flags <- update_flags(x, flags, C130, C150, D130, D150, HTOT, HDOM)
  return(x)
}


# Calculate basal areas
#' @export
calculate_basal_areas <- function(x, C130, C150) {
  #debug
  cat("[DEBUG] ==================== BASAL AREA CALCULATION ====================\n")
  calculated_areas <- c()
  # Calculate G130 if necessary
  if (!"G130" %in% colnames(x) && C130 %in% colnames(x)) {
    x$G130 <- (x[[C130]]^2) / ((4 * pi) * 10000)
    g130_values <- sum(!is.na(x$G130))
    cat("[DEBUG] [OK]G130 calculated for", g130_values, "trees\n")
    cat("[DEBUG] G130 examples:", paste(round(utils::head(x$G130[!is.na(x$G130)], 3), 6), collapse = ", "), "\n")
    calculated_areas <- c(calculated_areas, "G130")
  }
  # Calculate G150 if necessary
  if (!"G150" %in% colnames(x) && C150 %in% colnames(x)) {
    x$G150 <- (x[[C150]]^2) / ((4 * pi) * 10000)
    g150_values <- sum(!is.na(x$G150))
    cat("[DEBUG] [OK]G150 calculated for", g150_values, "trees\n")
    cat("[DEBUG] G150 examples:", paste(round(utils::head(x$G150[!is.na(x$G150)], 3), 6), collapse = ", "), "\n")
    calculated_areas <- c(calculated_areas, "G150")
  }
  if (length(calculated_areas) == 0) {
    cat("[DEBUG] [INFO]  No basal area to calculate (already present)\n")
  } else {
    cat("[DEBUG] Calculated basal areas:", paste(calculated_areas, collapse = ", "), "\n")
  }
  cat("[DEBUG] Formula used: G = C^2/(4pi*10000)\n")
  cat("[DEBUG] [OK]Basal area calculation completed\n\n")
  return(x)
}
