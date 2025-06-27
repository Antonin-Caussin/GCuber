#' Calcul du volume à partir des équations allométriques
#' @description Applique les équations allométriques spécifiques à chaque espèce pour calculer le volume.
#' @param df Le data.frame contenant les données d'arbres
#' @param volume_type Type de volume à calculer (ex: "V22", "V22B", "V22_HA", "E")
#' @param equation_id Identifiant de l'équation à utiliser
#' @param source Source des équations (ex: "Dagnellie")
#' @return Le data.frame enrichi avec les colonnes de volume, validité et équation utilisée
#' @export
calculate_volume <- function(x, volume_type = "V22", equations, equation_id = 1,
                             D130 = "D130", specimens = NULL, source = "Dagnellie") {

  # Initialisation des compteurs pour le résumé de validité
  trees_checked <- 0
  trees_outside_domain <- 0
  trees_no_validity_limits <- 0
  trees_below_min <- 0
  trees_above_max <- 0

  # Initialisation des colonnes de résultats
  if (!volume_type %in% names(x)) {
    x[[volume_type]] <- NA_real_
  }
  if (!"Validity_Status" %in% names(x)) {
    x$Validity_Status <- NA_character_
  }
  if (!"Equation_Used" %in% names(x)) {
    x$Equation_Used <- NA_character_
  }

  cat("[DEBUG] ==================== DÉBUT DU CALCUL DE VOLUME ====================\n")
  cat("[DEBUG] Nombre d'arbres à traiter:", nrow(x), "\n")
  cat("[DEBUG] Colonne D130 utilisée:", D130, "\n")
  cat("[DEBUG] Type de volume:", volume_type, "\n")
  cat("[DEBUG] ID d'équation demandé:", equation_id, "\n")
  cat("[DEBUG] Source:", source, "\n")

  # =========================================================================
  # NOUVELLE MÉTHODE DE FILTRAGE DES ÉQUATIONS
  # =========================================================================

  # Filter equations by volume type first
  eqs_volume <- equations[equations$Y == volume_type, ]

  # Calculation for each row
  for (i in seq_len(nrow(x))) {
    tree_species <- x$Species[i]
    DHB_value <- x[[D130]][i]  # Fixed: use D130 parameter instead of DHB

    # Initialize volume variable for this row
    volume_res <- 0
    local_equation_id <- equation_id
    eq_candidates <- eqs_volume[eqs_volume$Species == tree_species, ]

    # DEBUG: Display candidate equations for this species
    if (i <= 5) {
      cat("  Row", i, "- Species:", tree_species, "- Number of candidate equations:", nrow(eq_candidates), "\n")
    }

    if (nrow(eq_candidates) == 0) {
      # If no specific equation is found, warning.
      warning(paste("No equation found for species:", tree_species))
      x$Validity_Status[i] <- "NO_EQUATION"
      next
    }

    # Select appropriate equation
    if (local_equation_id > nrow(eq_candidates)) {
      warning(paste("Equation with id", local_equation_id, "does not exist for species", tree_species,
                    ". Using equation 1 instead."))
      eq <- eq_candidates[1, , drop = FALSE]
    } else {
      eq <- eq_candidates[local_equation_id, , drop = FALSE]
    }

    # Store equation information
    x$Equation_Used[i] <- paste0(eq$Species, ":", eq$Y, ":A0=", eq$A0)

    # =========================================================================
    # VÉRIFICATION DU DOMAINE DE VALIDITÉ
    # =========================================================================

    validity_status <- "VALID"

    if (!is.na(DHB_value)) {
      trees_checked <- trees_checked + 1

      # Check if validity limits exist for this specific equation
      if (!is.na(eq$D_Min[1]) && !is.na(eq$D_Max[1])) {
        D_Min <- eq$D_Min[1]
        D_Max <- eq$D_Max[1]

        # Check if value is within domain
        if (DHB_value < D_Min) {
          trees_outside_domain <- trees_outside_domain + 1
          trees_below_min <- trees_below_min + 1
          validity_status <- "BELOW_MIN"
        } else if (DHB_value > D_Max) {
          trees_outside_domain <- trees_outside_domain + 1
          trees_above_max <- trees_above_max + 1
          validity_status <- "ABOVE_MAX"
        }
      } else {
        # Equation found but no validity limits
        trees_no_validity_limits <- trees_no_validity_limits + 1
        validity_status <- "NO_LIMITS"
      }
    }
    x$Validity_Status[i] <- validity_status

    # =========================================================================
    # CALCUL DU VOLUME
    # =========================================================================

    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(x)) {
        variables[[v]] <- x[[v]][i]
      } else {
        stop(paste("Variable", v, "is used in an equation but missing from the data."))
      }
    }

    # Vérification des valeurs manquantes dans les variables nécessaires
    missing_vars <- names(variables)[sapply(variables, function(v) is.na(v) | !is.finite(v))]
    if (length(missing_vars) > 0) {
      warning(paste("Missing or invalid input variables at row", i, ":", paste(missing_vars, collapse = ", ")))
      x$Validity_Status[i] <- "MISSING_INPUT"
      next
    }


    a0_value <- eq$A0[1]

    # DEBUG: Display equation details
    if (is.na(a0_value)) {
      warning(paste("Missing A0 value for species", tree_species, "at row", i))
      next
    } else if (a0_value %in% c(1, 2, 3)) {
      # For standard linear equations, start with b0
      volume_res <- eq$b0[1]
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
          volume_res <- volume_res + b_val * x_val
        }
      }

    } else {
      warning(paste("Unknown equation type (A0 =", a0_value, ") for row", i))
      next
    }

    if (is.na(volume_res) || !is.finite(volume_res)) {
      warning(paste("Invalid volume result at row", i, ":", volume_res))
      next
    }

    # DEBUG: Display calculated volume
    if (i <= 5) {
      cat("  Calculated volume:", volume_res, "- Validity:", validity_status, "\n")
    }

    # Store volume for this row
    x[[volume_type]][i] <- volume_res
  }

  # =========================================================================
  # RÉSUMÉ DE VALIDITÉ
  # =========================================================================

  cat("[DEBUG] ==================== VALIDITY DOMAIN SUMMARY ====================\n")
  cat("[DEBUG] Trees checked for validity:", trees_checked, "\n")
  cat("[DEBUG] Trees outside validity domain:", trees_outside_domain, "\n")
  cat("[DEBUG] Trees without validity limits:", trees_no_validity_limits, "\n")
  if (trees_outside_domain > 0) {
    cat("[DEBUG]   - Below D_Min:", trees_below_min, "\n")
    cat("[DEBUG]   - Above D_Max:", trees_above_max, "\n")
  }

  # Warning message if necessary
  if (trees_outside_domain > 0) {
    percentage_outside <- round((trees_outside_domain / trees_checked) * 100, 1)
    warning(paste0("VALIDITY DOMAIN WARNING: ", trees_outside_domain, " trees (",
                   percentage_outside, "%) have DHB values outside the validity domain. ",
                   trees_below_min, " trees below D_Min and ",
                   trees_above_max, " trees above D_Max."))
  } else {
    cat("[DEBUG] [OK] All trees are within validity domain\n")
  }

  cat("[DEBUG] [OK] Volume calculation and validity check completed\n\n")

  return(x)  # Don't forget to return the modified dataframe!
}

#' Calcul de l'épaisseur et du volume d'écorce
#' @description Utilise les équations spécifiques (type E) pour estimer l'épaisseur d'écorce, puis calcule le volume d'écorce et le volume de bois net.
#' @param df Le data.frame contenant les données d'arbres
#' @param total_volume_col Nom de la colonne contenant le volume total (ex: "V22")
#' @return Le data.frame enrichi avec les colonnes E, Bark_Volume et Wood_Volume
#' @export
calculate_bark_thickness <- function(df, equations,total_volume_col = "V22") {
  if (!exists("equations")) stop("Le data.frame 'equations' doit être chargé dans l'environnement global.")
  bark_eqs <- equations[equations$Y == "E", ]
  if (nrow(bark_eqs) == 0) {
    warning("Aucune équation d'écorce (type E) trouvée.")
    return(df)
  }

  df$E <- NA_real_
  df$Bark_Volume <- NA_real_
  df$Wood_Volume <- NA_real_

  for (i in seq_len(nrow(df))) {
    sp <- df$Species[i]
    total_vol <- df[[total_volume_col]][i]
    if (is.na(sp) || is.na(total_vol)) next

    eq_sp <- bark_eqs[bark_eqs$Species == sp, ]
    if (nrow(eq_sp) == 0) next

    eq <- eq_sp[1, ]
    vars <- list()
    for (v in c("C130", "C150", "D130", "D150", "HTOT", "HDOM", "G130", "G150")) {
      if (v %in% names(df)) vars[[v]] <- df[[v]][i]
    }

    thickness <- NA_real_
    if (eq$A0 == 4) {
      val <- evaluate_expression(eq$X1, vars)
      if (!is.na(val) && val > 0) thickness <- 10^(eq$b0 + eq$b1 * log10(val))
    } else {
      thickness <- eq$b0
      for (j in 1:5) {
        xj <- eq[[paste0("X", j)]]
        bj <- eq[[paste0("b", j)]]
        if (!is.na(xj) && xj != "0") {
          val <- evaluate_expression(xj, vars)
          if (!is.na(val)) thickness <- thickness + bj * val
        }
      }
    }

    if (!is.na(thickness) && thickness > 0) {
      r_ext <- df$D130[i] / 2
      r_int <- r_ext - thickness
      if (r_int > 0) {
        bark_ratio <- (r_ext^3 - r_int^3) / r_ext^3
        bark_vol <- total_vol * bark_ratio
        wood_vol <- total_vol - bark_vol
        df$E[i] <- thickness
        df$Bark_Volume[i] <- bark_vol
        df$Wood_Volume[i] <- wood_vol
      }
    }
  }

  return(df)
}


#' Calcul de la biomasse à partir des équations spécifiques
#' @description Calcule la biomasse pour chaque arbre en appliquant toutes les équations disponibles pour son espèce.
#' @param df Le data.frame contenant les données d'arbres
#' @return Le data.frame enrichi avec les colonnes de biomasse et carbone
#' @export
# Calculate biomass with multiple equations per species and corresponding carbon
calculate_biomass <- function(x, equations) {
  cat("[DEBUG] ==================== BIOMASS CALCULATION ====================\n")

  cat("[DEBUG] Available equations in equations:", nrow(equations), "\n")
  cat("[DEBUG] Unique species in equations:", length(unique(equations$Species)), "\n")

  calculated_biomass <- 0
  species_without_equations <- c()

  # Filter biomass equations (assuming biomass type is "BIOMASS" or similar)
  eqs_biomass <- equations[equations$Y == "BIOMASS", ]  # Ajustez selon votre nomenclature

  cat("[DEBUG] Number of biomass equations found:", nrow(eqs_biomass), "\n")

  if (nrow(eqs_biomass) == 0) {
    warning("No biomass equations found in equations")
    return(x)
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

      # Initialize columns in x
      x[[biomass_col_name]] <- NA_real_
      x[[carbon_col_name]] <- NA_real_
      x[[eq_col_name]] <- NA_character_
    }
  }

  # Add total biomass and carbon columns
  x$Biomass_Total <- NA_real_
  x$Carbon_Total <- NA_real_

  cat("[DEBUG] Created", length(biomass_columns), "biomass columns and", length(carbon_columns), "carbon columns for", length(species_with_eqs), "species\n")
  cat("[DEBUG] Biomass columns:", paste(head(biomass_columns, 5), collapse = ", "),
      if(length(biomass_columns) > 5) "..." else "", "\n")
  cat("[DEBUG] Carbon columns:", paste(head(carbon_columns, 5), collapse = ", "),
      if(length(carbon_columns) > 5) "..." else "", "\n")

  # Row-by-row biomass calculation
  for (i in seq_len(nrow(x))) {
    tree_species <- x$Species[i]
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
        x[[eq_col_name]][i] <- paste0(eq$Species, ":BIOMASS:A0=", eq$A0, ":Eq", eq_idx)

        # Get expressions from X1 to X5 columns
        exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
        exprs <- exprs[!is.na(exprs) & exprs != "0"]
        vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

        # Prepare variables for evaluation
        variables <- list()
        for (v in vars_needed) {
          if (v %in% names(x)) {
            variables[[v]] <- x[[v]][i]
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

            # Store biomass and carbon values
            x[[biomass_col_name]][i] <- biomass_value

            # Add to totals
            row_biomass_total <- row_biomass_total + biomass_value
            row_has_biomass <- TRUE
            calculated_biomass <- calculated_biomass + 1

            if (i <= 3) {
              cat("[DEBUG] Row", i, "- Species:", tree_species,
                  "- Eq", eq_idx, "- Biomass:", round(biomass_value, 4))
            }
          }
        }
      }

      # Store total biomass and carbon for this row
      if (row_has_biomass) {
        x$Biomass_Total[i] <- row_biomass_total
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

  cat("[DEBUG] [OK] Biomass  calculated for", calculated_biomass, "equation applications\n")

  # Display summary statistics
  total_biomass_values <- x$Biomass_Total[!is.na(x$Biomass_Total)]

  if (length(total_biomass_values) > 0) {
    cat("[DEBUG] Total biomass examples:", paste(round(head(total_biomass_values, 3), 4), collapse = ", "), "\n")
    cat("[DEBUG] Total biomass range: [", round(min(total_biomass_values), 4), " - ",
        round(max(total_biomass_values), 4), "]\n")
    cat("[DEBUG] Trees with biomass calculations:", length(total_biomass_values), "\n")
  }


  # Display column summary
  biomass_cols_with_data <- sapply(biomass_columns, function(col) sum(!is.na(x[[col]])))

  cat("[DEBUG] Biomass columns with data:\n")
  for (col in names(biomass_cols_with_data[biomass_cols_with_data > 0])) {
    cat("[DEBUG]   ", col, ":", biomass_cols_with_data[col], "values\n")
  }

  cat("[DEBUG] [OK] Biomass calculation completed\n\n")

  return(x)
}

#' Calcul du carbone à partir de la biomasse
#' @description Calcule le carbone total comme 50% de la biomasse totale.
#' @param df Le data.frame contenant la colonne Biomass_Total
#' @return Le data.frame enrichi avec la colonne Carbon_Total
#' @export
calculate_carbon <- function(df) {
  if (!"Biomass_Total" %in% names(df)) {
    warning("La colonne 'Biomass_Total' est absente. Le carbone ne peut pas être calculé.")
    df$Carbon_Total <- NA_real_
    return(df)
  }

  df$Carbon_Total <- df$Biomass_Total * 0.5
  return(df)
}


#' Calcul de la variance individuelle et des intervalles de prédiction
#' @description Calcule la largeur relative des intervalles de prédiction pour chaque arbre.
#' @param df Le data.frame contenant les volumes calculés
#' @param volume_type Le nom de la colonne de volume (ex: "V22")
#' @param equation_id Identifiant de l'équation utilisée
#' @return Le data.frame enrichi avec les colonnes Relative_Interval_Width et Interval_Interpretation
#' @export
calculate_prediction_interval <- function(x, eqs_volume, equation_id = 1,
                                          confidence_level = 0.95) {

  # Initialize result vector (relative width)
  relative_width <- rep(NA, nrow(x))

  cat("Calculating prediction intervals with correlation handling...\n")

  for (i in seq_len(nrow(x))) {
    tree_species <- x$Species[i]

    cat("Processing tree", i, "of", nrow(x), "- Species:", tree_species, "\n")

    # Retrieve the equation used
    eq_candidates <- eqs_volume[eqs_volume$Species == tree_species, ]
    if (nrow(eq_candidates) == 0) {
      cat("  WARNING: No equation found for species:", tree_species, "\n")
      next
    }

    local_equation_id <- min(equation_id, nrow(eq_candidates))
    eq <- eq_candidates[local_equation_id, , drop = FALSE]

    cat("  Using equation", local_equation_id, "for species:", tree_species, "\n")

    # Identify necessary variables in the equation
    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]

    cat("  Found expressions:", paste(exprs, collapse = ", "), "\n")

    # Use all.vars() to correctly extract variables
    vars_needed <- c()
    for (expr in exprs) {
      if (expr != "0" && !is.na(expr)) {
        tryCatch({
          vars_in_expr <- all.vars(parse(text = expr))
          vars_needed <- c(vars_needed, vars_in_expr)
        }, error = function(e) {
          # If expression cannot be parsed, use regex as fallback
          vars_in_expr <- regmatches(expr, gregexpr("\\b[A-Za-z_][A-Za-z0-9_]*\\b", expr))[[1]]
          vars_needed <- c(vars_needed, vars_in_expr)
        })
      }
    }
    vars_needed <- unique(vars_needed)

    cat("  Variables needed:", paste(vars_needed, collapse = ", "), "\n")

    # Check that all necessary variables are available in the dataset
    missing_vars <- vars_needed[!vars_needed %in% names(x)]
    if (length(missing_vars) > 0) {
      warning(paste("Missing variables in dataset:", paste(missing_vars, collapse = ", "), "for species:", tree_species))
      next
    }

    # Retrieve variable values for this tree
    xi_values <- list()
    for (v in vars_needed) {
      xi_values[[v]] <- x[[v]][i]
      if (is.na(xi_values[[v]]) || !is.finite(xi_values[[v]])) {
        warning(paste("Invalid variable", v, "for row", i))
        next
      }
    }

    cat("  Variable values:", paste(names(xi_values), "=", sapply(xi_values, function(x) round(x, 3)), collapse = ", "), "\n")

    # Determine number of parameters/variables
    n_params <- length(vars_needed)

    cat("  Number of parameters:", n_params, "\n")

    if (n_params == 1) {
      # UNIVARIATE CASE (1 parameter only)
      cat("  Using univariate formula\n")

      # Get the variable name (first and only variable)
      var_name <- vars_needed[1]

      # Check necessary parameters for univariate case
      required_cols <- c("sigma", "n")
      x_mean_col <- paste0("x_mean_", var_name)
      sce_col <- paste0("SCE_", var_name)

      missing_cols <- required_cols[!required_cols %in% names(eq)]
      if (!x_mean_col %in% names(eq)) missing_cols <- c(missing_cols, x_mean_col)
      if (!sce_col %in% names(eq)) missing_cols <- c(missing_cols, sce_col)

      if (length(missing_cols) > 0) {
        warning(paste("Missing columns:", paste(missing_cols, collapse = ", "), "for species:", tree_species))
        cat("  Available columns:", paste(names(eq), collapse = ", "), "\n")
        next
      }

      # Check for NA values
      if (is.na(eq$sigma[1]) || is.na(eq$n[1]) || is.na(eq[[x_mean_col]][1]) || is.na(eq[[sce_col]][1])) {
        warning(paste("Missing parameters (sigma, n, x_mean or SCE) for species:", tree_species))
        next
      }

      sigma <- eq$sigma[1]
      n <- eq$n[1]
      x_mean <- eq[[x_mean_col]][1]
      SCEx <- eq[[sce_col]][1]

      cat("  Parameters: sigma =", sigma, ", n =", n, ", x_mean =", x_mean, ", SCE =", SCEx, "\n")

      # Retrieve xi value for the variable
      xi <- xi_values[[var_name]]

      # Univariate formula: Var_pred(xi) = sigma^2 * (1 + 1/n + (xi - x_bar)^2/SCEx)
      variance_pred <- sigma^2 * (1 + 1/n + (xi - x_mean)^2/SCEx)

      cat("  Prediction variance:", variance_pred, "\n")

    } else if (n_params > 1) {
      # MULTIVARIATE CASE (several correlated parameters)
      cat("  Using multivariate formula\n")

      # Check necessary parameters for multivariate case
      if (is.na(eq$sigma[1]) || is.na(eq$n[1])) {
        warning(paste("Missing parameters (sigma or n) for multivariate species:", tree_species))
        next
      }

      sigma <- eq$sigma[1]
      n <- eq$n[1]

      cat("  Base parameters: sigma =", sigma, ", n =", n, "\n")

      # Build vector of differences (xi - x_bar)
      x_diff <- numeric(n_params)
      x_means <- numeric(n_params)
      variable_names <- character(n_params)

      # Retrieve means and calculate differences
      for (j in 1:n_params) {
        var_name <- vars_needed[j]
        variable_names[j] <- var_name

        # Look for corresponding mean in format x_mean_VARIABLE
        mean_col <- paste0("x_mean_", var_name)

        if (mean_col %in% names(eq) && !is.na(eq[[mean_col]][1])) {
          x_means[j] <- eq[[mean_col]][1]
          x_diff[j] <- xi_values[[var_name]] - x_means[j]
        } else {
          warning(paste("Missing mean column", mean_col, "for variable", var_name, "of species:", tree_species))
          cat("  Available columns:", paste(names(eq), collapse = ", "), "\n")
          next
        }
      }

      cat("  Variable means:", paste(variable_names, "=", round(x_means, 3), collapse = ", "), "\n")
      cat("  Differences (xi - x_mean):", paste(round(x_diff, 3), collapse = ", "), "\n")

      # Build inverse covariance matrix (X^T*X)^-1
      # Search for matrix elements in equation columns
      cov_matrix_inv <- matrix(0, nrow = n_params, ncol = n_params)

      # Fill inverse covariance matrix
      for (j in 1:n_params) {
        for (k in 1:n_params) {
          if (j == k) {
            # Diagonal elements: use 1/SCE_variablename
            var_name <- variable_names[j]
            sce_col <- paste0("SCE_", var_name)

            if (sce_col %in% names(eq) && !is.na(eq[[sce_col]][1]) && eq[[sce_col]][1] > 0) {
              cov_matrix_inv[j, k] <- 1 / eq[[sce_col]][1]
              cat("  Found", sce_col, "=", eq[[sce_col]][1], "\n")
            } else {
              warning(paste("Missing or invalid", sce_col, "for variable", var_name, "of species:", tree_species))
              cat("  Available columns:", paste(names(eq), collapse = ", "), "\n")
              next
            }
          } else {
            # Off-diagonal elements: search for covariances based on variable names
            var_name_j <- variable_names[j]
            var_name_k <- variable_names[k]

            # Create covariance column name: COV_VARIABLE1_VARIABLE2 (alphabetical order)
            var_names_sorted <- sort(c(var_name_j, var_name_k))
            cov_col <- paste0("COV_", var_names_sorted[1], "_", var_names_sorted[2])

            if (cov_col %in% names(eq) && !is.na(eq[[cov_col]][1])) {
              cov_matrix_inv[j, k] <- eq[[cov_col]][1]
              cov_matrix_inv[k, j] <- eq[[cov_col]][1]  # Symmetric matrix
              cat("  Found", cov_col, "=", eq[[cov_col]][1], "\n")
            } else {
              # If no covariance found, use 0 (partial orthogonality assumption)
              cov_matrix_inv[j, k] <- 0
              cov_matrix_inv[k, j] <- 0
              cat("  No covariance found for", cov_col, "- assuming 0\n")
            }
          }
        }
      }

      cat("  Covariance matrix diagonal:", paste(round(diag(cov_matrix_inv), 6), collapse = ", "), "\n")

      # Check that matrix is invertible (non-zero determinant)
      tryCatch({
        det_cov <- det(cov_matrix_inv)
        cat("  Matrix determinant:", det_cov, "\n")

        if (abs(det_cov) < 1e-10) {
          warning(paste("Quasi-singular covariance matrix for species:", tree_species, "- using orthogonal approximation"))
          # Fallback: use only diagonal terms
          quadratic_form <- sum(x_diff^2 * diag(cov_matrix_inv))
        } else {
          # Calculate quadratic form: (x_i - x_bar)^T * (X^TX)^-1 * (x_i - x_bar)
          quadratic_form <- as.numeric(t(x_diff) %*% cov_matrix_inv %*% x_diff)
        }
      }, error = function(e) {
        warning(paste("Error in matrix calculation for species:", tree_species, "- using orthogonal approximation"))
        # Fallback: use only diagonal terms
        quadratic_form <- sum(x_diff^2 * diag(cov_matrix_inv))
      })

      cat("  Quadratic form:", quadratic_form, "\n")

      # Complete multivariate formula with covariance matrix
      # Var_pred = sigma^2 * (1 + 1/n + (x_i - x_bar)^T * (X^TX)^-1 * (x_i - x_bar))
      variance_pred <- sigma^2 * (1 + 1/n + quadratic_form)

      cat("  Prediction variance:", variance_pred, "\n")

    } else {
      warning(paste("No variables identified in equation for species:", tree_species))
      next
    }

    # Calculate t quantile with appropriate degrees of freedom
    t_quantile <- qt(1 - (1 - confidence_level)/2, df = n - 2)

    cat("  t-quantile (df =", n - 2, "):", t_quantile, "\n")

    # Calculate relative width of prediction interval
    # Uses Volume column directly as eqs_volume is pre-filtered
    volume_pred <- x$Volume[i]

    if (!is.na(volume_pred) && !is.na(variance_pred) && variance_pred > 0 && volume_pred > 0) {
      margin_error <- t_quantile * sqrt(variance_pred)
      interval_width <- 2 * margin_error

      # Relative width = absolute width / predicted value
      relative_width[i] <- interval_width / volume_pred

      cat("  Predicted volume:", volume_pred, "\n")
      cat("  Margin of error:", margin_error, "\n")
      cat("  Interval width:", interval_width, "\n")
      cat("  Relative width:", round(relative_width[i], 4), "\n")
    }

    cat("  ---\n")
  }

  return(relative_width)
}

# Function to interpret relative width
interpret_relative_width <- function(relative_width) {
  interpretation <- character(length(relative_width))

  for (i in seq_along(relative_width)) {
    if (is.na(relative_width[i])) {
      interpretation[i] <- "No calculation"
    } else if (relative_width[i] < 0.10) {
      interpretation[i] <- "Very narrow -> Very reliable "
    } else if (relative_width[i] <= 0.25) {
      interpretation[i] <- "Acceptable -> Rather reliable "
    } else if (relative_width[i] <= 0.50) {
      interpretation[i] <- "Wide -> Uncertain "
    } else {
      interpretation[i] <- "Very wide -> Risky "
    }
  }

  return(interpretation)
}

# Function to summarize relative widths of prediction intervals
summarize_relative_intervals <- function(relative_widths, x) {
  valid_widths <- relative_widths[!is.na(relative_widths)]

  if (length(valid_widths) == 0) {
    cat("No prediction intervals calculated.\n")
    return()
  }

  cat("=== SUMMARY OF RELATIVE INTERVAL WIDTHS ===\n")
  cat("Number of intervals calculated:", length(valid_widths), "/", length(relative_widths), "\n")
  cat("Mean relative width:", round(mean(valid_widths), 4), "\n")
  cat("Median relative width:", round(median(valid_widths), 4), "\n")
  cat("Min relative width:", round(min(valid_widths), 4), "\n")
  cat("Max relative width:", round(max(valid_widths), 4), "\n")
  cat("Standard deviation of relative widths:", round(sd(valid_widths), 4), "\n")

  # Interpretation summary
  interpretations <- interpret_relative_width(relative_widths)
  interpretation_table <- table(interpretations[!is.na(relative_widths)])

  cat("\n=== INTERPRETATION SUMMARY ===\n")
  for (i in seq_along(interpretation_table)) {
    cat(names(interpretation_table)[i], ":", interpretation_table[i], "\n")
  }

  # Summary by species if species data is available
  if ("Species" %in% names(x)) {
    valid_indices <- which(!is.na(relative_widths))
    species_data <- data.frame(
      Species = x$Species[valid_indices],
      Relative_Width = valid_widths
    )

    species_summary <- aggregate(Relative_Width ~ Species, data = species_data,
                                 FUN = function(x) c(
                                   n = length(x),
                                   mean = mean(x),
                                   median = median(x),
                                   sd = sd(x)
                                 ))

    cat("\n=== SUMMARY BY SPECIES ===\n")
    for (i in seq_len(nrow(species_summary))) {
      cat("Species:", species_summary$Species[i], "\n")
      cat("  Count:", round(species_summary$Relative_Width[i, "n"], 0),
          "- Mean:", round(species_summary$Relative_Width[i, "mean"], 4),
          "- Median:", round(species_summary$Relative_Width[i, "median"], 4),
          "- Std Dev:", round(species_summary$Relative_Width[i, "sd"], 4), "\n\n")
    }
  }

  # Useful percentiles
  cat("=== PERCENTILES ===\n")
  percentiles <- quantile(valid_widths, probs = c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95))
  for (i in seq_along(percentiles)) {
    percentile_name <- gsub("%", "", names(percentiles)[i])
    cat(paste0("P", percentile_name, ":"), round(percentiles[i], 4), "\n")
  }
}
