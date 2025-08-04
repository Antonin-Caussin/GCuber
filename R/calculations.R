# =====================================================================
#                         calculate_volume()
# =====================================================================
#' Calculate Stem Volume from Allometric Equations
#'
#' @description
#' Applies species-specific allometric equations to compute the stem volume
#' of individual trees.
#'
#' @param x
#'   A data.frame containing tree-level observations. Must include at least:
#'   \itemize{
#'     \item \code{Species} (character)
#'     \item a diameter column (default name \code{D130})
#'   }
#' @param volume_type
#'   Character; type of volume to calculate. Supported values include:
#'   \code{"V22"}, \code{"V22B"}, \code{"V22_HA"}, \code{"E"}.
#' @param equations
#'   A data.frame of allometric equations with columns:
#'   \code{Species}, \code{A0}, \code{b0} to \code{b5},
#'   \code{X1} to \code{X5}, \code{D_Min}, \code{D_Max}, \code{Y}, and
#'   optionally \code{Source_Eq}.
#' @param equation_id
#'   Integer; index of the equation to use for each species
#'   (default is 1).
#' @param D130
#'   Name of the diameter-at-breast-height column (default = \code{"D130"}).
#' @param GHA150
#'   Name of the basal-area column at 150 cm (default = \code{"GHA150"}).
#' @param GHA130
#'   Name of the basal-area column at 130 cm (default = \code{"GHA130"}).
#' @param specimens
#'   Optional character vector of specimen identifiers, used to map
#'   equations to individual observations.
#' @param source
#'   Optional character; source of the equations
#'   (e.g., \code{"Dagnelie"}, \code{"Myers"}).
#'
#' @return
#'   The input \code{x} enriched with:
#'   \itemize{
#'     \item a new column \code{[volume_type] [m^3]},
#'     \item \code{Validity Status},
#'     \item \code{Equation Used},
#'     \item one \code{Volume_<Variable>_Validity} column per predictive variable.
#'   }
#'
#' @seealso
#' \code{\link{calculate_bark_thickness}} for deriving bark and wood volumes
#' from total stem volume.
#'
#' @export
#'

calculate_volume <- function(x, volume_type = "V22", equations, equation_id = 1,
                             D130 = "D130", GHA150 = "GHA150", GHA130 = "GHA130",
                             specimens = NULL, source = "Dagnelie") {

  vol_col <- paste0(volume_type, " [m^3]")
  if (!vol_col %in% names(x)) x[[vol_col]] <- NA_real_
  if (!"Validity Status" %in% names(x)) x[["Validity Status"]] <- NA_character_
  if (!"Equation Used" %in% names(x))   x[["Equation Used"]]   <- NA_character_

  eqs_volume <- equations[equations$Y == volume_type, ]
  if (!is.null(source) && "Source_Eq" %in% names(eqs_volume)) {
    eqs_volume <- eqs_volume[eqs_volume$Source_Eq == source, ]
  }

  all_validity_vars <- character()

  for (i in seq_len(nrow(x))) {
    tree_species     <- x$Species[i]
    volume_res       <- NA_real_
    local_equation_id<- equation_id
    eq_candidates    <- eqs_volume[eqs_volume$Species == tree_species, ]

    if (nrow(eq_candidates) == 0) {
      warning(sprintf("Row %d: No equation found for species '%s'", i, tree_species))
      x[["Validity Status"]][i] <- "NO EQUATION"
      next
    }

    if (local_equation_id > nrow(eq_candidates)) {
      warning(sprintf("Row %d: Equation id %d not found for '%s', using first.",
                      i, local_equation_id, tree_species))
      eq <- eq_candidates[1, , drop = FALSE]
    } else {
      eq <- eq_candidates[local_equation_id, , drop = FALSE]
    }

    x[["Equation Used"]][i] <- sprintf("%s:%s:A0=%s",
                                       eq$Species, eq$Y, eq$A0)

    validity_status <- "VALID"
    used_vars       <- character()

    # repérer toutes les variables utilisées dans X1…X5
    for (j in 1:5) {
      expr <- eq[[paste0("X", j)]]
      if (!is.na(expr) && expr != "0") {
        vars <- tryCatch(all.vars(parse(text = expr)), error = function(e) NULL)
        used_vars <- c(used_vars, vars)
      }
    }
    used_vars <- unique(used_vars)
    all_validity_vars <- unique(c(all_validity_vars, used_vars))

    # ——— VALIDATION DES DOMAINES AVEC GESTION DES NA ———
    for (var in used_vars) {
      col_validity <- paste0("Volume_", var, "_Validity")
      if (!col_validity %in% names(x)) x[[col_validity]] <- NA_character_

      value <- x[[var]][i]

      if (is.na(value)) {
        # si la donnée est manquante, on ne compare pas
        vstat <- "MISSING"
        validity_status <- "MISSING"
      } else {
        # déterminer bornes min/max
        min_col <- paste0(var, "_Min")
        max_col <- paste0(var, "_Max")
        if (all(c(min_col, max_col) %in% names(eq))) {
          limit_min <- suppressWarnings(as.numeric(eq[[min_col]]))
          limit_max <- suppressWarnings(as.numeric(eq[[max_col]]))
        } else if (var == D130) {
          limit_min <- suppressWarnings(as.numeric(eq[[paste0(D130, "_Min")]]))
          limit_max <- suppressWarnings(as.numeric(eq[[paste0(D130, "_Max")]]))
        } else {
          limit_min <- limit_max <- NA_real_
        }

        # tests uniquement si bornes non-NA
        if (is.na(limit_min) || is.na(limit_max)) {
          vstat <- "NO LIMITS"
        } else if (value < limit_min) {
          vstat <- "BELOW MIN"
          validity_status <- "OUTSIDE DOMAIN"
        } else if (value > limit_max) {
          vstat <- "ABOVE MAX"
          validity_status <- "OUTSIDE DOMAIN"
        } else {
          vstat <- "VALID"
        }
      }

      x[[col_validity]][i] <- vstat
    }

    x[["Validity Status"]][i] <- validity_status

    # si OK pour le calcul
    if (validity_status %in% c("VALID", "OUTSIDE DOMAIN", "NO LIMITS")) {
      if (!is.na(eq$A0) && eq$A0 %in% c(1,2,3)) {
        volume_res <- eq$b0
        temp_data  <- as.list(x[i, ])

        if (!is.null(eq$Source_Eq) && eq$Source_Eq == "Vallet" &&
            !is.null(temp_data$HTOT)) {
          temp_data$HTOT <- temp_data$HTOT * 10
        }

        for (j in 1:5) {
          expr <- eq[[paste0("X", j)]]
          if (!is.na(expr) && expr != "0") {
            x_val <- tryCatch(evaluate_expression(expr, temp_data),
                              error = function(e) NA_real_)
            volume_res <- volume_res + eq[[paste0("b", j)]] * x_val
          }
        }

        if (!is.null(eq$Source_Eq) && eq$Source_Eq == "Algan") {
          volume_res <- volume_res / 1000
        }
        if (!is.null(eq$Source_Eq) && eq$Source_Eq == "Vallet") {
          volume_res <- volume_res / 1000000
        }
      } else {
        warning(sprintf("Row %d: Unsupported A0 type '%s' for '%s'",
                        i, eq$A0, tree_species))
      }
      x[[vol_col]][i] <- round(volume_res, 3)
    }
  }

  return(x)
}



# =====================================================================
#                      calculate_bark_thickness()
# =====================================================================
#' Estimate Bark Thickness and Compute Bark & Wood Volumes
#'
#' @description
#' Uses species-specific allometric equations (Y == "E") to estimate bark thickness,
#' then derives bark volume and net wood volume from a total stem volume.
#'
#' @param x
#'   A \code{data.frame} of tree-level observations. Must include at least:
#'   \itemize{
#'     \item \code{Species} (character)
#'     \item \code{D130} - diameter at breast height (numeric)
#'     \item a column with total stem volume (see \code{total_volume_col})
#'   }
#' @param equations
#'   A \code{data.frame} of bark-thickness allometric equations with columns:
#'   \code{Species}, \code{A0}, \code{b0} to \code{b5}, \code{X1} to \code{X5},
#'   optionally \code{Source_Eq}, and \code{Y} == "E".
#' @param volume_type
#'   Character; suffix for default total-volume column name. If
#'   \code{total_volume_col} is \code{NULL}, uses \code{paste0(volume_type, " [m^3]")}
#'   (default \code{"V22"}).
#' @param total_volume_col
#'   Optional character; name of the column containing total stem volume.
#'   If \code{NULL}, constructed from \code{volume_type}.
#' @param source
#'   Optional character; restrict to equations from this source
#'   (e.g. \code{"Vallet"}, \code{"Dagnelie"}). If \code{NULL}, all sources are used.
#'
#' @return
#'   The input \code{x} enriched with three new columns:
#'   \itemize{
#'     \item \code{E} - estimated bark thickness (cm)
#'     \item \code{Bark Volume [m^3]} - volume of bark
#'     \item \code{Wood Volume [m^3]} - net wood volume (total minus bark)
#'   }
#'
#' @seealso
#' \code{\link{calculate_volume}} for computing total stem volume from DBH.
#'
#' @export

calculate_bark_thickness <- function(x, equations,
                                     volume_type = "V22",
                                     total_volume_col = NULL,
                                     source = NULL) {

vol_col <- if (is.null(total_volume_col)) {
    paste0(volume_type, " [m^3]")
  } else {
    total_volume_col
  }

  # Filtre des équations d'épaisseur d'écorce (Y == "E")
  bark_eqs <- subset(equations, Y == "E")
  if (!is.null(source) && "Source_Eq" %in% names(bark_eqs)) {
    bark_eqs <- subset(bark_eqs, Source_Eq == source)
  }

  if (nrow(bark_eqs) == 0) {
    warning("No bark thickness equations found; setting Wood Volume = Total Volume for all trees.")
    x[["Bark Volume [m^3]"]] <- NA_real_
    x[["Wood Volume [m^3]"]] <- as.numeric(x[[vol_col]])
    return(x)
  }

  x[["Bark Volume [m^3]"]] <- NA_real_
  x[["Wood Volume [m^3]"]] <- NA_real_

  for (i in seq_len(nrow(x))) {
    sp      <- x$Species[i]
    tot_vol <- x[[vol_col]][i]
    if (is.na(sp) || is.na(tot_vol)) next

    eq_sp <- bark_eqs[bark_eqs$Species == sp, ]
    if (nrow(eq_sp) == 0) {
      x[["Wood Volume [m^3]"]][i] <- tot_vol
      next
    }

    eq    <- eq_sp[1, , drop = FALSE]
    vars  <- as.list(x[i, ])
    thickness <- NA_real_

    if (!is.na(eq$A0) && eq$A0 == 4) {
      val <- evaluate_expression(eq$X1, vars)
      if (!is.na(val) && val > 0) {
        thickness <- 10^(eq$b0 + eq$b1 * log10(val))
      }
    } else if (!is.na(eq$b0)) {
      thickness <- eq$b0
      for (j in 1:5) {
        expr <- eq[[paste0("X", j)]]
        coef <- eq[[paste0("b", j)]]
        if (!is.na(expr) && expr != "0") {
          v <- evaluate_expression(expr, vars)
          if (!is.na(v)) thickness <- thickness + coef * v
        }
      }
    }

    if (!is.na(thickness) && thickness > 0) {
      r_ext <- x$D130[i] / 2
      r_int <- r_ext - thickness
      if (r_int > 0) {
        ratio    <- (r_ext^3 - r_int^3) / r_ext^3
        bark_vol <- round(tot_vol * ratio, 3)
        x[["Bark Volume [m^3]"]][i] <- bark_vol
        x[["Wood Volume [m^3]"]][i] <- round(tot_vol - bark_vol, 3)
      } else {
        x[["Wood Volume [m^3]"]][i] <- round(tot_vol, 3)
      }
    } else {
      x[["Wood Volume [m^3]"]][i] <- round(tot_vol, 3)
    }
  }

  return(x)
}


# =====================================================================
#                           calculate_biomass()
# =====================================================================
#' Calculate forest biomass using allometric equations or volume* density
#'
#' @description
#' Computes aboveground biomass at the tree level using one of two methods:
#'
#' - **`"equation"`**: applies species-specific allometric models provided in a reference table.
#' - **`"volume"`**: multiplies volume data (e.g., V22) by species-specific infra-density (`ID`), with optional bark volume integration.
#'
#' When `method = "volume"` and `bark = TRUE`, the function uses `Wood Volume` and `Bark Volume` (typically from `calculate_bark_thickness()`) to estimate total biomass.
#'
#' When `method = "equation"`, one or more equations are applied per species, with results stored in dynamically named columns by type of biomass (`Type_Eq`).
#'
#' Root and total biomass are always computed as 20% and 120% of the aboveground biomass, respectively.
#'
#' @param x A `data.frame` containing tree-level input data. Must include a `Species` column. Depending on the method:
#'   - For `"volume"`: must include one or more volume columns (e.g., `"V22"`, `"Aboveground"`), or `Wood Volume` and `Bark Volume` if `bark = TRUE`.
#'   - For `"equation"`: must include the predictor variables required by the selected equations (e.g., `D130`, `HTOT`, etc.).
#'
#' @param equations A `data.frame` of allometric equations. Depending on the method:
#'   - For `"volume"`: must contain columns `Species` and `ID` (infra-density in g/cm^3).
#'   - For `"equation"`: must contain `Species`, `Y`, `A0`, `b0`-`b5`, `X0`-`X5`, and optionally `Type_Eq`, `Region`.
#'
#' @param method Character string. Method to use for biomass estimation:
#'   - `"equation"`: uses allometric models.
#'   - `"volume"`: uses volume* density. Default is `"volume"`.
#'
#' @param bark Logical. If `TRUE` and `method = "volume"`, computes biomass using both `Wood Volume` and `Bark Volume`. Default is `FALSE`.
#'
#' @return A modified version of `x` with added biomass columns:
#'
#' - For `method = "equation"`:
#'   - One or more columns of the form `Biomass_<Type_Eq>` with aboveground biomass.
#'   - `Biomass Root`: root biomass (20% of total).
#'   - `Biomass Total`: total biomass (120% of aboveground).
#'
#' - For `method = "volume"` and `bark = FALSE`:
#'   - One column of the form `Biomass_<VolumeType>` (e.g., `Biomass_V22`).
#'   - `Biomass Root`, `Biomass_Total`.
#'
#' - For `method = "volume"` and `bark = TRUE`:
#'   - `Biomass_Wood`, `Biomass_Bark`, `Biomass_BarkWood`.
#'   - `Biomass Root`, `Biomass_Total`.
#'
#' @details
#' ### Volume method
#' If `bark = FALSE`, the function uses the first available volume column among:
#' `V22`, `V22B`, `V22_HA`, `Sawlog`, `Aboveground`, `Merchantable`.
#' The selected volume is multiplied by `ID* 1000` (g/cm^3 to kg/m^3).
#'
#' If `bark = TRUE`, the function uses both `Wood Volume` and `Bark Volume`, which must be present in `x`.
#'
#' ### Equation method
#' Equations are applied species-wise. Models are selected by `A0` value:
#'
#' - `A0 = 15`: linear model using up to 3 predictors (e.g., `Y = b0*X0 + b1*X1 + b2*X2`)
#' - `A0 = 16`: semi-log model (e.g., `Y = b0*X0 + b1*log(X1)`)
#' - `A0 = 17`: power model (e.g., `Y = b0 * X0^b1`)
#'
#' Expressions in `X0:X5` are evaluated dynamically using columns in `x`.
#'
#' If the `Region` column equals `"Belgium"`, results are scaled by `×1000`.
#'
#' @seealso
#' - [calculate_bark_thickness()] to compute `Wood Volume` and `Bark Volume`.
#' - [calculate_carbon()] to convert biomass into carbon stock.
#' - [calculate_volume()] for volume-based biomass estimation.
#'
#' @importFrom stats setNames
#' @export
#'
calculate_biomass <- function(x, equations, method = c("equation", "volume"), bark = FALSE) {
  method <- match.arg(method)

  if (method == "equation") {
    return(calculate_biomass_equation_dynamic(x, equations))
  } else if (method == "volume") {
    return(calculate_biomass_volume_dynamic(x, equations, bark = bark))
  }
}

calculate_biomass_equation_dynamic <- function(x, equations) {
  # Filter valid biomass equations
  eqs_biomass <- equations[
    tolower(equations$Y) == "biomass" &
      equations$A0 %in% c(15, 16, 17, 18, 19, 20, 21),
  ]

  if (nrow(eqs_biomass) == 0) {
    warning("No valid biomass equations found.")
    return(x)
  }

  # Ensure coefficients are numeric
  b_columns <- paste0("b", 0:5)
  eqs_biomass[b_columns] <- lapply(eqs_biomass[b_columns], as.numeric)

  # Prepare output columns
  x[["Biomass Total [kg]"]]  <- NA_real_
  x[["Biomass Root [kg]"]]   <- NA_real_
  biomass_cols   <- list()
  validity_counts <- list()

  # Determine which user vars are used in equations
  user_vars <- setdiff(names(x), c("Species"))
  used_vars <- unique(unlist(
    lapply(eqs_biomass[, paste0("X", 0:5)], function(exprs) {
      sapply(exprs, function(expr) {
        vars <- all.vars(parse(text = expr))
        vars[vars %in% user_vars]
      })
    })
  ))

  # Initialize validity columns and counters
  for (var in used_vars) {
    x[[paste0("Biomass_", var, "_Validity")]] <- NA_character_
    validity_counts[[var]] <- c(VALID=0, BELOW_MIN=0, ABOVE_MAX=0, NO_LIMITS=0, MISSING=0)
  }

  # Split equations by species
  eq_mapping <- split(eqs_biomass, eqs_biomass$Species)

  for (i in seq_len(nrow(x))) {
    sp <- x$Species[i]
    if (is.na(sp) || !(sp %in% names(eq_mapping))) next

    eqs  <- eq_mapping[[sp]]
    tot_biomass_i <- 0

    for (j in seq_len(nrow(eqs))) {
      eq   <- eqs[j, ]
      vars <- list()
      # Always initialize column for this eq type
      type_eq <- if (!is.na(eq$Type_Eq)) eq$Type_Eq else paste0("AB", eq$A0)
      col_base <- paste0("Biomass_", type_eq)
      if (!col_base %in% names(biomass_cols)) {
        biomass_cols[[col_base]] <- rep(NA_real_, nrow(x))
      }

      # Extract variable values for this equation
      for (k in 0:5) {
        expr <- eq[[paste0("X", k)]]
        if (!is.na(expr) && nzchar(expr) && expr != "0") {
          for (vn in all.vars(parse(text = expr))) {
            if (vn %in% user_vars) vars[[vn]] <- x[[vn]][i]
          }
        }
      }

      # Update validity counts for each var
      for (vn in names(vars)) {
        val     <- vars[[vn]]
        col_val <- paste0("Biomass_", vn, "_Validity")
        min_col <- paste0(vn, "_Min")
        max_col <- paste0(vn, "_Max")
        if (is.na(val)) {
          vstat <- "MISSING"
        } else if (all(c(min_col, max_col) %in% names(eq)) &&
                   !is.na(eq[[min_col]]) && !is.na(eq[[max_col]])) {
          vstat <- if (val < eq[[min_col]])      "BELOW MIN"
          else if (val > eq[[max_col]]) "ABOVE MAX"
          else                          "VALID"
        } else {
          vstat <- "NO LIMITS"
        }
        x[[col_val]][i] <- vstat
        if (vstat %in% names(validity_counts[[vn]])) {
          validity_counts[[vn]][vstat] <- validity_counts[[vn]][vstat] + 1
        }
      }

      # Special case A0 = 15
      if (eq$A0 == 15 && all(c("HTOT", "D130") %in% names(vars))) {
        g_ab15 <- eq$b0 + eq$b1 * (vars$HTOT * 100) + eq$b2 * (vars$D130^2)
        kg_ab15 <- g_ab15 / 1000
        biomass_cols[["Biomass_AB15"]][i] <- kg_ab15
        tot_biomass_i <- kg_ab15
        next
      }

      # Other A0 cases including 19
      biomass <- NA_real_
      if (eq$A0 == 16 && !is.na(vars$D130)) {
        biomass <- eq$b0 * (vars$D130 ^ eq$b1)
      } else if (eq$A0 == 17 && !is.na(vars$D130)) {
        biomass <- eq$b0 * ((vars$D130 * 10) ^ eq$b1)
      } else if (eq$A0 == 18 && !is.na(vars$D130)) {
        biomass <- exp(eq$b0 + eq$b1 * log(vars$D130)) / 1000
      } else if (eq$A0 == 20 && !is.na(vars$D130)) {
        biomass <- exp(eq$b0 + eq$b1 * log(vars$D130))
      } else if (eq$A0 == 21 && !is.na(vars$D130)) {
        biomass <- eq$b0 + eq$b1 * log(vars$D130)
      }

      # Store generic biomass
      if (!is.na(biomass)) {
        biomass_cols[[col_base]][i] <- biomass
        tot_biomass_i <- tot_biomass_i + biomass
      }
    }

    # After all eqs for tree i, compute root & total
    if (tot_biomass_i > 0) {
      x[["Biomass Root [kg]"]][i]  <- tot_biomass_i * 0.2
      x[["Biomass Total [kg]"]][i] <- tot_biomass_i * 1.2
    }
  }

  # Bind in all biomass_cols
  for (nm in names(biomass_cols)) {
    x[[nm]] <- biomass_cols[[nm]]
  }

  # Final validity warnings without subscript errors
  get_count <- function(cnts, lvl) if (lvl %in% names(cnts)) cnts[[lvl]] else 0L
  for (vn in names(validity_counts)) {
    cnts    <- validity_counts[[vn]]
    n_below <- get_count(cnts, "BELOW MIN")
    n_above <- get_count(cnts, "ABOVE MAX")
    checked <- sum(cnts)
    if ((n_below + n_above) > 0 && checked > 0) {
      pct <- round(100 * (n_below + n_above) / checked, 1)
      warning(sprintf(
        "VALIDITY DOMAIN WARNING: %s -> %d BELOW MIN, %d ABOVE MAX (%d/%d, %.1f%%)",
        vn, n_below, n_above, n_below + n_above, checked, pct
      ))
    }
  }

  return(x)
}

calculate_biomass_volume_dynamic <- function(x, equations, bark = FALSE) {
  if (!"Species" %in% names(x)) stop("Column 'Species' is missing.")
  if (!"ID" %in% names(equations)) stop("Column 'ID' (infra-density) is missing.")

  id_by_species <- setNames(equations$ID * 1000, equations$Species)

  if (bark) {
    biomass_wood <- rep(NA_real_, nrow(x))
    biomass_bark <- rep(NA_real_, nrow(x))

    for (i in seq_len(nrow(x))) {
      sp       <- x$Species[i]
      id       <- id_by_species[sp]
      wood_vol <- x[["Wood Volume [m^3]"]][i]
      bark_vol <- x[["Bark Volume [m^3]"]][i]
      if (!is.na(id)) {
        if (!is.na(wood_vol)) biomass_wood[i] <- wood_vol * id
        if (!is.na(bark_vol)) biomass_bark[i] <- bark_vol * id
      }
    }

    x[["Biomass Wood  [kg]"]]          <- biomass_wood
    x[["Biomass Bark [kg]"]]          <- biomass_bark
    x[["Biomass Aboveground  [kg]"]]  <- rowSums(cbind(biomass_wood, biomass_bark), na.rm = TRUE)
    x[["Biomass Root [kg]"]]          <- x[["Biomass Aboveground  [kg]"]] * 0.2
    x[["Biomass Total [kg]"]]         <- x[["Biomass Aboveground  [kg]"]] * 1.2

  } else {
    valid_volume_types <- c("V22", "V22B", "V22_HA", "Sawlog", "Aboveground", "Merchantable")
    volume_columns <- intersect(valid_volume_types, names(x))
    if (length(volume_columns) == 0) {
      stop("No valid volume columns found. Expected one of: ", paste(valid_volume_types, collapse = ", "))
    }

    volume_used <- volume_columns[1]
    biomass    <- rep(NA_real_, nrow(x))

    for (i in seq_len(nrow(x))) {
      sp  <- x$Species[i]
      vol <- x[[volume_used]][i]
      id  <- id_by_species[sp]
      if (!is.na(sp) && !is.na(vol) && !is.na(id)) {
        biomass[i] <- vol * id
      }
    }

    col_name <- paste0("Biomass ", volume_used)
    x[[col_name]] <- biomass
    x[["Biomass Root [kg]"]] <- biomass * 0.2
    x[["Biomass Total [kg]"]]      <- biomass * 1.2
  }

  return(x)
}


# =====================================================================
#                         calculate_carbon()
# =====================================================================
#' Carbon calculation from biomass
#' @description Calculates total carbon as 50% of total biomass.
#' @param x A data.frame containing a Biomass_Total column
#' @return The same data.frame enriched with the Carbon Total column
#' @export
#' @examples
#' x <- data.frame(Biomass_Total = c(100, 120, 80))
#' x <- calculate_carbon(x)

calculate_carbon <- function(x) {
  if (!"Biomass Total [kg]" %in% names(x)) {
    warning("Column 'Biomass_Total' is missing. Carbon cannot be calculated.")
    x[["Carbon Total [kg]"]] <- NA_real_
    return(x)
  }

  x[["Carbon Total [kg]"]] <- round(x[["Biomass Total [kg]"]]  * 0.47 ,3)
  return(x)
}


# =====================================================================
#                         calculate_prediction_interval()
# =====================================================================
#' Calculate Relative Prediction Intervals for Tree Volume Estimates
#'
#' @description
#' Computes the relative width of prediction intervals for tree volume estimates based on
#' species-specific allometric equations and their associated uncertainty. The function supports
#' univariate and multivariate models, using provided statistical parameters such as residual
#' standard error (`sigma`), number of observations (`n`), variance estimates (`SCE_*`),
#' and covariances (`COV_*`).
#'
#' The function adds two new columns to the input dataset:
#' \itemize{
#'   \item \code{Relative_Width} - The width of the prediction interval divided by the predicted volume.
#'   \item \code{Reliability} - A qualitative interpretation of the interval's reliability.
#' }
#' Optionally, it prints summary statistics globally and by species.
#'
#' @details
#' For each individual tree, the prediction interval is calculated using:
#'
#' \deqn{
#' \text{Variance} = \sigma^2 \left(1 + \frac{1}{n} + \frac{(x - \bar{x})^2}{SCE} \right)
#' }
#'
#' for univariate models, and using the covariance matrix for multivariate models.
#'
#' The final relative width is computed as:
#'
#' \deqn{
#' \text{Relative Width} = \frac{2 \cdot t_{\alpha/2} \cdot \sqrt{\text{Variance}}}{\hat{Y}}
#' }
#'
#' Where:
#' \itemize{
#'   \item \eqn{t_{\alpha/2}} is the Student's t quantile at the desired confidence level.
#'   \item \eqn{\hat{Y}} is the predicted volume for the individual.
#' }
#'
#' Interpretation is based on thresholds:
#' \itemize{
#'   \item < 0.10 -> "Very narrow -> Very reliable"
#'   \item <= 0.25 -> "Acceptable -> Rather reliable"
#'   \item <= 0.50 -> "Wide -> Uncertain"
#'   \item > 0.50 -> "Very wide -> Risky"
#'   \item NA -> "No calculation"
#' }
#'
#' @param x A \code{data.frame} containing individual tree measurements. Must contain:
#' \itemize{
#'   \item \code{Volume} column with predicted values,
#'   \item \code{Species} column matching the \code{equations} table.
#'   \item All predictor variables used in the equations (e.g., \code{D130}, \code{HTOT}, etc.).
#' }
#' @param equations A \code{data.frame} of allometric model parameters, including:
#' \itemize{
#'   \item Columns: \code{Y}, \code{Species}, \code{Source_Eq}, \code{sigma}, \code{n},
#'   \code{x_mean_*}, \code{SCE_*}, \code{COV_*}, and predictors \code{X1} to \code{X5}.
#' }
#' @param volume_type Character. The type of volume estimate to use (e.g., \code{"V22"}).
#' @param equation_id Integer. Index of the equation to use (default = 1).
#' @param confidence_level Numeric. Confidence level for the interval (default = 0.95).
#' @param source Character. Optional source filter for selecting equations (e.g., \code{"Dagnelie"}).
#' @param summarize Logical. If \code{TRUE}, prints summary statistics globally and per species.
#'
#' @return A \code{data.frame}, same as input \code{x}, with additional columns:
#' \describe{
#'   \item{\code{Relative_Width}}{Numeric vector of relative interval widths.}
#'   \item{\code{Reliability}}{Character vector of qualitative interval interpretations.}
#' }
#'
#' @section Printed Summary (optional):
#' If \code{summarize = TRUE}, the function prints:
#' \itemize{
#'   \item Global statistics: count, mean, median, min, max, standard deviation.
#'   \item Interval interpretation frequency table.
#'   \item Summary by species (count, mean, median, SD).
#'   \item Percentiles: P5, P10, P25, P75, P90, P95.
#' }
#'
#' @examples
#' \dontrun{
#' x <- data.frame(Species = "Fagus_sylvatica", D130 = 32, Volume = 1.5)
#' equations <- data.frame(
#'   Y = "V22",
#'   Species = "Fagus_sylvatica",
#'   sigma = 0.2,
#'   n = 100,
#'   x_mean_D130 = 30,
#'   SCE_D130 = 200,
#'   X1 = "log(D130)",
#'   Source_Eq = "Dagnelie"
#' )
#'
#' result <- calculate_prediction_interval(x, equations, volume_type = "V22", source = "Dagnelie")
#' head(result)
#' }
#'
#' @seealso
#' \code{\link{carbofor}}, \code{\link{calculate_volume}}, \code{\link{calculate_biomass}}
#'
#' @author
#' Antonin Caussin
#'
#' @export

calculate_prediction_interval <- function(x, equations, volume_type, equation_id = 1,
                                          confidence_level = 0.95, source = NULL,
                                          summarize = TRUE) {

  interpret_relative_width <- function(relative_width) {
    sapply(relative_width, function(rw) {
      if (is.na(rw)) "No calculation"
      else if (rw < 0.10) "Very reliable"
      else if (rw <= 0.25) "Reliable"
      else if (rw <= 0.50) "Uncertain"
      else "Risky"
    })
  }

  summarize_relative_intervals <- function(relative_widths, x) {
    valid_widths <- relative_widths[!is.na(relative_widths)]
    if (length(valid_widths) == 0) return()

    interpretations <- interpret_relative_width(relative_widths)
    interpretation_table <- table(interpretations[!is.na(relative_widths)])

    if ("Species" %in% names(x)) {
      valid_indices <- which(!is.na(relative_widths))
      species_data <- data.frame(
        Species = x$Species[valid_indices],
        Relative_Width = valid_widths
      )

      species_summary <- aggregate(Relative_Width ~ Species, data = species_data,
                                   FUN = function(x) c(n = length(x), mean = mean(x), median = median(x), sd = sd(x)))
    }
  }

  eqs_volume <- equations[equations$Y == volume_type, ]
  if (!is.null(source)) eqs_volume <- eqs_volume[eqs_volume$Source_Eq == source, ]
  relative_width <- rep(NA_real_, nrow(x))

  for (i in seq_len(nrow(x))) {
    tree_species <- x$Species[i]
    eq_candidates <- eqs_volume[eqs_volume$Species == tree_species, ]
    if (nrow(eq_candidates) == 0) {
      warning(paste("No equation found for species:", tree_species))
      next
    }

    local_equation_id <- min(equation_id, nrow(eq_candidates))
    eq <- eq_candidates[local_equation_id, , drop = FALSE]

    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(lapply(exprs, function(expr) {
      tryCatch(all.vars(parse(text = expr)),
               error = function(e) regmatches(expr, gregexpr("\\b[A-Za-z_][A-Za-z0-9_]*\\b", expr))[[1]])
    })))

    missing_vars <- setdiff(vars_needed, names(x))
    if (length(missing_vars) > 0) {
      warning(paste("Missing variables in dataset:", paste(missing_vars, collapse = ", "), "for species:", tree_species))
      next
    }

    xi_values <- lapply(vars_needed, function(v) x[[v]][i])
    names(xi_values) <- vars_needed
    if (any(sapply(xi_values, function(val) is.na(val) || !is.finite(val)))) {
      warning(paste("Invalid predictor value(s) for row", i))
      next
    }

    n_params <- length(vars_needed)

    if (n_params == 1) {
      var <- vars_needed[1]
      x_mean_col <- paste0("x_mean_", var)
      sce_col <- paste0("SCE_", var)
      cols_required <- c("sigma", "n", x_mean_col, sce_col)
      missing_cols <- cols_required[!cols_required %in% names(eq)]
      if (length(missing_cols) > 0) {
        warning(paste("Missing required columns for univariate model of species:", tree_species))
        next
      }

      sigma <- eq$sigma[1]
      n <- eq$n[1]
      x_mean <- eq[[x_mean_col]][1]
      SCEx <- eq[[sce_col]][1]
      xi <- xi_values[[var]]

      if (any(is.na(c(sigma, n, x_mean, SCEx)))) {
        warning(paste("Missing parameters for univariate prediction of species:", tree_species))
        next
      }

      variance_pred <- sigma^2 * (1 + 1/n + (xi - x_mean)^2 / SCEx)

    } else {
      sigma <- eq$sigma[1]
      n <- eq$n[1]
      if (is.na(sigma) || is.na(n)) {
        warning(paste("Missing sigma or n for multivariate model of species:", tree_species))
        next
      }

      x_diff <- numeric(n_params)
      cov_matrix_inv <- matrix(0, nrow = n_params, ncol = n_params)

      for (j in seq_len(n_params)) {
        var_j <- vars_needed[j]
        mean_col <- paste0("x_mean_", var_j)
        sce_col <- paste0("SCE_", var_j)
        if (!mean_col %in% names(eq) || is.na(eq[[mean_col]][1])) {
          warning(paste("Missing", mean_col, "for species:", tree_species))
          next
        }
        x_mean_j <- eq[[mean_col]][1]
        x_diff[j] <- xi_values[[var_j]] - x_mean_j

        if (!sce_col %in% names(eq) || is.na(eq[[sce_col]][1]) || eq[[sce_col]][1] == 0) {
          warning(paste("Missing or invalid", sce_col, "for species:", tree_species))
          next
        }
        cov_matrix_inv[j, j] <- 1 / eq[[sce_col]][1]
      }

      for (j in seq_len(n_params)) {
        for (k in seq_len(n_params)) {
          if (j != k) {
            var_names <- sort(c(vars_needed[j], vars_needed[k]))
            cov_col <- paste0("COV_", var_names[1], "_", var_names[2])
            if (cov_col %in% names(eq) && !is.na(eq[[cov_col]][1])) {
              cov_val <- eq[[cov_col]][1]
              cov_matrix_inv[j, k] <- cov_val
              cov_matrix_inv[k, j] <- cov_val
            }
          }
        }
      }

      quadratic_form <- tryCatch(
        as.numeric(t(x_diff) %*% cov_matrix_inv %*% x_diff),
        error = function(e) sum(x_diff^2 * diag(cov_matrix_inv))
      )
      variance_pred <- sigma^2 * (1 + 1/n + quadratic_form)
    }

    t_val <- qt(1 - (1 - confidence_level)/2, df = n - 2)

    if (volume_type %in% names(x)) {
      volume_col <- volume_type
    } else {
      alt <- paste0(volume_type, " [m^3]")
      if (alt %in% names(x)) {
        volume_col <- alt
      }
    }

    if (!volume_col %in% names(x)) {
      warning(paste("Column", volume_col, "not found in input data. Skipping row", i))
      next
    }
    volume_pred <- x[[volume_col]][i]

    if (isTRUE(!is.na(volume_pred) && !is.na(variance_pred) &&
               is.finite(volume_pred) && is.finite(variance_pred) &&
               volume_pred > 0 && variance_pred > 0)) {
      margin_error <- t_val * sqrt(variance_pred)
      relative_width[i] <- 2 * margin_error / volume_pred
    }
  }

  x[["Relative Width"]] <- round(relative_width,2)
  x[["Reliability"]] <- interpret_relative_width(relative_width)
  if (summarize) summarize_relative_intervals(relative_width, x)
  return(x)
}

