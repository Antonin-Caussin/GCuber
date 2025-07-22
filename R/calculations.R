# =====================================================================
#                         calculate_volume()
# =====================================================================
#' Volume calculation from allometric equations
#' @description Applies species-specific allometric equations to calculate volume.
#' @param x A data.frame containing individual tree data (must include at least Species and diameter column)
#' @param volume_type Type of volume to calculate (e.g., "V22", "V22B", "V22_HA", "E")
#' @param equations A data.frame of allometric equations including: Species, A0, b0 to b5, X1 to X5, D_Min, D_Max, Y
#' @param equation_id Index of the equation to use for each species (default is 1)
#' @param D130 Name of the diameter variable to use (default = "D130")
#' @param specimens A vector of specimen codes or identifiers, used to map equations to observations
#' @param source Source of the equations (e.g., "Dagnelie", "Myers")
#' @return The data.frame enriched with columns: [volume_type], Validity_Status, and Equation_Used
#' @export
#' @examples
#' trees <- data.frame(Species = "Fagus_sylvatica", D130 = 32)
#' specimens <- c("FASY001")
#' result <- calculate_volume(trees, equations = equations, specimens = specimens)

calculate_volume <- function(x, volume_type = "V22", equations, equation_id = 1,
                             D130 = "D130", specimens = NULL, source = "Dagnellie") {

  equations <- equations
  trees_checked <- 0
  trees_outside_domain <- 0
  trees_no_validity_limits <- 0
  trees_below_min <- 0
  trees_above_max <- 0

  if (!volume_type %in% names(x)) x[[volume_type]] <- NA_real_
  if (!"Validity_Status" %in% names(x)) x$Validity_Status <- NA_character_
  if (!"Equation_Used" %in% names(x)) x$Equation_Used <- NA_character_

  eqs_volume <- equations[equations$Y == volume_type, ]
  if (!is.null(source)) {
    eqs_volume <- eqs_volume[eqs_volume$Source_Eq == source, ]
  }


  for (i in seq_len(nrow(x))) {
    tree_species <- x$Species[i]
    DHB_value <- x[[D130]][i]

    volume_res <- 0
    local_equation_id <- equation_id
    eq_candidates <- eqs_volume[eqs_volume$Species == tree_species, ]

    if (nrow(eq_candidates) == 0) {
      warning(paste("No equation found for species:", tree_species))
      x$Validity_Status[i] <- "NO_EQUATION"
      next
    }

    if (local_equation_id > nrow(eq_candidates)) {
      warning(paste("Equation with id", local_equation_id, "does not exist for species", tree_species,
                    ". Using equation 1 instead."))
      eq <- eq_candidates[1, , drop = FALSE]
    } else {
      eq <- eq_candidates[local_equation_id, , drop = FALSE]
    }

    x$Equation_Used[i] <- paste0(eq$Species, ":", eq$Y, ":A0=", eq$A0)

    validity_status <- "VALID"
    if (!is.na(DHB_value)) {
      trees_checked <- trees_checked + 1

      if (!is.na(eq$D130_Min[1]) && !is.na(eq$D130_Max[1])) {
        D_Min <- eq$D130_Min[1]
        D_Max <- eq$D130_Max[1]

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
        trees_no_validity_limits <- trees_no_validity_limits + 1
        validity_status <- "NO_LIMITS"
      }
    }
    x$Validity_Status[i] <- validity_status

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

    missing_vars <- names(variables)[sapply(variables, function(v) is.na(v) | !is.finite(v))]
    if (length(missing_vars) > 0) {
      warning(paste("Missing or invalid input variables at row", i, ":", paste(missing_vars, collapse = ", ")))
      x$Validity_Status[i] <- "MISSING_INPUT"
      next
    }

    a0_value <- eq$A0[1]

    if (is.na(a0_value)) {
      warning(paste("Missing A0 value for species", tree_species, "at row", i))
      next
    } else if (a0_value %in% c(1, 2, 3)) {
      volume_res <- eq$b0[1]
      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)

        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          if (eq[[x_col]][1] == "/") {
            warning(paste("Invalid expression at row", i, "for X", j))
            next
          }

          x_val <- tryCatch({
            evaluate_expression(eq[[x_col]][1], variables)
          }, error = function(e) {
            warning(paste("Error during evaluation of X", j, ":", e$message))
            NA
          })

          if (length(x_val) == 0 || is.na(x_val)) {
            warning(paste("Evaluation of X", j, "failed at row", i))
            next
          }

          b_val <- eq[[b_col]][1]
          if (is.na(b_val)) {
            warning(paste("Missing coefficient b", j, "at row", i))
            next
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

    x[[volume_type]][i] <- volume_res
  }

  if (trees_outside_domain > 0) {
    percentage_outside <- round((trees_outside_domain / trees_checked) * 100, 1)
    warning(paste0("VALIDITY DOMAIN WARNING: ", trees_outside_domain, " trees (",
                   percentage_outside, "%) have DHB values outside the validity domain. ",
                   trees_below_min, " trees below D_Min and ",
                   trees_above_max, " trees above D_Max."))
  }

  return(x)
}

# =====================================================================
#                      calculate_bark_thickness()
# =====================================================================
#' Bark thickness and bark volume calculation
#'
#' @description
#' Uses species-specific allometric equations (Y = "E") to estimate bark thickness,
#' then computes bark volume and net wood volume from total volume.
#'
#' @param df A data.frame containing tree-level data. Must include at least Species, D130, and a total volume column.
#' @param equations A data.frame containing bark thickness equations (Y = "E") with columns:
#' Species, A0, b0 to b5, X1 to X5, and optionally Source_Eq.
#' @param total_volume_col Name of the column containing the total volume (default = "V22").
#' @param source Optional character string specifying the source of the equations (e.g., "Vallet", "Dagnellie").
#' If NULL, all sources are considered.
#'
#' @return The input data.frame enriched with the columns:
#' \item{E}{Estimated bark thickness (cm)}
#' \item{Bark_Volume}{Volume of bark (same unit as total_volume_col)}
#' \item{Wood_Volume}{Net wood volume (total - bark)}
#'
#' @export
#'
#' @examples
#' df <- data.frame(Species = "Hetre", D130 = 32, V22 = 1.2)
#' result <- calculate_bark_thickness(df, equations = equations, source = "Vallet")

calculate_bark_thickness <- function(df, equations, total_volume_col = "V22", source) {
  if (!exists("equations")) stop("The 'equations' data.frame must be loaded in the global environment.")

  bark_eqs <- equations[equations$Y == "E", ]
  if (!is.null(source) && "Source_Eq" %in% names(bark_eqs)) {
    bark_eqs <- bark_eqs[bark_eqs$Source_Eq == source, ]
  }

  if (nrow(bark_eqs) == 0) {
    warning("No bark equations (type E) found for the selected source.")
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
      if (!is.null(source) && source == "Vallet") {
        # Vallet fournit directement un ratio surfacique
        bark_ratio <- thickness
      } else {
        # Épaisseur classique à convertir en volume
        r_ext <- df$D130[i] / 2
        r_int <- r_ext - thickness
        if (r_int <= 0) next
        bark_ratio <- (r_ext^3 - r_int^3) / r_ext^3
      }

      bark_vol <- total_vol * bark_ratio
      wood_vol <- total_vol - bark_vol
      df$E[i] <- thickness
      df$Bark_Volume[i] <- bark_vol
      df$Wood_Volume[i] <- wood_vol
    }
  }

  return(df)
}




# =====================================================================
#                           calculate_biomass()
# =====================================================================
#' Calculate biomass using allometric equations or volume × infra-density
#'
#' @description
#' This function computes tree-level aboveground biomass (and derived root and total biomass)
#' using one of two approaches:
#'
#' - **"equation" method**: applies species-specific allometric models provided in a reference table.
#' - **"volume" method**: multiplies measured or estimated tree volume by species-specific wood infra-density (`ID`).
#'
#' The function supports flexible model structures (including log-linear and additive forms),
#' automatic species matching, and fallback options when data are incomplete.
#'
#' @param x A `data.frame` containing individual tree data. It must include:
#'   - A `Species` column for species identification.
#'   - Either volume columns (see `Details`) or measurement variables (e.g., diameter, height) used in allometric equations.
#'
#' @param equations A `data.frame` containing model parameters for each species. Depending on the method:
#'   - For `"equation"`: must include columns `Species`, `Y`, `A0`, `b0`–`b5`, and `X1`–`X5`.
#'   - For `"volume"`: must include columns `Species` and `ID` (infra-density in g/cm³).
#'
#' @param method Character. Biomass estimation method. Either:
#'   - `"equation"` (uses species-specific allometric models),
#'   - `"volume"` (uses volume × infra-density). Default is `"volume"`.
#'
#' @return A modified `data.frame` identical to `x` with three new columns:
#' \describe{
#'   \item{Biomass_Aboveground}{Estimated aboveground biomass (in consistent volume × density units).}
#'   \item{Biomass_Root}{Estimated root biomass, computed as 20\% of the aboveground biomass.}
#'   \item{Biomass_Total}{Total biomass, computed as 120\% of the aboveground biomass.}
#' }
#'
#' If `method = "equation"`, additional columns may be added per equation used
#' (e.g., `<Species>_Biomass_A0<EqType>_Eq<Index>`) along with metadata columns identifying the equation applied.
#'
#' @details
#' When `method = "volume"`, the function searches for volume columns in `x` whose names match one or more of the following:
#' `"V22"`, `"V22B"`, `"V22_HA"`, `"Sawlog"`, `"Aboveground"`, `"Merchantable"`. These columns are summed to compute total volume.
#'
#' When `method = "equation"`, the function evaluates the model based on the value of `A0`:
#' \itemize{
#'   \item A0 = 1–3 or 5: additive linear form: \eqn{Y = b0 + b1*X1 + ... + b5*X5}
#'   \item A0 = 4: log-linear form: \eqn{log10(Y) = b0 + b1*log10(X1) + ...}
#' }
#'
#' Expressions in `X1:X5` are evaluated dynamically, and variable names must match column names in `x`.
#'
#' @section Limitations:
#' - The "volume" method assumes that volume data and wood density are consistent in units (e.g., m³ × g/cm³ → kg).
#' - If multiple equations exist per species in `"equation"` mode, all are applied and their results are summed. This assumes they target complementary compartments.
#' - No biomass is calculated for species missing in the `equations` table.
#'
#' @seealso
#' \code{\link{calculate_carbon}} to compute carbon stock from biomass,
#' \code{\link{calculate_volume}} for upstream volume computation,
#' To access the equations, see the internal object `equations` bundled with the package.
#'
#' @examples
#' # Using volume × density method
#' df <- data.frame(Species = c("Fagus", "Quercus"),
#'                  V22 = c(0.8, 1.2))
#' eq <- data.frame(Species = c("Fagus", "Quercus"), ID = c(0.56, 0.65))
#' calculate_biomass(df, eq, method = "volume")
#'
#' # Using allometric equations
#' eqs <- data.frame(Species = "Fagus", Y = "BIOMASS", A0 = 1,
#'                   b0 = 0.2, b1 = 0.03, b2 = NA, b3 = NA, b4 = NA, b5 = NA,
#'                   X1 = "D130", X2 = NA, X3 = NA, X4 = NA, X5 = NA)
#' df <- data.frame(Species = "Fagus", D130 = 32)
#' calculate_biomass(df, eqs, method = "equation")
#'
#' @importFrom stats setNames
#' @export


calculate_biomass <- function(x, equations, method = c("equation", "volume")) {
  method <- match.arg(method)

  if (method == "equation") {
    return(calculate_biomass_equation(x, equations))
  } else if (method == "volume") {
    return(calculate_biomass_volume(x, equations))
  }
}

# Internal method: biomass by equation
calculate_biomass_equation <- function(x, equations) {
  eqs_biomass <- equations[equations$Y == "BIOMASS", ]
  if (nrow(eqs_biomass) == 0) {
    warning("No biomass equations found in equations")
    return(x)
  }

  b_columns <- paste0("b", 0:5)
  eqs_biomass[b_columns] <- lapply(eqs_biomass[b_columns], as.numeric)
  species_with_eqs <- unique(eqs_biomass$Species)

  eq_mapping <- list()
  for (species in species_with_eqs) {
    eq_mapping[[species]] <- eqs_biomass[eqs_biomass$Species == species, ]
  }

  x$Biomass_Aboveground <- NA_real_
  x$Biomass_Root <- NA_real_
  x$Biomass_Total <- NA_real_

  for (i in seq_len(nrow(x))) {
    sp <- x$Species[i]
    biomass_total <- 0

    if (!is.na(sp) && sp %in% names(eq_mapping)) {
      eqs <- eq_mapping[[sp]]
      for (j in seq_len(nrow(eqs))) {
        eq <- eqs[j, ]
        vars <- list()
        for (k in 1:5) {
          xname <- eq[[paste0("X", k)]]
          if (!is.na(xname) && xname != "0" && xname %in% names(x)) {
            vars[[xname]] <- x[[xname]][i]
          }
        }

        biomass <- eq$b0
        if (eq$A0 %in% c(1, 2, 3, 5)) {
          for (k in 1:5) {
            xexpr <- eq[[paste0("X", k)]]
            bval <- eq[[paste0("b", k)]]
            if (!is.na(xexpr) && xexpr != "0" && !is.na(bval)) {
              val <- evaluate_expression(xexpr, vars)
              if (!is.na(val)) {
                biomass <- biomass + bval * val
              }
            }
          }
        } else if (eq$A0 == 4) {
          log_sum <- eq$b0
          for (k in 1:5) {
            xexpr <- eq[[paste0("X", k)]]
            bval <- eq[[paste0("b", k)]]
            if (!is.na(xexpr) && xexpr != "0" && !is.na(bval)) {
              val <- evaluate_expression(xexpr, vars)
              if (!is.na(val) && val > 0) {
                log_sum <- log_sum + bval * log10(val)
              } else if (!is.na(val) && val <= 0) {
                warning(paste("Cannot calculate log of non-positive value:", val, "at row", i))
                log_sum <- NA
                break
              }
            }
          }
          if (!is.na(log_sum)) {
            biomass <- 10^log_sum
          } else {
            biomass <- NA
          }
        }

        if (!is.na(biomass)) {
          biomass_total <- biomass_total + biomass
        }
      }

      if (!is.na(biomass_total) && biomass_total > 0) {
        x$Biomass_Aboveground[i] <- biomass_total
        x$Biomass_Root[i] <- biomass_total * 0.2
        x$Biomass_Total[i] <- biomass_total * 1.2
      }
    }
  }

  return(x)
}

# Internal method: biomass by volume × infra-density
calculate_biomass_volume <- function(x, equations) {
  if (!"Species" %in% names(x)) {
    stop("Column 'Species' is missing in input data.")
  }

  if (!"ID" %in% names(equations)) {
    stop("Column 'ID' (infra-density) is missing in equations data.")
  }

  valid_volume_types <- c("V22", "V22B", "V22_HA", "Sawlog", "Aboveground", "Merchantable")
  volume_columns <- intersect(valid_volume_types, names(x))

  if (length(volume_columns) == 0) {
    stop("No valid volume columns found in input data. Expected one or more of: ",
         paste(valid_volume_types, collapse = ", "))
  }

  id_by_species <- setNames(equations$ID, equations$Species)
  biomass_aboveground <- rep(NA_real_, nrow(x))

  for (i in seq_len(nrow(x))) {
    species <- x$Species[i]
    vols <- sum(as.numeric(x[i, volume_columns]), na.rm = TRUE)

    if (!is.na(species) && species %in% names(id_by_species)) {
      density <- id_by_species[[species]]
      if (!is.na(vols) && !is.na(density)) {
        biomass_aboveground[i] <- vols * density
      }
    }
  }

  x$Biomass_Aboveground <- biomass_aboveground
  x$Biomass_Root <- biomass_aboveground * 0.2
  x$Biomass_Total <- biomass_aboveground * 1.2

  return(x)
}



# =====================================================================
#                         calculate_carbon()
# =====================================================================
#' Carbon calculation from biomass
#' @description Calculates total carbon as 50% of total biomass.
#' @param df A data.frame containing a Biomass_Total column
#' @return The same data.frame enriched with the Carbon_Total column
#' @export
#' @examples
#' df <- data.frame(Biomass_Total = c(100, 120, 80))
#' df <- calculate_carbon(df)

calculate_carbon <- function(df) {
  if (!"Biomass_Total" %in% names(df)) {
    warning("Column 'Biomass_Total' is missing. Carbon cannot be calculated.")
    df$Carbon_Total <- NA_real_
    return(df)
  }

  df$Carbon_Total <- df$Biomass_Total * 0.47
  return(df)
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
#'   \item \code{Relative_Width} — The width of the prediction interval divided by the predicted volume.
#'   \item \code{Reliability} — A qualitative interpretation of the interval's reliability.
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
#'   \item < 0.10 → "Very narrow → Very reliable"
#'   \item ≤ 0.25 → "Acceptable → Rather reliable"
#'   \item ≤ 0.50 → "Wide → Uncertain"
#'   \item > 0.50 → "Very wide → Risky"
#'   \item NA → "No calculation"
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
#' @param source Character. Optional source filter for selecting equations (e.g., \code{"Dagnellie"}).
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
#' df <- data.frame(Species = "Fagus_sylvatica", D130 = 32, Volume = 1.5)
#' equations <- data.frame(
#'   Y = "V22",
#'   Species = "Fagus_sylvatica",
#'   sigma = 0.2,
#'   n = 100,
#'   x_mean_D130 = 30,
#'   SCE_D130 = 200,
#'   X1 = "log(D130)",
#'   Source_Eq = "Dagnellie"
#' )
#'
#' result <- calculate_prediction_interval(df, equations, volume_type = "V22", source = "Dagnellie")
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
      else if (rw < 0.10) "Very narrow -> Very reliable"
      else if (rw <= 0.25) "Acceptable -> Rather reliable"
      else if (rw <= 0.50) "Wide -> Uncertain"
      else "Very wide -> Risky"
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
    volume_col <- volume_type
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

  x$Relative_Width <- relative_width
  x$Reliability <- interpret_relative_width(relative_width)
  if (summarize) summarize_relative_intervals(relative_width, x)
  return(x)
}
