#' Calcul du volume à partir des équations allométriques
#' @description Applique les équations allométriques spécifiques à chaque espèce pour calculer le volume.
#' @param df Le data.frame contenant les données d'arbres
#' @param volume_type Type de volume à calculer (ex: "V22", "V22B", "V22_HA", "E")
#' @param equation_id Identifiant de l'équation à utiliser
#' @param source Source des équations (ex: "Dagnellie")
#' @return Le data.frame enrichi avec les colonnes de volume, validité et équation utilisée
#' @export
calculate_volume <- function(df, volume_type = "V22", equation_id = 1, source = "Dagnellie") {
  if (!exists("equations")) stop("Le data.frame 'equations' doit être chargé dans l'environnement global.")

  df_result <- df
  df_result$Equation_Used <- NA_character_
  df_result$Validity_Status <- NA_character_

  if (!volume_type %in% names(df_result)) {
    df_result[[volume_type]] <- NA_real_
  }

  eqs_volume <- equations[equations$Y == volume_type & equations$Source_Eq == source, ]
  if (nrow(eqs_volume) == 0) stop(paste("Aucune équation trouvée pour le type de volume :", volume_type))

  for (i in seq_len(nrow(df_result))) {
    sp <- df_result$Species[i]
    eqs_sp <- eqs_volume[eqs_volume$Species == sp, ]
    if (nrow(eqs_sp) == 0) {
      df_result$Validity_Status[i] <- "NO_EQUATION"
      next
    }

    eq <- if (nrow(eqs_sp) >= equation_id) eqs_sp[equation_id, ] else eqs_sp[1, ]
    df_result$Equation_Used[i] <- paste0(eq$Species, ":", eq$Y, ":A0=", eq$A0)

    # Vérification de validité
    d <- df_result$D130[i]
    if (isTRUE(!is.na(d)) && isTRUE(!is.na(eq$D_Min)) && isTRUE(!is.na(eq$D_Max))) {
      if (d < eq$D_Min) df_result$Validity_Status[i] <- "BELOW_MIN"
      else if (d > eq$D_Max) df_result$Validity_Status[i] <- "ABOVE_MAX"
      else df_result$Validity_Status[i] <- "VALID"
    } else {
      df_result$Validity_Status[i] <- "NO_LIMITS"
    }

    # Évaluation de l'équation
    vars <- list()
    for (v in c("C130", "C150", "D130", "D150", "HTOT", "HDOM", "G130", "G150")) {
      if (v %in% names(df_result)) vars[[v]] <- df_result[[v]][i]
    }

    a0 <- eq$A0
    if (a0 %in% c(1, 2, 3, 5)) {
      res <- eq$b0
      for (j in 1:5) {
        xj <- eq[[paste0("X", j)]]
        bj <- eq[[paste0("b", j)]]
        if (!is.na(xj) && xj != "0") {
          val <- evaluate_expression(xj, vars)
          if (!is.na(val)) res <- res + bj * val
        }
      }
    } else if (a0 == 4) {
      x1 <- eq$X1
      b0 <- eq$b0
      b1 <- eq$b1
      val <- evaluate_expression(x1, vars)
      res <- if (!is.na(val) && val > 0) 10^(b0 + b1 * log10(val)) else NA
    } else {
      res <- NA
    }

    df_result[[volume_type]][i] <- ifelse(is.finite(res), res, NA)
  }

  return(df_result)
}


#' Calcul de l'épaisseur et du volume d'écorce
#' @description Utilise les équations spécifiques (type E) pour estimer l'épaisseur d'écorce, puis calcule le volume d'écorce et le volume de bois net.
#' @param df Le data.frame contenant les données d'arbres
#' @param total_volume_col Nom de la colonne contenant le volume total (ex: "V22")
#' @return Le data.frame enrichi avec les colonnes E, Bark_Volume et Wood_Volume
#' @export
calculate_bark_thickness <- function(df, total_volume_col = "V22") {
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
calculate_biomass <- function(df) {
  if (!exists("equations")) stop("Le data.frame 'equations' doit être chargé dans l'environnement global.")
  eqs_biomass <- equations[equations$Y == "BIOMASS", ]
  if (nrow(eqs_biomass) == 0) {
    warning("Aucune équation de biomasse trouvée.")
    return(df)
  }

  df$Biomass_Total <- NA_real_
  df$Carbon_Total <- NA_real_

  for (i in seq_len(nrow(df))) {
    sp <- df$Species[i]
    eqs_sp <- eqs_biomass[eqs_biomass$Species == sp, ]
    if (nrow(eqs_sp) == 0) next

    total_biomass <- 0
    for (j in seq_len(nrow(eqs_sp))) {
      eq <- eqs_sp[j, ]
      vars <- list()
      for (v in c("C130", "C150", "D130", "D150", "HTOT", "HDOM", "G130", "G150")) {
        if (v %in% names(df)) vars[[v]] <- df[[v]][i]
      }

      a0 <- eq$A0
      if (a0 %in% c(1, 2, 3, 5)) {
        res <- eq$b0
        for (k in 1:5) {
          xk <- eq[[paste0("X", k)]]
          bk <- eq[[paste0("b", k)]]
          if (!is.na(xk) && xk != "0") {
            val <- evaluate_expression(xk, vars)
            if (!is.na(val)) res <- res + bk * val
          }
        }
      } else if (a0 == 4) {
        x1 <- eq$X1
        b0 <- eq$b0
        b1 <- eq$b1
        val <- evaluate_expression(x1, vars)
        res <- if (!is.na(val) && val > 0) 10^(b0 + b1 * log10(val)) else NA
      } else {
        res <- NA
      }

      if (!is.na(res) && is.finite(res)) {
        total_biomass <- total_biomass + res
      }
    }

    df$Biomass_Total[i] <- total_biomass
    df$Carbon_Total[i] <- total_biomass * 0.5
  }

  return(df)
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
calculate_individual_variance <- function(df, volume_type = "V22", equation_id = 1) {
  if (!exists("equations")) stop("Le data.frame 'equations' doit être chargé dans l'environnement global.")
  eqs_volume <- equations[equations$Y == volume_type, ]
  if (nrow(eqs_volume) == 0) {
    warning("Aucune équation de volume trouvée pour le type spécifié.")
    df$Relative_Interval_Width <- NA_real_
    df$Interval_Interpretation <- "No equation"
    return(df)
  }

  df$Relative_Interval_Width <- NA_real_
  df$Interval_Interpretation <- NA_character_

  for (i in seq_len(nrow(df))) {
    sp <- df$Species[i]
    vol <- df[[volume_type]][i]
    eqs_sp <- eqs_volume[eqs_volume$Species == sp, ]
    if (nrow(eqs_sp) == 0 || is.na(vol) || vol <= 0) next

    eq <- if (nrow(eqs_sp) >= equation_id) eqs_sp[equation_id, ] else eqs_sp[1, ]
    sigma <- eq$sigma
    n <- eq$n
    if (is.na(sigma) || is.na(n)) next

    # Calcul de la variance prédictive
    rel_width <- NA_real_
    if (!is.na(eq$SCE_C130) && !is.na(eq$x_mean_C130) && "C130" %in% names(df)) {
      xi <- df$C130[i]
      if (!is.na(xi)) {
        var_pred <- sigma^2 * (1 + 1/n + (xi - eq$x_mean_C130)^2 / eq$SCE_C130)
        t_val <- qt(0.975, df = n - 2)
        interval_width <- 2 * t_val * sqrt(var_pred)
        rel_width <- interval_width / vol
      }
    }

    df$Relative_Interval_Width[i] <- rel_width

    # Interprétation
    df$Interval_Interpretation[i] <- if (is.na(rel_width)) {
      "No calculation"
    } else if (rel_width < 0.10) {
      "Very narrow → Very reliable"
    } else if (rel_width <= 0.25) {
      "Acceptable → Rather reliable"
    } else if (rel_width <= 0.50) {
      "Wide → Uncertain"
    } else {
      "Very wide → Risky"
    }
  }

  return(df)
}


