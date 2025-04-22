#' Calcule les volumes forestiers selon differentes methodes
#'
#' @param df Un data frame contenant les donnees des arbres
#' @param type_volume Type de volume à calculer (ex: "VC22", "VC22B", "Ecorce")
#' @param essence Essence d'arbre specifique (ou NULL pour utiliser celle des donnees)
#' @param equations_df Le data frame contenant les equations allometriques
#' @param id_equation Indice de l'equation à utiliser si plusieurs sont disponibles (par defaut = 1)
#' @param coefs_conversion (Optionnel) Data frame de conversion de C150 vers C130
#' @return Un data frame avec les donnees d'entree plus une colonne de volume calcule
#' @export

calculer_volumes <- function(df, type_volume = "VC22", essence = NULL,
                             equations_df = equations, id_equation = 1,
                             coefs_conversion = NULL) {

  # Verification des colonnes minimales requises
  if (!all(c("Essence") %in% colnames(df))) {
    stop("Le fichier doit contenir au moins la colonne 'Essence'")
  }

  # Verification de la présence de C130 ou C150
  if (!any(c("C130", "C150") %in% colnames(df))) {
    stop("Le fichier doit contenir au moins une colonne 'C130' ou 'C150'")
  }

  # Conversion C150 -> C130 si necessaire
  if (!"C130" %in% colnames(df) && "C150" %in% colnames(df)) {
    if (is.null(coefs_conversion)) {
      stop("Le tableau 'coefs_conversion' est requis pour convertir C150 en C130.")
    }

    df <- merge(df, coefs_conversion, by = "Essence", all.x = TRUE)

    if (any(is.na(df$Coef_C150_C130))) {
      stop("Coefficient de conversion manquant pour certaines essences.")
    }

    df$C130 <- df$C150 * df$Coef_C150_C130
  }

  # Calcul des surfaces terrières si nécessaires
  if (!"G130" %in% colnames(df) && "C130" %in% colnames(df)) {
    df$G130 <- (df$C130^2) / (4 * pi)
  }

  if (!"G150" %in% colnames(df) && "C150" %in% colnames(df)) {
    df$G150 <- (df$C150^2) / (4 * pi)
  }

  # Filtrer les equations selon le type de volume
  eqs_volume <- equations_df[equations_df$Y == type_volume, ]

  if (nrow(eqs_volume) == 0) {
    stop(paste("Aucune equation trouvee pour le type de volume:", type_volume))
  }

  # Créer une colonne pour stocker les équations utilisées (pour débogage)
  df$Equation_Utilisee <- NA_character_

  # Verifie si certaines equations necessitent HDOM
  variables_requises <- unique(unlist(sapply(1:5, function(i) {
    var_col <- paste0("X", i)
    unique(unlist(regmatches(as.character(eqs_volume[[var_col]]),
                             gregexpr("[A-Za-z_][A-Za-z0-9_]*", as.character(eqs_volume[[var_col]])))))
  })))

  for (var in variables_requises) {
    if (var != "0" && !(var %in% colnames(df))) {
      stop(paste("La variable", var, "est requise par certaines équations mais absente des données."))
    }
  }

  # Fonction d'evaluation dynamique plus sécurisée
  evaluer_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
    # Créer un environnement isolé avec uniquement les variables nécessaires
    env <- list2env(variables)
    tryCatch({
      eval(parse(text = expr_text), envir = env)
    }, error = function(e) {
      stop(paste("Erreur lors de l'évaluation de l'expression:", expr_text, "-", e$message))
    })
  }

  # Initialisation du resultat
  df$Volume <- NA_real_

  # Traitement de chaque arbre
  for (i in seq_len(nrow(df))) {
    essence_arbre <- df$Essence[i]

    # Sélection des équations pertinentes
    if (!is.null(essence)) {
      # Si une essence spécifique est demandée
      eq_candidates <- eqs_volume[eqs_volume$Essences == essence, ]
    } else {
      # Utiliser l'essence de l'arbre
      eq_candidates <- eqs_volume[eqs_volume$Essences == essence_arbre, ]

      # Si aucune équation pour cette essence, utiliser l'équation générale
      if (nrow(eq_candidates) == 0) {
        eq_candidates <- eqs_volume[eqs_volume$Essences == "General", ]
      }
    }

    if (nrow(eq_candidates) == 0) {
      warning(paste("Pas d'equation trouvee pour l'essence:", essence_arbre))
      next
    }

    # Sélectionner l'équation selon id_equation
    if (nrow(eq_candidates) < id_equation) {
      stop(paste("id_equation =", id_equation, "dépasse le nombre d'équations disponibles (",
                 nrow(eq_candidates), ") pour", essence_arbre))
    }

    # Sélectionner une seule équation
    eq <- eq_candidates[id_equation, , drop = FALSE]

    # Enregistrer l'équation utilisée
    df$Equation_Utilisee[i] <- paste0(eq$Essences, ":", eq$Y, ":A0=", eq$A0)

    # Préparation des variables nécessaires pour cette équation
    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(df)) {
        variables[[v]] <- df[[v]][i]
      } else {
        stop(paste("La variable", v, "est utilisée dans une équation mais absente des données."))
      }
    }

    # Calcul du volume selon le type d'équation (A0)
    a0_value <- eq$A0[1]  # Utilisation de l'indice [1] pour s'assurer d'avoir une valeur unique
    if (a0_value == 1) {
      # Équation linéaire: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
      volume <- eq$b0[1]
      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)
        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          x_val <- evaluer_expression(eq[[x_col]][1], variables)
          volume <- volume + eq[[b_col]][1] * x_val
        }
      }
    } else if (a0_value == 2) {
      # Même équation linéaire que A0=1 mais avec des coefficients différents
      volume <- eq$b0[1]
      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)

        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          x_val <- evaluer_expression(eq[[x_col]][1], variables)
          volume <- volume + eq[[b_col]][1] * x_val
        }
      }
    } else if (a0_value == 3) {
      # Même équation linéaire que A0=1 et A0=2 mais avec des coefficients différents
      volume <- eq$b0[1]
      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)

        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          x_val <- evaluer_expression(eq[[x_col]][1], variables)
          volume <- volume + eq[[b_col]][1] * x_val
        }
      }
    } else if (a0_value == 4) {
      # Équation logarithmique: Volume = 10^(b0 + b1*log(X1))
      log_input <- evaluer_expression(eq$X1[1], variables)
      if (log_input <= 0) {
        warning(paste("Valeur négative ou nulle pour logarithme à la ligne", i))
        next
      }
      volume <- 10^(eq$b0[1] + eq$b1[1] * log10(log_input))
    } else {
      warning(paste("Type d'équation inconnu (A0 =", a0_value, ") pour la ligne", i))
      next
    }

    # Vérification de la validité du résultat
    if (is.na(volume) || !is.finite(volume)) {
      warning(paste("Résultat de volume non valide à la ligne", i, ":", volume))
      next
    }

    df$Volume[i] <- volume
  }

  return(df)
}
