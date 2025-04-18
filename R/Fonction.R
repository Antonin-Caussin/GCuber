#' Calcule les volumes forestiers selon différentes méthodes
#'
#' @param df Un data frame contenant les données des arbres
#' @param type_volume Type de volume à calculer (ex: "VC22", "VC22B", "Ecorce")
#' @param essence Essence d'arbre spécifique (ou NULL pour utiliser celle des données)
#' @param equations_df Le data frame contenant les équations allométriques
#' @param id_equation Indice de l'équation à utiliser si plusieurs sont disponibles (par défaut = 1)
#' @param coefs_conversion (Optionnel) Data frame de conversion de C150 vers C130
#' @return Un data frame avec les données d'entrée plus une colonne de volume calculé
#' @export

calculer_volumes <- function(df, type_volume = "VC22", essence = NULL,
                             equations_df = equations_allometriques, id_equation = 1,
                             coefs_conversion = NULL) {

  # Conversion C150 -> C130 si nécessaire
  if (!"C130" %in% colnames(donnees_arbres) && "C150" %in% colnames(donnees_arbres)) {
    if (!"Essence" %in% colnames(donnees_arbres)) {
      stop("Impossible de convertir C150 en C130 sans la colonne 'Essence'.")
    }
    if (is.null(coefs_conversion)) {
      stop("Le tableau 'coefs_conversion' est requis pour convertir C150 en C130.")
    }

    donnees_arbres <- merge(donnees_arbres, coefs_conversion, by = "Essence", all.x = TRUE)

    if (any(is.na(donnees_arbres$Coef_C150_C130))) {
      stop("Coefficient de conversion manquant pour certaines essences.")
    }

    donnees_arbres$C130 <- donnees_arbres$C150 * donnees_arbres$Coef_C150_C130
  }

  if (!"G130" %in% colnames(donnees_arbres) && "C130" %in% colnames(donnees_arbres)) {
    donnees_arbres$G130 <- (donnees_arbres$C130^2) / (4 * pi)
  }

  if (!"G150" %in% colnames(donnees_arbres) && "C150" %in% colnames(donnees_arbres)) {
    donnees_arbres$G150 <- (donnees_arbres$C150^2) / (4 * pi)
  }

  # Vérification des colonnes de base
  if (!all(c("Essence", "C130") %in% colnames(donnees_arbres))) {
    stop("Le fichier doit contenir au moins les colonnes 'Essence' et 'C130'")
  }

  # Filtrer les équations selon le type de volume
  eqs_volume <- equations_df[equations_df$Y == type_volume, ]

  if (nrow(eqs_volume) == 0) {
    stop(paste("Aucune équation trouvée pour le type de volume:", type_volume))
  }

  # Vérifie si certaines équations nécessitent HDOM
  necessite_hdom <- any(grepl("HDOM", c(eqs_volume$X1, eqs_volume$X2,
                                        eqs_volume$X3, eqs_volume$X4, eqs_volume$X5)))
  if (necessite_hdom && !("HDOM" %in% colnames(donnees_arbres))) {
    stop("Certaines équations nécessitent la colonne 'HDOM' absente de vos données.")
  }

  # Fonction d’évaluation dynamique
  evaluer_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
    env <- list2env(variables)
    eval(parse(text = expr_text), envir = env)
  }

  # Initialisation du résultat
  donnees_arbres$Volume <- NA

  for (i in seq_len(nrow(donnees_arbres))) {
    essence_arbre <- donnees_arbres$Essence[i]

    # Sélection de l’équation
    eq <- if (!is.null(essence)) {
      eqs_volume[eqs_volume$Essences == essence, ]
    } else {
      eq_tmp <- eqs_volume[eqs_volume$Essences == essence_arbre, ]
      if (nrow(eq_tmp) == 0) {
        eq_tmp <- eqs_volume[eqs_volume$Essences == "General", ]
      }
      eq_tmp
    }

    if (nrow(eq) == 0) {
      warning(paste("Pas d'équation trouvée pour l'essence:", essence_arbre))
      next
    }

    if (nrow(eq) > 1) {
      if (id_equation > nrow(eq)) {
        stop(paste("id_equation =", id_equation, "dépasse le nombre d'équations disponibles pour", essence_arbre))
      }
      eq <- eq[id_equation, , drop = FALSE]
    }

    # Préparation des variables nécessaires
    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(donnees_arbres)) {
        variables[[v]] <- donnees_arbres[[v]][i]
      } else {
        stop(paste("La variable", v, "est utilisée dans une équation mais absente des données."))
      }
    }

    # Calcul du volume
    if (eq$A0 == 1) {
      volume <- eq$b0
      for (j in 1:5) {
        if (!is.na(eq[[paste0("X", j)]]) && eq[[paste0("X", j)]] != "0") {
          x_val <- evaluer_expression(eq[[paste0("X", j)]], variables)
          volume <- volume + eq[[paste0("b", j)]] * x_val
        }
      }
    } else if (eq$A0 == 2) {
      log_input <- evaluer_expression(eq$X1, variables)
      volume <- 10^(eq$b0 + eq$b1 * log(log_input))
    }

    donnees_arbres$Volume[i] <- volume
  }

  return(donnees_arbres)
}
