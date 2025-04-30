calculer_volumes <- function(df, type_volume = "VC22", essence = NULL,
                             equations_df = equations, id_equation = 1,
                             coefs_conversion = NULL, remove_na = FALSE) {

  # Liste des types de volume valides
  types_volume_valides <- c("VC22", "VC22B", "E", "VC22_HA")

  # Verifier la coherence entre type_volume et id_equation
  if (type_volume == "VC22" && !(id_equation %in% 1:3)) {
    stop("Pour le type de volume 'VC22', id_equation doit etre entre 1 et 3.")
  }

  #if (type_volume == "E" && !(id_equation %in% 4:5)) {
  #stop("Pour le type de volume 'E', id_equation doit etre 4 ou 5.")
  #}

  if (type_volume == "VC22_ha" && id_equation != 1) {
    stop("Pour le type de volume 'VC22_HA', id_equation doit etre 1.")
  }

  if (type_volume == "VC22B" && id_equation != 1) {
    stop("Pour le type de volume 'VC22B', id_equation doit etre 1.")
  }


  # Verification du type de volume
  if (!(type_volume %in% types_volume_valides)) {
    stop(paste("Type de volume invalide:", type_volume,
               "\nTypes valides:", paste(types_volume_valides, collapse = ", ")))
  }

  # Verification et preparation des identifiants d'essence
  colonnes_id_essence <- c("Essence", "Code", "Abr")
  colonne_essence_trouvee <- NULL

  # Verifie quelles colonnes d'identification sont presentes dans df
  colonnes_presentes <- intersect(colonnes_id_essence, colnames(df))

  if (length(colonnes_presentes) == 0) {
    stop("Le fichier doit contenir au moins une des colonnes suivantes: 'Essence', 'Code', ou 'Abr'")
  }

  # Utilise la premiere colonne presente comme identifiant d'essence
  colonne_essence_trouvee <- colonnes_presentes[1]

  # Si necessaire, ajoute une colonne Essence pour correspondre aux equations
  if (colonne_essence_trouvee != "Essence") {
    # Extrait les correspondances uniques des essences depuis equations_df
    if (!all(c("Essences", colonne_essence_trouvee) %in% colnames(equations_df))) {
      stop(paste("Le dataframe 'equations_df' doit contenir les colonnes 'Essences' et '",
                 colonne_essence_trouvee, "'", sep=""))
    }

    # Cree un dataframe de correspondance a partir de equations_df
    mapping_df <- unique(equations_df[, c("Essences", colonne_essence_trouvee)])
    names(mapping_df)[names(mapping_df) == "Essences"] <- "Essence"  # Standardise le nom

    # Cree une colonne temporaire Essence pour les calculs
    df <- merge(df, mapping_df, by = colonne_essence_trouvee, all.x = TRUE)

    # Verifier si des correspondances n'ont pas ete trouvees
    if (any(is.na(df$Essence))) {
      na_values <- unique(df[[colonne_essence_trouvee]][is.na(df$Essence)])
      warning(paste("Aucune correspondance trouvee pour les valeurs suivantes de",
                    colonne_essence_trouvee, ":", paste(na_values, collapse=", ")))
    }
  }

  # Verification des colonnes requises pour le diametre
  if (!any(c("C130", "C150") %in% colnames(df))) {
    stop("Le fichier doit contenir au moins une colonne 'C130' ou 'C150'")
  }

  # Conversion si necessaire
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

  # Calcul des surfaces terrieres si necessaires
  if (!"G130" %in% colnames(df) && "C130" %in% colnames(df)) {
    df$G130 <- (df$C130^2) / ((4 * pi)*10000)
  }

  if (!"G150" %in% colnames(df) && "C150" %in% colnames(df)) {
    df$G150 <- (df$C150^2) / ((4 * pi)*10000)
  }

  # Filtrer les equations
  eqs_volume <- equations_df[equations_df$Y == type_volume, ]
  if (nrow(eqs_volume) == 0) {
    stop(paste("Aucune equation trouvee pour le type de volume:", type_volume))
  }

  # S'assurer que les coefficients b0 a b5 sont numeriques
  colonnes_b <- paste0("b", 0:5)
  eqs_volume[colonnes_b] <- lapply(eqs_volume[colonnes_b], as.numeric)


  # Initialisation
  df$Equation_Utilisee <- NA_character_

  # Initialisation de la colonne de volume spécifiée par l'utilisateur
  if (!(type_volume %in% names(df))) {
    df[[type_volume]] <- NA_real_
  }

  # Fonction d'evaluation securisee
  evaluer_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
    env <- list2env(variables)
    tryCatch({
      eval(parse(text = expr_text), envir = env)
    }, error = function(e) {
      stop(paste("Erreur lors de l'evaluation de l'expression:", expr_text, "-", e$message))
    })
  }

  # Calcul pour chaque ligne
  for (i in seq_len(nrow(df))) {
    essence_arbre <- df$Essence[i]
    cat("Traitement ligne", i, "essence:", essence_arbre, "\n")

    eq_candidates <- if (!is.null(essence)) {
      eqs_volume[eqs_volume$Essences == essence, ]
    } else {
      eqs_volume[eqs_volume$Essences == essence_arbre, ]
    }

    if (nrow(eq_candidates) == 0) {
      # Si aucune équation spécifique n'est trouvée, utiliser l'équation générale
      eq_candidates <- eqs_volume[eqs_volume$Essences == "General", ]
      if (nrow(eq_candidates) == 0) {
        warning(paste("Pas d'equation trouvee pour l'essence:", essence_arbre))
        next
      }
    }

    # Vérifier si l'index d'équation demandé est disponible
    if (id_equation > nrow(eq_candidates)) {
      warning(paste("L'équation avec id", id_equation, "n'existe pas pour l'essence", essence_arbre,
                    ". Utilisation de l'équation 1 à la place."))
      eq <- eq_candidates[1, , drop = FALSE]
    } else {
      eq <- eq_candidates[id_equation, , drop = FALSE]
    }

    df$Equation_Utilisee[i] <- paste0(eq$Essences, ":", eq$Y, ":A0=", eq$A0)

    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(df)) {
        variables[[v]] <- df[[v]][i]
      } else {
        stop(paste("La variable", v, "est utilisee dans une equation mais absente des donnees."))
      }
    }

    a0_value <- eq$A0[1]

    # Ajoutez cette verification ici
    if (is.na(a0_value)) {
      warning(paste("Valeur A0 manquante pour l'essence", essence_arbre, "a la ligne", i))
      next
    } else if (a0_value %in% c(1, 2, 3, 5)) {
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
      C130 <- evaluer_expression(eq$X1[1], variables)  # Recupere directement la valeur de C130
      if (C130 <= 0) {
        warning(paste("Valeur negative ou nulle pour logarithme a la ligne", i))
        next
      }
      volume <- 10^(eq$b0[1] + eq$b1[1] * log10(C130))
    } else {
      warning(paste("Type d'equation inconnu (A0 =", a0_value, ") pour la ligne", i))
      next
    }

    if (is.na(volume) || !is.finite(volume)) {
      warning(paste("Resultat de volume non valide a la ligne", i, ":", volume))
      next
    }

    cat("  Volume calcule:", volume, "\n")

    # Stocker le volume uniquement pour cette ligne, dans la colonne spécifiée par type_volume
    df[[type_volume]][i] <- volume

    cat("  Volume stocke a la ligne", i, ":", df[[type_volume]][i], "\n")
  }

  # Nettoyage final si demande
  if (remove_na) {
    df <- df[!is.na(df[[type_volume]]), ]
  }

  return(df)
}
