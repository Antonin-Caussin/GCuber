calculer_volumes <- function(df, type_volume = "VC22", essence = NULL,
                             equations_df = equations, id_equation = 1,
                             remove_na = FALSE,
                             C130 = "C130", C150 = "C150",
                             HTOT = "HTOT", HDOM = "HDOM",
                             specimens = NULL,
                             verbose = FALSE) {  # Ajout du paramètre verbose pour contrôler l'affichage

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

  # Définir un mapping entre les noms de colonnes standard et les noms fournis par l'utilisateur
  col_mapping <- list()

  # Gestion du paramètre specimens pour l'identification des essences
  colonnes_id_essence <- list()
  type_specimens <- NULL

  if (!is.null(specimens)) {
    # Vérifier que la colonne spécifiée existe dans le dataframe
    if (!(specimens %in% colnames(df))) {
      stop(paste("La colonne spécifiée '", specimens, "' n'existe pas dans les données.", sep=""))
    }

    # Analyser le contenu de la colonne pour déterminer le type d'identifiant
    # Prendre un échantillon de valeurs non-NA pour l'analyse
    sample_values <- na.omit(df[[specimens]])

    if (length(sample_values) == 0) {
      stop(paste("La colonne '", specimens, "' ne contient que des valeurs manquantes.", sep=""))
    }

    # Déterminer le type d'identifiant basé sur la nature des données
    if (is.numeric(sample_values)) {
      # Si les valeurs sont numériques, c'est probablement un code
      type_specimens <- "Code"
    } else if (is.character(sample_values) || is.factor(sample_values)) {
      # Convertir en caractère si c'est un facteur
      if (is.factor(sample_values)) {
        sample_values <- as.character(sample_values)
      }

      # Calculer la longueur moyenne des chaînes
      mean_length <- mean(nchar(sample_values))

      if (mean_length <= 4) {
        # Si longueur moyenne <= 4, c'est probablement une abréviation
        type_specimens <- "Abr"
      } else {
        # Sinon, c'est probablement le nom complet de l'essence
        type_specimens <- "Essence"
      }
    } else {
      stop(paste("Le type de données dans la colonne '", specimens, "' n'est pas reconnu.", sep=""))
    }

    # Ajouter au mapping
    colonnes_id_essence[[type_specimens]] <- specimens
    cat("Type d'identifiant détecté dans la colonne '", specimens, "': ", type_specimens, "\n", sep="")
  } else {
    # Si specimens n'est pas spécifié, chercher les colonnes standard
    standard_cols <- c("Essence", "Code", "Abr")
    for (std_col in standard_cols) {
      if (std_col %in% colnames(df)) {
        colonnes_id_essence[[std_col]] <- std_col
      }
    }
  }

  if (length(colonnes_id_essence) == 0) {
    stop("Aucune colonne d'identification d'essence spécifiée ou trouvée dans les données.")
  }

  # Sélectionner la première colonne d'identification disponible
  colonne_essence_utilisee <- names(colonnes_id_essence)[1]
  col_id_essence <- colonnes_id_essence[[colonne_essence_utilisee]]

  # Vérifier que la colonne d'identification sélectionnée existe dans les données
  if (!(col_id_essence %in% colnames(df))) {
    stop(paste("La colonne d'identification d'essence spécifiée '", col_id_essence, "' n'existe pas dans les données.", sep=""))
  }

  # Vérifier si les colonnes de diamètre existent
  C130_exists <- C130 %in% colnames(df)
  C150_exists <- C150 %in% colnames(df)

  if (!C130_exists && !C150_exists) {
    stop(paste("Aucune des colonnes de diamètre spécifiées ('", C130, "' ou '", C150, "') n'existe dans les données.", sep=""))
  }

  # Créer une copie de df pour éviter de modifier le dataframe original
  df_result <- df

  # Si nécessaire, ajoute une colonne Essence pour correspondre aux équations
  if (colonne_essence_utilisee != "Essence") {
    # Extrait les correspondances uniques des essences depuis equations_df
    if (!all(c("Essences", colonne_essence_utilisee) %in% colnames(equations_df))) {
      stop(paste("Le dataframe 'equations_df' doit contenir les colonnes 'Essences' et '",
                 colonne_essence_utilisee, "'", sep=""))
    }

    # Cree un dataframe de correspondance a partir de equations_df
    mapping_df <- unique(equations_df[, c("Essences", colonne_essence_utilisee)])
    names(mapping_df)[names(mapping_df) == "Essences"] <- "Essence"  # Standardise le nom

    # Cree une colonne temporaire Essence pour les calculs
    df_result <- merge(df_result, mapping_df, by = col_id_essence, all.x = TRUE)

    # Verifier si des correspondances n'ont pas ete trouvees
    if (any(is.na(df_result$Essence))) {
      na_values <- unique(df_result[[col_id_essence]][is.na(df_result$Essence)])
      warning(paste("Aucune correspondance trouvee pour les valeurs suIVantes de",
                    col_id_essence, ":", paste(na_values, collapse=", ")))
    }
  } else {
    # Si la colonne s'appelle déjà "Essence", pas besoin de mapping
    df_result$Essence <- df_result[[col_id_essence]]
  }

  # Conversion C150 en C130 si nécessaire
  if (!C130_exists && C150_exists) {
    cat("Conversion de C150 en C130 nécessaire...\n")

    # Extraire les coefficients par essence
    coefs_df <- unique(equations_df[, c("Essences", "NumEquation", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Essence"

    # Initialiser la colonne C130 avec NA
    df_result$C130 <- NA_real_

    # Effectuer la conversion ligne par ligne
    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Essence[i]
      c150_value <- df_result[[C150]][i]

      if (!is.na(essence_arbre) && !is.na(c150_value)) {
        # Trouver les coefficients pour cette essence
        coef_row <- coefs_df[coefs_df$Essence == essence_arbre, ]

        if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
          # Récupérer les coefficients
          HV_coef <- coef_row$HV[1]
          IV_coef <- coef_row$IV[1]

          # Appliquer la formule de conversion
          # Utilisez les colonnes HV et IV comme coefficients
          df_result$C130[i] <- HV_coef * c150_value + IV_coef

        } else {
          warning(paste("Impossible de convertir C150 en C130 pour l'essence:", essence_arbre,
                        "à la ligne", i, ". Coefficients manquants."))
        }
      }
    }

    # Vérifier si certaines conversions ont échoué
    if (any(is.na(df_result$C130))) {
      warning("Certaines conversions C150 à C130 ont échoué. Vérifiez les données.")
    }

    cat("Conversion C150 → C130 terminée.\n")
  }

  # Conversion C130 en C150 si nécessaire
  if (!C150_exists && C130_exists) {
    cat("Conversion de C130 en C150 nécessaire...\n")

    # Extraire les coefficients par essence
    coefs_df <- unique(equations_df[, c("Essences", "NumEquation", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Essence"

    # Initialiser la colonne C150 avec NA
    df_result[[C150]] <- NA_real_

    # Effectuer la conversion ligne par ligne
    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Essence[i]
      c130_value <- df_result$C130[i]

      if (!is.na(essence_arbre) && !is.na(c130_value)) {
        # Trouver les coefficients pour cette essence
        coef_row <- coefs_df[coefs_df$Essence == essence_arbre, ]

        if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
          # Récupérer les coefficients
          HV_coef <- coef_row$HV[1]
          IV_coef <- coef_row$IV[1]

          # Appliquer la formule de conversion inverse (C130 vers C150)
          # Pour inverser C130 = HV_coef * C150 + IV_coef
          # Nous obtenons C150 = (C130 - IV_coef) / HV_coef
          df_result[[C150]][i] <- (c130_value - IV_coef) / HV_coef

        } else {
          warning(paste("Impossible de convertir C130 en C150 pour l'essence:", essence_arbre,
                        "à la ligne", i, ". Coefficients manquants."))
        }
      }
    }

    cat("Conversion C130 → C150 terminée.\n")
  }
  # Vérifier et gérer les colonnes de hauteur
  HTOT_exists <- HTOT %in% colnames(df_result)
  HDOM_exists <- HDOM %in% colnames(df_result)

  # Copier les colonnes de hauteur si nécessaire
  if (HTOT_exists && HTOT != "HTOT") {
    df_result$HTOT <- df_result[[HTOT]]
  }

  if (HDOM_exists && HDOM != "HDOM") {
    df_result$HDOM <- df_result[[HDOM]]
  }

  # Calcul des surfaces terrieres si necessaires
  if (!"G130" %in% colnames(df_result) && "C130" %in% colnames(df_result)) {
    df_result$G130 <- (df_result$C130^2) / ((4 * pi)*10000)
  }

  if (!"G150" %in% colnames(df_result) && C150_exists) {
    # Utiliser le nom de colonne spécifié pour C150
    df_result$G150 <- (df_result[[C150]]^2) / ((4 * pi)*10000)
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
  df_result$Equation_Utilisee <- NA_character_

  # Initialisation de la colonne de volume spécifiée par l'utilisateur
  if (!(type_volume %in% names(df_result))) {
    df_result[[type_volume]] <- NA_real_
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
  for (i in seq_len(nrow(df_result))) {
    essence_arbre <- df_result$Essence[i]
    if (verbose) cat("Traitement ligne", i, "essence:", essence_arbre, "\n")

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

    df_result$Equation_Utilisee[i] <- paste0(eq$Essences, ":", eq$Y, ":A0=", eq$A0)

    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(df_result)) {
        variables[[v]] <- df_result[[v]][i]
      } else {
        stop(paste("La variable", v, "est utilisee dans une equation mais absente des donnees."))
      }
    }

    a0_value <- eq$A0[1]

    # DÉBUT DE LA PARTIE AJOUTÉE: Visualisation de l'équation et des coefficients
    if (verbose || type_volume == "VC22B") {  # Afficher pour VC22B ou si verbose=TRUE
      cat("\n-------------------------------------------------------\n")
      cat("DÉTAILS DE L'ÉQUATION POUR LA LIGNE", i, "\n")
      cat("Essence: ", essence_arbre, "\n")
      cat("Type de volume: ", type_volume, "\n")
      cat("Type d'équation (A0): ", a0_value, "\n")
      cat("Coefficients:\n")
      for (j in 0:5) {
        b_col <- paste0("b", j)
        if (!is.na(eq[[b_col]][1])) {
          cat("  b", j, " = ", eq[[b_col]][1], "\n", sep="")
        }
      }

      cat("Variables utilisées:\n")
      for (j in 1:5) {
        x_col <- paste0("X", j)
        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          expr_val <- eq[[x_col]][1]
          cat("  X", j, " = ", expr_val, sep="")
          # Tenter d'évaluer l'expression
          tryCatch({
            val <- evaluer_expression(expr_val, variables)
            cat(" = ", val, "\n", sep="")
          }, error = function(e) {
            cat(" [ERREUR: ", e$message, "]\n", sep="")
          })
        }
      }

      # Si c'est une équation linéaire, montrer la formule complète
      if (a0_value %in% c(1, 2, 3, 5)) {
        cat("Formule: Volume = ", eq$b0[1], sep="")
        for (j in 1:5) {
          x_col <- paste0("X", j)
          b_col <- paste0("b", j)
          if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
            x_val <- tryCatch(evaluer_expression(eq[[x_col]][1], variables), error = function(e) "?")
            b_val <- eq[[b_col]][1]
            sign <- if (b_val >= 0) " + " else " - "
            b_abs <- abs(b_val)
            cat(sign, b_abs, " * ", eq[[x_col]][1], " [", x_val, "]", sep="")
          }
        }
        cat("\n")
      } else if (a0_value == 4) {
        # Formule logarithmique
        cat("Formule: Volume = 10^(", eq$b0[1], " + ", eq$b1[1], " * log10(C130))\n", sep="")
        C130_val <- tryCatch(evaluer_expression("C130", variables), error = function(e) "?")
        cat("  avec C130 = ", C130_val, "\n", sep="")
      }
    }
    # FIN DE LA PARTIE AJOUTÉE

    # Ajoutez cette verification ici
    if (is.na(a0_value)) {
      warning(paste("Valeur A0 manquante pour l'essence", essence_arbre, "a la ligne", i))
      next
    } else if (a0_value %in% c(1, 2, 3, 5)) {
      volume <- eq$b0[1]

      # AJOUT: Visualisation du calcul détaillé
      if (verbose || type_volume == "VC22B") {
        cat("Calcul détaillé:\n")
        cat("  Départ avec b0 =", eq$b0[1], "\n")
        cumul_volume <- eq$b0[1]
      }

      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)
        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          x_val <- evaluer_expression(eq[[x_col]][1], variables)
          term_val <- eq[[b_col]][1] * x_val
          volume <- volume + term_val

          # AJOUT: Visualisation des étapes intermédiaires
          if (verbose || type_volume == "VC22B") {
            cumul_volume <- cumul_volume + term_val
            cat("  + b", j, " * X", j, " = ", eq[[b_col]][1], " * ", x_val, " = ", term_val,
                " (cumul: ", cumul_volume, ")\n", sep="")
          }
        }
      }
    } else if (a0_value == 4) {
      C130 <- evaluer_expression(eq$X1[1], variables)  # Recupere directement la valeur de C130
      if (C130 <= 0) {
        warning(paste("Valeur negatIVe ou nulle pour logarithme a la ligne", i))
        next
      }
      volume <- 10^(eq$b0[1] + eq$b1[1] * log10(C130))

      # AJOUT: Visualisation du calcul logarithmique
      if (verbose || type_volume == "VC22B") {
        cat("Calcul logarithmique:\n")
        cat("  log10(C130) =", log10(C130), "\n")
        cat("  ", eq$b0[1], " + ", eq$b1[1], " * ", log10(C130), " = ",
            eq$b0[1] + eq$b1[1] * log10(C130), "\n", sep="")
        cat("  10^(", eq$b0[1] + eq$b1[1] * log10(C130), ") = ", volume, "\n", sep="")
      }
    } else {
      warning(paste("Type d'equation inconnu (A0 =", a0_value, ") pour la ligne", i))
      next
    }

    if (is.na(volume) || !is.finite(volume)) {
      warning(paste("Resultat de volume non valide a la ligne", i, ":", volume))
      next
    }

    # AJOUT: Affichage du résultat final
    if (verbose || type_volume == "VC22B") {
      cat("RÉSULTAT FINAL pour la ligne", i, ": Volume", type_volume, "=", volume, "\n")
      if (volume < 0) {
        cat("⚠️ ATTENTION: Volume négatif détecté! ⚠️\n")
      } else if (volume < 0.001) {
        cat("⚠️ ATTENTION: Volume anormalement petit détecté! ⚠️\n")
      }
      cat("-------------------------------------------------------\n\n")
    } else {
      if (verbose) cat("  Volume calcule:", volume, "\n")
    }

    # Stocker le volume uniquement pour cette ligne, dans la colonne spécifiée par type_volume
    df_result[[type_volume]][i] <- volume

    if (verbose) cat("  Volume stocke a la ligne", i, ":", df_result[[type_volume]][i], "\n")
  }

  # Nettoyage final si demande
  if (remove_na) {
    df_result <- df_result[!is.na(df_result[[type_volume]]), ]
  }

  return(df_result)
}
