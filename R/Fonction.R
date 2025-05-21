#' Calcule différents types de volumes à partir de données dendrométriques
#'
#' Cette fonction permet de calculer différents types de volumes d'arbres à partir de données
#' dendrométriques en utilisant diverses équations allométriques. Elle gère la diversité
#' des essences forestières, les différentes méthodes de mesure (C130, C150) et peut s'adapter
#' à différentes structures de données d'entrée.
#'
#' @param df Un data frame contenant les données dendrométriques des arbres.
#' @param type_volume Le type de volume à calculer. Valeurs valides : "V22", "V22B", "E", "V22_HA". Par défaut : "V22".
#' @param essence Si spécifié, les calculs seront effectués uniquement avec les équations correspondant
#'        à cette essence. Par défaut : NULL (utilise les essences indiquées dans les données).
#' @param id_equation L'identifiant de l'équation à utiliser pour chaque essence, representant le nombre d'entrée. Par défaut : 1.
#' @param remove_na Booléen indiquant si les lignes avec des volumes non calculés doIVent être supprimées.
#'        Par défaut : FALSE.
#' @param C130 Nom de la colonne contenant la circonférence à 130 cm. Par défaut : "C130".
#' @param C150 Nom de la colonne contenant la circonférence à 150 cm. Par défaut : "C150".
#' @param HTOT Nom de la colonne contenant la hauteur totale. Par défaut : "HTOT".
#' @param HDOM Nom de la colonne contenant la hauteur dominante. Par défaut : "HDOM".
#' @param specimens Nom de la colonne contenant l'identifiant des essences(nom complet, code ou abréviation). Par défaut : NULL.
#'
#' @details
#' \subsection{Types de volume supportés}{
#'   \itemize{
#'     \item \strong{V22} : Volume marchand jusqu'à une découpe de 22 cm de circonférence.
#'     \item \strong{V22B} : Volume des branches jusqu'a une découpe de 22cm de circonférence.
#'     \item \strong{E} : Volume d'écorce de l'arbre.
#'     \item \strong{V22_HA} : Volume marchand par hectare.
#'   }
#' }
#'
#' \subsection{Structure des données d'entrée requise}{
#'   La fonction nécessite au minimum :
#'   \itemize{
#'     \item Une colonne d'identification des essences forestières (spécifiée via \code{specimens}.
#'     \item Une colonne de diamètre (soit \code{C130}, soit \code{C150}).
#'   }
#' }
#'
#' \subsection{Types d'équations supportés (valeurs de A0)}{
#'   \enumerate{
#'     \item Équation linéaire standard à 1 entrée: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'     \item Équation linéaire standard à 2 entrée: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'     \item Équation linéaire standard à 3 entrée: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'     \item Équation logarithmique : Volume = 10^(b0 + b1*log10(C130))
#'     \item Équation linéaire standard à une entrée pour le volume d'ecorce : Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
#'   }
#' }
#'
#' @return Un data frame similaire à \code{df} avec les colonnes supplémentaires suivantes :
#' \itemize{
#'   \item La colonne spécifiée par \code{type_volume} contenant les volumes calculés.
#'   \item \code{Equation_Utilisee} : Information sur l'équation utilisée pour chaque ligne.
#'   \item Si une conversion C150 à C130 a été effectuée, une colonne \code{C130} est ajoutée.
#'   \item Si le mapping d'essence a été nécessaire, une colonne \code{Species} est ajoutée.
#' }
#'
#' @examples
#' # Exemple de base avec données standard
#' # Supposons que nous avons un data frame "donnees_arbres" avec des colonnes C130 et Essence
#' \dontrun{
#' resultats <- calculer_volumes(
#'   df = donnees_arbres,
#'   type_volume = "V22",
#'   C130 = circ2024
#' )
#'
#' # Exemple avec noms de colonnes personnalisés
#' resultats <- calculer_volumes(
#'   df = donnees_arbres,
#'   type_volume = "E",
#'   id_equation = 2,
#'   C130 = "Circonference130",
#'   HTOT = "HauteurTotale",
#'   specimens = "NomEssence"
#' )
#'
#' # Exemple avec conversion C150 à C130
#' resultats <- calculer_volumes(
#'   df = donnees_arbres,
#'   type_volume = "V22",
#'   C150 = "Circ150"
#' )
#' }
#'
#' @note
#' La fonction affiche des messages d'information pendant l'exécution pour faciliter le débogage.
#' Des avertissements sont émis si des correspondances d'essences ne sont pas trouvées ou si
#' le calcul du volume échoue pour certaines lignes.
#'
#' @seealso
#' Fonctions connexes pour la gestion forestière et les calculs dendrométriques.
#'
#' @author Caussin Antonin
#' @references
#' Dagnelie, P., Rondeux, J., & Thill, A. (1985). Tables de cubage des arbres et des peuplements forestiers. Gembloux, Belgique: Presses agronomiques de Gembloux.
#'
#' @importFrom stats na.omit
#' @export

calculer_volumes <- function(df, type_volume = "V22", essence = NULL,
                             id_equation = 1,
                             remove_na = FALSE,
                             C130 = "C130", C150 = "C150",
                             HTOT = "HTOT", HDOM = "HDOM",
                             specimens = NULL) {

  # Avertissement pour type_volume = "E" avec id_equation incompatible
  if (type_volume == "E" && id_equation %in% c(1, 2, 3,4,5)) {
    warning(paste("ATTENTION: Pour le type de volume 'E', il n'est pas necessaire de specifier l'id_equation",
                  "Vous avez spécifié id_equation =", id_equation,
                  "qui pourrait ne pas être adapté ou ne pas traiter tout le jeu de donnés. La fonction tentera d'utiliser l'équation 4 et/ou 5 si disponible."))
  }

  # Utilisation du data frame interne equations
  equations_df <- equations

  # Liste des types de volume valides
  types_volume_valides <- c("V22", "V22B", "E", "V22_HA")

  # Verifier la coherence entre type_volume et id_equation, sauf pour le type E qui sera géré spécialement
  if (type_volume == "V22" && !(id_equation %in% 1:3)) {
    stop("Pour le type de volume 'V22', id_equation doit etre entre 1 et 3.")
  }

  # Pour le type "E", nous ne vérifions pas la cohérence ici car elle sera gérée dynamiquement
  # en fonction de l'espèce dans la boucle de calcul

  if (type_volume == "V22_ha" && id_equation != 1) {
    stop("Pour le type de volume 'V22_HA', id_equation doit etre 1.")
  }

  if (type_volume == "V22B" && id_equation != 1) {
    stop("Pour le type de volume 'V22B', id_equation doit etre 1.")
  }

  # Verification du type de volume
  if (!(type_volume %in% types_volume_valides)) {
    stop(paste("Type de volume invalide:", type_volume,
               "\nTypes valides:", paste(types_volume_valides, collapse = ", ")))
  }

  # DEBUG: Afficher le type de volume choisi
  cat("Type de volume sélectionné:", type_volume, "\n")

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
  }

  if (length(colonnes_id_essence) == 0) {
    stop("Aucune colonne d'identification d'essence spécifiée ou trouvée dans les données.")
  }

  # Sélectionner la première colonne d'identification disponible
  colonne_essence_utilisee <- names(colonnes_id_essence)[1]
  col_id_essence <- colonnes_id_essence[[colonne_essence_utilisee]]

  # DEBUG: Afficher les colonnes d'identification
  cat("Colonne d'identification utilisée:", colonne_essence_utilisee, "(", col_id_essence, ")\n")

  # Créer une copie de df pour éviter de modifier le dataframe original
  df_result <- df

  # Si nécessaire, ajoute une colonne Species pour correspondre aux équations
  if (colonne_essence_utilisee != "Essence" || col_id_essence != "Essence") {
    # Extrait les correspondances uniques des essences depuis equations_df
    if (!all(c("Essences", colonne_essence_utilisee) %in% colnames(equations_df))) {
      stop(paste("Le dataframe 'equations_df' doit contenir les colonnes 'Essences' et '",
                 colonne_essence_utilisee, "'", sep=""))
    }

    # DEBUG: Vérifier l'existence des colonnes nécessaires
    cat("Colonnes disponibles dans equations_df:", paste(colnames(equations_df), collapse=", "), "\n")

    # Cree un dataframe de correspondance a partir de equations_df
    mapping_df <- unique(equations_df[, c("Essences", colonne_essence_utilisee)])
    names(mapping_df)[names(mapping_df) == "Essences"] <- "Species"  # Renommer en Species au lieu de Essence

    # DEBUG: Afficher un aperçu du mapping
    cat("Aperçu du mapping des essences:\n")
    print(head(mapping_df))

    # Cree une colonne temporaire Species pour les calculs
    if (col_id_essence != colonne_essence_utilisee) {
      # Renommer temporairement la colonne dans mapping_df pour qu'elle corresponde au nom dans df_result
      names(mapping_df)[names(mapping_df) == colonne_essence_utilisee] <- col_id_essence
    }

    # Conserver l'original avant merge pour éviter les duplications
    orig_colnames <- colnames(df_result)

    # Utiliser merge avec all.x=TRUE pour garder toutes les lignes de df_result
    df_result <- merge(df_result, mapping_df, by = col_id_essence, all.x = TRUE)

    # Vérifier si le merge a créé des colonnes dupliquées (.x, .y)
    if ("Species.x" %in% colnames(df_result)) {
      # Nettoyer les noms de colonnes dupliqués
      names(df_result)[names(df_result) == "Species.x"] <- "Species"

      # Supprimer la colonne dupliquée si elle existe
      if ("Species.y" %in% colnames(df_result)) {
        df_result$Species.y <- NULL
      }
    }

    # Verifier si des correspondances n'ont pas ete trouvees
    if (any(is.na(df_result$Species))) {
      na_values <- unique(df_result[[col_id_essence]][is.na(df_result$Species)])
      warning(paste("Aucune correspondance trouvée pour les valeurs suivantes de",
                    col_id_essence, ":", paste(na_values, collapse=", ")))
    }
  } else {
    # Si la colonne s'appelle déjà "Essence", la renommer en "Species"
    names(df_result)[names(df_result) == "Essence"] <- "Species"
  }

  # DEBUG: Vérifier les colonnes après mapping
  cat("Colonnes après mapping:", paste(colnames(df_result), collapse=", "), "\n")

  # Vérifier si les colonnes de diamètre existent
  C130_exists <- C130 %in% colnames(df)
  C150_exists <- C150 %in% colnames(df)

  if (!C130_exists && !C150_exists) {
    stop(paste("Aucune des colonnes de diamètre spécifiées ('", C130, "' ou '", C150, "') n'existe dans les données.", sep=""))
  }

  # Conversion C150 en C130 si nécessaire
  if (!C130_exists && C150_exists) {
    cat("Conversion de C150 en C130 nécessaire...\n")

    # Vérifier que les colonnes HV et IV existent
    if(!all(c("Essences", "HV", "IV") %in% colnames(equations_df))) {
      stop("Les colonnes 'Essences', 'HV' et 'IV' doivent exister dans le dataframe d'équations")
    }

    # Extraire les coefficients par essence
    coefs_df <- unique(equations_df[, c("Essences", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Species"

    # DEBUG: Afficher les coefficients
    cat("Aperçu des coefficients de conversion:\n")
    print(head(coefs_df))

    # Initialiser la colonne C130 avec NA
    df_result$C130 <- NA_real_

    # Effectuer la conversion ligne par ligne
    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]

      if (!is.na(essence_arbre)) {
        c150_value <- df_result[[C150]][i]

        if (!is.na(c150_value)) {
          # Trouver les coefficients pour cette essence
          coef_row <- coefs_df[coefs_df$Species == essence_arbre, ]

          # DEBUG: Pour les premières lignes, afficher les détails
          if (i <= 5) {
            cat("Ligne", i, "- Essence:", essence_arbre, "- Coefficients trouvés:",
                nrow(coef_row), "\n")
            if (nrow(coef_row) > 0) {
              cat("  HV:", coef_row$HV[1], "IV:", coef_row$IV[1], "\n")
            }
          }

          if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
            # Récupérer les coefficients
            HV_coef <- coef_row$HV[1]
            IV_coef <- coef_row$IV[1]

            # Appliquer la formule de conversion
            df_result$C130[i] <- HV_coef * c150_value + IV_coef

            # DEBUG: Afficher le résultat de la conversion
            if (i <= 5) {
              cat("  Conversion:", c150_value, "->", df_result$C130[i], "\n")
            }
          } else {
            warning(paste("Impossible de convertir C150 en C130 pour l'essence:", essence_arbre,
                          "à la ligne", i, ". Coefficients manquants."))
          }
        }
      }
    }

    # Vérifier si certaines conversions ont échoué
    failed_conversions <- sum(is.na(df_result$C130))
    if (failed_conversions > 0) {
      warning(paste(failed_conversions, "conversions C150 à C130 ont échoué. Vérifiez les données."))
    }

    cat("Conversion C150 → C130 terminée.\n")
  }

  # Conversion C130 en C150 si nécessaire
  if (!C150_exists && C130_exists) {
    cat("Conversion de C130 en C150 nécessaire...\n")

    # Vérifier que les colonnes HV et IV existent
    if(!all(c("Essences", "HV", "IV") %in% colnames(equations_df))) {
      stop("Les colonnes 'Essences', 'HV' et 'IV' doivent exister dans le dataframe d'équations")
    }

    # Extraire les coefficients par essence
    coefs_df <- unique(equations_df[, c("Essences", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Species"

    # Initialiser la colonne C150 avec NA
    df_result[[C150]] <- NA_real_

    # Effectuer la conversion ligne par ligne
    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]

      if (!is.na(essence_arbre)) {
        c130_value <- df_result$C130[i]

        if (!is.na(c130_value)) {
          # Trouver les coefficients pour cette essence
          coef_row <- coefs_df[coefs_df$Species == essence_arbre, ]

          # DEBUG: Pour les premières lignes, afficher les détails
          if (i <= 5) {
            cat("Ligne", i, "- Essence:", essence_arbre, "- Coefficients trouvés:",
                nrow(coef_row), "\n")
            if (nrow(coef_row) > 0) {
              cat("  HV:", coef_row$HV[1], "IV:", coef_row$IV[1], "\n")
            }
          }

          if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
            # Récupérer les coefficients
            HV_coef <- coef_row$HV[1]
            IV_coef <- coef_row$IV[1]

            # Appliquer la formule de conversion inverse (C130 vers C150)
            # Pour inverser C130 = HV_coef * C150 + IV_coef
            # Nous obtenons C150 = (C130 - IV_coef) / HV_coef
            df_result[[C150]][i] <- (c130_value - IV_coef) / HV_coef

            # DEBUG: Afficher le résultat de la conversion
            if (i <= 5) {
              cat("  Conversion inverse:", c130_value, "->", df_result[[C150]][i], "\n")
            }

          } else {
            warning(paste("Impossible de convertir C130 en C150 pour l'essence:", essence_arbre,
                          "à la ligne", i, ". Coefficients manquants."))
          }
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
  # DEBUG: Afficher les équations trouvées
  cat("Nombre d'équations trouvées pour", type_volume, ":", nrow(eqs_volume), "\n")
  cat("Aperçu des équations disponibles:\n")
  print(head(eqs_volume[, c("Essences", "Y", "A0")]))

  if (nrow(eqs_volume) == 0) {
    stop(paste("Aucune équation trouvée pour le type de volume:", type_volume))
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

  # Modification de la fonction evaluer_expression pour plus de débogage
  evaluer_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)

    # Vérifier si l'expression est valide syntaxiquement
    if (expr_text == "/") {
      warning("Expression invalide détectée: '/'")
      return(NA)
    }

    # Vérifier que toutes les variables nécessaires sont présentes et non NA
    var_names <- all.vars(parse(text = expr_text))
    for (v in var_names) {
      if (!v %in% names(variables) || is.na(variables[[v]])) {
        warning(paste("Variable manquante ou NA:", v, "pour l'expression:", expr_text))
        return(NA)
      }
    }

    env <- list2env(variables)
    tryCatch({
      eval(parse(text = expr_text), envir = env)
    }, error = function(e) {
      warning(paste("Erreur lors de l'evaluation de l'expression:", expr_text, "-", e$message))
      return(NA)
    })
    env <- list2env(variables)
    tryCatch({
      eval(parse(text = expr_text), envir = env)
    }, error = function(e) {
      warning(paste("Erreur lors de l'evaluation de l'expression:", expr_text, "-", e$message))
      return(NA)
    })
  }

  # Calcul pour chaque ligne
  for (i in seq_len(nrow(df_result))) {
    essence_arbre <- df_result$Species[i]  # Utilisation de Species au lieu de Essence

    # Initialisation de la variable volume pour cette ligne
    volume <- 0

    # Pour le volume de type "E", déterminer dynamiquement l'équation à utiliser
    # selon l'espèce, toujours l'équation 4 ou 5 spécifique à chaque essence
    if (type_volume == "E") {
      # Trouver toutes les équations disponibles pour cette essence et ce type de volume
      eq_candidates_E <- eqs_volume[eqs_volume$Essences == essence_arbre, ]

      # Filtrer uniquement les équations avec A0 = 4 ou A0 = 5
      eq_candidates_E_filtered <- eq_candidates_E[eq_candidates_E$A0 %in% c(4, 5), ]

      if (nrow(eq_candidates_E_filtered) > 0) {
        # Déterminer quelle équation (4 ou 5) est associée à cette essence
        if (any(eq_candidates_E_filtered$A0 == 4)) {
          local_id_equation <- 4
          cat(" Utilisation de l'équation A0 = 4 pour", essence_arbre, "\n")
        } else {
          local_id_equation <- 5
          cat(" Utilisation de l'équation A0 = 5 pour", essence_arbre, "\n")
        }
      } else {
        # Si aucune équation 4 ou 5 n'est trouvée pour cette essence, garder l'id_equation original
        cat(" Aucune équation de type 4 ou 5 trouvée pour", essence_arbre, ". Utilisation de l'équation par défaut.\n")
      }
    } else {
      # Pour les autres types de volume, garder l'équation spécifiée par l'utilisateur
      local_id_equation <- id_equation
    }

    eq_candidates <- if (!is.null(essence)) {
      eqs_volume[eqs_volume$Essences == essence, ]
    } else {
      eqs_volume[eqs_volume$Essences == essence_arbre, ]
    }

    # DEBUG: Afficher les équations candidates pour cette essence
    if (i <= 5) {
      cat("  Nombre d'équations candidates:", nrow(eq_candidates), "\n")
    }

    if (nrow(eq_candidates) == 0) {
      # Si aucune équation spécifique n'est trouvée, utiliser l'équation générale
      eq_candidates <- eqs_volume[eqs_volume$Essences == "General", ]
      if (nrow(eq_candidates) == 0) {
        warning(paste("Pas d'equation trouvee pour l'essence:", essence_arbre))
        next
      }
    }

    # Pour le type "E", utiliser l'équation avec A0 correspondant à local_id_equation
    if (type_volume == "E" && local_id_equation %in% c(4, 5)) {
      eq_by_a0 <- eq_candidates[eq_candidates$A0 == local_id_equation, ]
      if (nrow(eq_by_a0) > 0) {
        eq <- eq_by_a0[1, , drop = FALSE]  # Prendre la première équation si plusieurs correspondent
        cat("  Utilisation de l'équation A0 =", local_id_equation, "pour", essence_arbre, "\n")
      } else {
        # Si aucune équation avec le A0 spécifique n'est trouvée
        warning(paste("Pas d'équation avec A0 =", local_id_equation, "trouvée pour l'essence", essence_arbre))

        # Essayer avec l'autre type d'équation (4 ou 5)
        other_a0 <- if (local_id_equation == 4) 5 else 4
        eq_by_other_a0 <- eq_candidates[eq_candidates$A0 == other_a0, ]

        if (nrow(eq_by_other_a0) > 0) {
          eq <- eq_by_other_a0[1, , drop = FALSE]
          cat("  Utilisation de l'équation alternative A0 =", other_a0, "pour", essence_arbre, "\n")
        } else {
          # Si toujours aucune équation, utiliser la première disponible
          if (nrow(eq_candidates) > 0) {
            eq <- eq_candidates[1, , drop = FALSE]
            cat("  Utilisation de l'équation par défaut pour", essence_arbre, "\n")
          } else {
            warning(paste("Aucune équation trouvée pour l'essence", essence_arbre))
            next
          }
        }
      }
    } else {
      # Pour les autres types de volume, comportement standard
      if (local_id_equation > nrow(eq_candidates)) {
        warning(paste("L'équation avec id", local_id_equation, "n'existe pas pour l'essence", essence_arbre,
                      ". Utilisation de l'équation 1 à la place."))
        eq <- eq_candidates[1, , drop = FALSE]
      } else {
        eq <- eq_candidates[local_id_equation, , drop = FALSE]
      }
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

    # DEBUG: Afficher les détails de l'équation
    if (is.na(a0_value)) {
      warning(paste("Valeur A0 manquante pour l'essence", essence_arbre, "a la ligne", i))
      next
    } else if (a0_value %in% c(1, 2, 3, 5)) {
      # Pour les équations linéaires standard, commencer avec b0
      volume <- eq$b0[1]
      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)

        # Vérifier si l'expression X existe et est valide
        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          if (eq[[x_col]][1] == "/") {
            warning(paste("Expression invalide à la ligne", i, "pour X", j))
            next
          }

          # Capture explicitement la valeur retournée
          x_val <- tryCatch({
            evaluer_expression(eq[[x_col]][1], variables)
          }, error = function(e) {
            warning(paste("Erreur lors de l'évaluation de X", j, ":", e$message))
            NA
          })

          # Vérification sécurisée pour NA
          if (length(x_val) == 0 || is.na(x_val)) {
            warning(paste("L'évaluation de X", j, "a échoué à la ligne", i))
            next
          }

          b_val <- eq[[b_col]][1]
          if (is.na(b_val)) {
            warning(paste("Coefficient b", j, "manquant à la ligne", i))
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
        warning(paste("Valeur négative ou nulle pour logarithme à la ligne", i))
        next
      }
      volume <- 10^(eq$b0[1] + eq$b1[1] * log10(C130))
    } else {
      warning(paste("Type d'équation inconnu (A0 =", a0_value, ") pour la ligne", i))
      next
    }


    if (is.na(volume) || !is.finite(volume)) {
      warning(paste("Resultat de volume non valide a la ligne", i, ":", volume))
      next
    }

    # DEBUG: Afficher le volume calculé
    if (i <= 5) {
      cat("  Volume calcule:", volume, "\n")
    }

    # Stocker le volume uniquement pour cette ligne, dans la colonne spécifiée par type_volume
    df_result[[type_volume]][i] <- volume

    if (i <= 5) {
      cat("  Volume stocke a la ligne", i, ":", df_result[[type_volume]][i], "\n")
    }
  }

  # Nettoyage final si demande
  if (remove_na) {
    df_result <- df_result[!is.na(df_result[[type_volume]]), ]
  }

  return(df_result)
}
