#' Calcule différents types de volumes d'arbres à partir de données dendrométriques
#'
#' Cette fonction permet de calculer différents types de volumes d'arbres à partir de données
#' dendrométriques en utilisant dIVerses équations allométriques. Elle gère la dIVersité
#' des essences forestières, les différentes méthodes de mesure (C130, C150) et peut s'adapter
#' à différentes structures de données d'entrée.
#'
#' @param df Un data frame contenant les données dendrométriques des arbres.
#' @param type_volume Le type de volume à calculer. Valeurs valides : "VC22", "VC22B", "E", "VC22_HA". Par défaut : "VC22".
#' @param essence Optionnel. Si spécifié, les calculs seront effectués uniquement avec les équations correspondant
#'        à cette essence. Par défaut : NULL (utilise les essences indiquées dans les données).
#' @param equations_df Un data frame contenant les équations allométriques à utiliser. Par défaut : le data frame interne "equations"
#' @param id_equation L'identifiant de l'équation à utiliser pour chaque essence. Par défaut : 1.
#' @param coefs_conversion Table de conversion entre C150 et C130. Requis si les données contiennent C150
#'        mais pas C130. Par défaut : NULL.
#' @param remove_na Booléen indiquant si les lignes avec des volumes non calculés doIVent être supprimées.
#'        Par défaut : FALSE.
#' @param C130 Nom de la colonne contenant la circonférence à 130 cm. Par défaut : "C130".
#' @param C150 Nom de la colonne contenant la circonférence à 150 cm. Par défaut : "C150".
#' @param HTOT Nom de la colonne contenant la hauteur totale. Par défaut : "HTOT".
#' @param HDOM Nom de la colonne contenant la hauteur dominante. Par défaut : "HDOM".
#' @param specimens Optionnel. Nom de la colonne contenant l'identifiant des essences
#'        (nom complet, code ou abréviation). Par défaut : NULL.
#'
#' @details
#' \subsection{Types de volume supportés}{
#'   \itemize{
#'     \item \strong{VC22} : Volume commercial jusqu'à une découpe de 22 cm de circonférence.
#'     \item \strong{VC22B} : Variante du volume commercial.
#'     \item \strong{E} : Volume total de l'arbre.
#'     \item \strong{VC22_HA} : Volume commercial par hectare.
#'   }
#' }
#'
#' \subsection{Structure des données d'entrée requise}{
#'   La fonction nécessite au minimum :
#'   \itemize{
#'     \item Une colonne d'identification des essences forestières (spécifiée via \code{specimens}
#'           ou par défaut "Essence", "Code" ou "Abr").
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
#' @return Un data frame similaire à \code{df} avec les colonnes supplémentaires suIVantes :
#' \itemize{
#'   \item La colonne spécifiée par \code{type_volume} contenant les volumes calculés.
#'   \item \code{Equation_Utilisee} : Information sur l'équation utilisée pour chaque ligne.
#'   \item Si une conversion C150 à C130 a été effectuée, une colonne \code{C130} est ajoutée.
#'   \item Si le mapping d'essence a été nécessaire, une colonne \code{Essence} est ajoutée.
#' }
#'
#' @examples
#' # Exemple de base avec données standard
#' # Supposons que nous avons un data frame "donnees_arbres" avec des colonnes C130 et Essence
#' \dontrun{
#' resultats <- calculer_volumes(
#'   df = donnees_arbres,
#'   type_volume = "VC22"
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
#'   type_volume = "VC22",
#'   C150 = "Circ150"
#' )
#' }
#'
#' @note
#' La fonction affiche des messages d'information pendant l'exécution pour faciliter le débogage.
#' Des avertissements sont émis si des correspondances d'essences ne sont pas trouvées ou si
#' le calcul du volume échoue pour certaines lignes.
#' Pour les essences non trouvées dans les équations, la fonction tente d'utiliser une équation
#' générique ("General").
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

calculer_volumes <- function(df, type_volume = "VC22", essence = NULL,
                             equations_df = equations, id_equation = 1,
                              remove_na = FALSE,
                             C130 = "C130", C150 = "C150",
                             HTOT = "HTOT", HDOM = "HDOM",
                             specimens = NULL) {

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
        warning(paste("Valeur negatIVe ou nulle pour logarithme a la ligne", i))
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
    df_result[[type_volume]][i] <- volume

    cat("  Volume stocke a la ligne", i, ":", df_result[[type_volume]][i], "\n")
  }

  # Nettoyage final si demande
  if (remove_na) {
    df_result <- df_result[!is.na(df_result[[type_volume]]), ]
  }

  return(df_result)
}
