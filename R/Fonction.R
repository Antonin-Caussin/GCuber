  #' Calculate different types of volumes from dendrometric data
  #'
  #' This function allows calculating different types of tree volumes from
  #' dendrometric data using various allometric equations. It handles the diversity
  #' of forest species, different measurement methods (C130, C150) and can adapt
  #' to different input data structures.
  #'
  #' @param df A data frame containing the dendrometric data of the trees.
  #' @param type_volume The type of volume to calculate. Valid values: "V22", "V22B", "E", "V22_HA". Default: "V22".
  #' @param id_equation The identifier of the equation to use for each species, representing the entry number. Default: 1.
  #' @param remove_na Boolean indicating whether rows with uncalculated volumes should be removed.
  #'        Default: FALSE.
  #' @param C130 Name of the column containing the circumference at 130 cm. Default: "C130".
  #' @param C150 Name of the column containing the circumference at 150 cm. Default: "C150".
  #' @param HTOT Name of the column containing the total height. Default: "HTOT".
  #' @param HDOM Name of the column containing the dominant height. Default: "HDOM".
  #' @param specimens Name of the column containing the species identifier (full name, code or abbreviation). Default: NULL.
  #'
  #' @details
  #' \subsection{Supported volume types}{
  #'   \itemize{
  #'     \item \strong{V22} : Merchantable volume up to a 22 cm circumference cut.
  #'     \item \strong{V22B} : Branch volume up to a 22cm circumference cut.
  #'     \item \strong{E} : Tree bark volume.
  #'     \item \strong{V22_HA} : Merchantable volume per hectare.
  #'   }
  #' }
  #'
  #' \subsection{Required input data structure}{
  #'   The function requires at minimum:
  #'   \itemize{
  #'     \item A forest species identification column (specified via \code{specimens}.
  #'     \item A diameter column (either \code{C130} or \code{C150}).
  #'   }
  #' }
  #'
  #' \subsection{Supported equation types (A0 values)}{
  #'   \enumerate{
  #'     \item Standard linear equation with 1 input: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
  #'     \item Standard linear equation with 2 inputs: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
  #'     \item Standard linear equation with 3 inputs: Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
  #'     \item Logarithmic equation : Volume = 10^(b0 + b1*log10(C130))
  #'     \item Standard linear equation with one input for bark volume : Volume = b0 + b1*X1 + b2*X2 + ... + b5*X5
  #'   }
  #' }
  #'
  #' @return A data frame similar to \code{df} with the following additional columns:
  #' \itemize{
  #'   \item The column specified by \code{type_volume} containing the calculated volumes.
  #'   \item \code{Equation_Utilisee} : Information about the equation used for each row.
  #'   \item If a C150 to C130 conversion was performed, a \code{C130} column is added.
  #'   \item If species mapping was necessary, a \code{Species} column is added.
  #' }
  #'
  #' @examples
  #' # Basic example with standard data
  #' # Suppose we have a data frame "donnees_arbres" with C130 and Essence columns
  #' \dontrun{
  #' resultats <- calculer_volumes(
  #'   df = donnees_arbres,
  #'   type_volume = "V22",
  #'   C130 = circ2024
  #' )
  #'
  #' # Example with custom column names
  #' resultats <- calculer_volumes(
  #'   df = donnees_arbres,
  #'   type_volume = "E",
  #'   id_equation = 2,
  #'   C130 = "Circonference130",
  #'   HTOT = "HauteurTotale",
  #'   specimens = "NomEssence"
  #' )
  #'
  #' # Example with C150 to C130 conversion
  #' resultats <- calculer_volumes(
  #'   df = donnees_arbres,
  #'   type_volume = "V22",
  #'   C150 = "Circ150"
  #' )
  #' }
  #'
  #' @note
  #' The function displays information messages during execution to facilitate debugging.
  #' Warnings are issued if species correspondences are not found or if
  #' volume calculation fails for certain rows.
  #'
  #' @seealso
  #' Related functions for forest management and dendrometric calculations.
  #'
  #' @author Caussin Antonin
  #' @references
  #' Dagnelie, P., Rondeux, J., & Thill, A. (1985). Tables de cubage des arbres et des peuplements forestiers. Gembloux, Belgique: Presses agronomiques de Gembloux.
  #'
  #' @importFrom stats na.omit
  #' @export

calculer_volumes <- function(df, type_volume = "V22",
                             id_equation = 1,
                             specimens = NULL,
                             C130 = "C130",
                             C150 = "C150",
                             D130 = "D130",
                             D150 = "D150",
                             HTOT = "HTOT",
                             HDOM = "HDOM",
                             remove_na = FALSE) {

  # =========================================================================
  # FONCTIONS INTERNES
  # =========================================================================

  # Validation des paramètres d'entrée
  valider_parametres <- function() {
    # Avertissement pour le type E avec id_equation inutile
    if (type_volume == "E" && id_equation %in% c(1, 2, 3, 4, 5)) {
      warning(paste("WARNING: For volume type 'E', it is not necessary to specify id_equation.",
                    "You have specified id_equation =", id_equation,
                    "which might not be suitable or might not process the entire dataset."))
    }

    # Vérification du type_volume
    types_volume_valides <- c("V22", "V22B", "E", "V22_HA")
    if (!(type_volume %in% types_volume_valides)) {
      stop(paste("Invalid volume type:", type_volume,
                 "\nValid types:", paste(types_volume_valides, collapse = ", ")))
    }

    # Correspondance entre type_volume et id_equation
    if (type_volume == "V22" && !(id_equation %in% 1:3)) {
      stop("For volume type 'V22', id_equation must be between 1 and 3.")
    }
    if (type_volume == "V22_HA" && id_equation != 1) {
      stop("For volume type 'V22_HA', id_equation must be 1.")
    }
    if (type_volume == "V22B" && id_equation != 1) {
      stop("For volume type 'V22B', id_equation must be 1.")
    }

    # Vérification de l'existence des colonnes dans le dataframe utilisateur
    colonnes_requises <- c(C130, C150, D130, D150, HTOT, HDOM, specimens)
    colonnes_presentes <- colonnes_requises[colonnes_requises %in% colnames(df)]
    colonnes_manquantes <- setdiff(colonnes_requises, colonnes_presentes)

    # Affectation des flags globaux
    assign("C130_exists", C130 %in% colnames(df), envir = .GlobalEnv)
    assign("C150_exists", C150 %in% colnames(df), envir = .GlobalEnv)
    assign("D130_exists", D130 %in% colnames(df), envir = .GlobalEnv)
    assign("D150_exists", D150 %in% colnames(df), envir = .GlobalEnv)
    assign("HTOT_exists", HTOT %in% colnames(df), envir = .GlobalEnv)
    assign("HDOM_exists", HDOM %in% colnames(df), envir = .GlobalEnv)

    # Vérification des couples circonférence / diamètre
    if (!C130_exists && !C150_exists && !D130_exists && !D150_exists) {
      stop("Aucune colonne de circonférence (C130 ou C150) ni de diamètre (D130 ou D150) trouvée dans les données.")
    }

    if ((!C130_exists && !C150_exists) && (D130_exists || D150_exists)) {
      message("Aucune circonférence (C130 ou C150) n'est disponible, mais des diamètres sont présents.")
    }

    if ((C130_exists || C150_exists) && (!D130_exists && !D150_exists)) {
      message("Des circonférences sont présentes, mais aucun diamètre (D130 ou D150) n'a été trouvé.")
    }

    if (length(colonnes_manquantes) > 0) {
      warning(paste("Colonnes manquantes dans les données :", paste(colonnes_manquantes, collapse = ", ")))
    }
    #debug
    cat("[DEBUG] ==================== VALIDATION DES PARAMÈTRES ====================\n")
    cat("[DEBUG] Paramètres reçus:\n")
    cat("[DEBUG]   - type_volume =", type_volume, "\n")
    cat("[DEBUG]   - id_equation =", id_equation, "\n")
    cat("[DEBUG]   - specimens =", ifelse(is.null(specimens), "NULL", specimens), "\n")
    cat("[DEBUG]   - remove_na =", remove_na, "\n")
    cat("[DEBUG] Dimensions du dataframe d'entrée: [", nrow(df), "x", ncol(df), "]\n")
    cat("[DEBUG] Colonnes disponibles:", paste(colnames(df), collapse = ", "), "\n")
    cat("[DEBUG] Colonnes requises:", paste(colonnes_requises, collapse = ", "), "\n")
    cat("[DEBUG] Flags de colonnes définis:\n")
    cat("[DEBUG]   - C130_exists =", C130_exists, "\n")
    cat("[DEBUG]   - C150_exists =", C150_exists, "\n")
    cat("[DEBUG]   - D130_exists =", D130_exists, "\n")
    cat("[DEBUG]   - D150_exists =", D150_exists, "\n")
    cat("[DEBUG]   - HTOT_exists =", HTOT_exists, "\n")
    cat("[DEBUG]   - HDOM_exists =", HDOM_exists, "\n")
    if (length(colonnes_manquantes) > 0) {
      cat("[DEBUG] ⚠️  Colonnes manquantes:", paste(colonnes_manquantes, collapse = ", "), "\n")
    }
    cat("[DEBUG] ✅ Validation des paramètres terminée\n\n")
  }

  # Détection du type d'identification des espèces
  detecter_type_specimens <- function(specimens) {
    sample_values <- na.omit(df[[specimens]])

    if (length(sample_values) == 0) {
      stop(paste("Column '", specimens, "' contains only missing values.", sep=""))
    }

    if (is.numeric(sample_values)) {
      type_specimens <- "Code"
    } else if (is.character(sample_values) || is.factor(sample_values)) {
      if (is.factor(sample_values)) {
        sample_values <- as.character(sample_values)
      }

      mean_length <- mean(nchar(sample_values))
      type_specimens <- if (mean_length <= 4) "Abr" else "Essence"
    } else {
      stop(paste("Data type in column '", specimens, "' is not recognized.", sep=""))
    }

    #debug
    cat("[DEBUG] ==================== DÉTECTION TYPE SPECIMENS ====================\n")
    cat("[DEBUG] Colonne specimens analysée:", specimens, "\n")
    cat("[DEBUG] Type de données R:", class(df[[specimens]]), "\n")
    cat("[DEBUG] Nombre de valeurs non-NA:", length(sample_values), "\n")
    cat("[DEBUG] Échantillon de valeurs:", paste(head(sample_values, 3), collapse = ", "), "\n")
    if (is.character(sample_values) || is.factor(sample_values)) {
      cat("[DEBUG] Longueur moyenne des caractères:", round(mean_length, 2), "\n")
    }
    cat("[DEBUG] Type d'identification détecté:", type_specimens, "\n")
    cat("[DEBUG] ✅ Détection du type specimens terminée\n\n")

    return(type_specimens)
  }

  # Établissement de la correspondance des espèces
  etablir_correspondance_especes <- function(df) {
    if (is.null(specimens)) {
      stop("No species identification column specified or found in the data.")
    }

    if (!(specimens %in% colnames(df))) {
      stop(paste("The specified column '", specimens, "' does not exist in the data.", sep=""))
    }

    type_specimens <- detecter_type_specimens(specimens)

    df_result <- df

    # Add Species column if necessary
    if (type_specimens != "Essence" || specimens != "Essence") {
      # Check required columns in equations_df
      if (!all(c("Essences", type_specimens) %in% colnames(equations_df))) {
        stop(paste("The 'equations_df' dataframe must contain the columns 'Essences' and '",
                   type_specimens, "'", sep=""))
      }

      # Create mapping
      mapping_df <- unique(equations_df[, c("Essences", type_specimens)])
      names(mapping_df) <- c("Species", specimens)

      # Merge
      df_result <- merge(df_result, mapping_df, by = specimens, all.x = TRUE)

      # Clean duplicated columns
      if ("Species.x" %in% colnames(df_result)) {
        names(df_result)[names(df_result) == "Species.x"] <- "Species"
        if ("Species.y" %in% colnames(df_result)) {
          df_result$Species.y <- NULL
        }
      }

      # Check for missing correspondences
      if (any(is.na(df_result$Species))) {
        na_values <- unique(df_result[[specimens]][is.na(df_result$Species)])
        warning(paste("No correspondence found for the following values of",
                      specimens, ":", paste(na_values, collapse=", ")))
      }
    } else {
      names(df_result)[names(df_result) == "Essence"] <- "Species"
    }

    #debug
    cat("[DEBUG] ==================== CORRESPONDANCE ESPÈCES ====================\n")
    cat("[DEBUG] Colonne specimens utilisée:", specimens, " (type:", type_specimens, ")\n")
    cat("[DEBUG] Dimensions equations_df: [", nrow(equations_df), "x", ncol(equations_df), "]\n")
    cat("[DEBUG] Colonnes equations_df:", paste(colnames(equations_df), collapse = ", "), "\n")
    cat("[DEBUG] Nombre de lignes avant merge:", nrow(df), "\n")
    if (exists("mapping_df")) {
      cat("[DEBUG] Correspondances créées:", nrow(mapping_df), "\n")
      cat("[DEBUG] Aperçu mapping:\n")
      print(head(mapping_df, 3))
    }
    cat("[DEBUG] Nombre de lignes après merge:", nrow(df_result), "\n")
    valeurs_sans_correspondance <- unique(df_result[[specimens]][is.na(df_result$Species)])
    if (length(valeurs_sans_correspondance) > 0) {
      cat("[DEBUG] ⚠️  Valeurs sans correspondance (", length(valeurs_sans_correspondance), "):",
          paste(head(valeurs_sans_correspondance, 5), collapse = ", "), "\n")
    }
    cat("[DEBUG] ✅ Correspondance espèces terminée\n\n")

    return(df_result)
  }

  # Gestion des conversions de diamètres
  conversions_diametre <- function(df_result) {
    pi_val <- pi

    # Initialisation des colonnes cible si elles n'existent pas
    if (D130_exists && (!C130 %in% names(df_result))) df_result$C130 <- NA_real_
    if (D150_exists && (!C150 %in% names(df_result))) df_result$C150 <- NA_real_

    #debug
    cat("[DEBUG] ==================== CONVERSIONS DIAMÈTRE ====================\n")
    cat("[DEBUG] Conversions nécessaires:\n")
    if (D130_exists) {
      na_avant_D130 <- sum(is.na(df_result[[D130]]))
      cat("[DEBUG]   - D130 → C130: ", nrow(df_result) - na_avant_D130, " valeurs à convertir\n")
      cat("[DEBUG] Exemple D130:", head(df_result[[D130]], 5), "\n")
    }
    if (D150_exists) {
      na_avant_D150 <- sum(is.na(df_result[[D150]]))
      cat("[DEBUG]   - D150 → C150: ", nrow(df_result) - na_avant_D150, " valeurs à convertir\n")
      cat("[DEBUG] Exemple D150:", head(df_result[[D150]], 5), "\n")
    }
    cat("[DEBUG] Coefficient π utilisé:", round(pi, 6), "\n")

    for (i in seq_len(nrow(df_result))) {
      # ==== D130 -> C130 ====
      if (D130_exists && !is.na(df_result[[D130]][i])) {
        df_result$C130[i] <- df_result[[D130]][i] * pi_val
      }

      # ==== D150 -> C150 ====
      if (D150_exists && !is.na(df_result[[D150]][i])) {
        df_result$C150[i] <- df_result[[D150]][i] * pi_val
      }
    }

    #debug
    if (D130_exists && "C130" %in% colnames(df_result)) {
      conversions_reussies_130 <- sum(!is.na(df_result$C130) & D130_exists && !is.na(df_result[[D130]]))
      cat("[DEBUG] ✅ Conversions D130→C130 réussies:", conversions_reussies_130, "\n")
    }
    if (D150_exists && "C150" %in% colnames(df_result)) {
      conversions_reussies_150 <- sum(!is.na(df_result$C150) & D150_exists && !is.na(df_result[[D150]]))
      cat("[DEBUG] ✅ Conversions D150→C150 réussies:", conversions_reussies_150, "\n")
    }
    cat("[DEBUG] ✅ Conversions diamètre terminées\n\n")

    # Messages d'avertissement si certaines conversions échouent
    if (D130_exists && sum(is.na(df_result$C130)) > 0 && sum(!is.na(df_result[[D130]])) > 0) {
      warning("Certaines conversions D130 -> C130 ont échoué (valeurs manquantes ou non traitées).")
    }
    if (D150_exists && sum(is.na(df_result$C150)) > 0 && sum(!is.na(df_result[[D150]])) > 0) {
      warning("Certaines conversions D150 -> C150 ont échoué (valeurs manquantes ou non traitées).")
    }

    #debug
    if ("C130" %in% colnames(df_result)) {
      cat("[DEBUG] Exemple résultats C130:", head(df_result$C130, 5), "\n")
    }
    if ("C150" %in% colnames(df_result)) {
      cat("[DEBUG] Exemple résultats C150:", head(df_result$C150, 5), "\n")
    }
    cat("Conversion diamètre → circonférence terminée.\n")

    # Mise à jour des flags globaux
    assign("C130_exists", "C130" %in% colnames(df_result), envir = .GlobalEnv)
    assign("C150_exists", "C150" %in% colnames(df_result), envir = .GlobalEnv)

    return(df_result)
  }

  # Conversion générique des circonférences
  convertir_circonference <- function(df_result, C150, C130, from_col = NULL, to_col = NULL, direction = NULL) {
    # Détection automatique du sens de conversion si non spécifié
    if (is.null(from_col) || is.null(to_col) || is.null(direction)) {
      C130_exists_local <- "C130" %in% colnames(df_result)
      C150_exists_local <- C150 %in% colnames(df_result)

      if (!C130_exists_local && C150_exists_local) {
        from_col <- C150
        to_col <- "C130"
        direction <- "C150_to_C130"
      } else if (!C150_exists_local && C130_exists_local) {
        from_col <- "C130"
        to_col <- C150
        direction <- "C130_to_C150"
      } else {
        stop("Impossible de déterminer automatiquement le sens de conversion. Vérifiez la présence de C130 ou C150.")
      }
    }

    #debug
    cat("[DEBUG] ==================== CONVERSIONS CIRCONFÉRENCE ====================\n")
    cat("[DEBUG] Sens de conversion détecté:", direction, "\n")
    cat("[DEBUG] Colonne source:", from_col, "\n")
    cat("[DEBUG] Colonne cible:", to_col, "\n")

    # Extraction des coefficients de conversion (HV et IV)
    coefs_df <- unique(equations_df[, c("Essences", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Species"

    cat("[DEBUG] Coefficients disponibles pour", nrow(coefs_df), "espèces\n")
    cat("[DEBUG] Aperçu coefficients HV/IV:\n")
    print(head(coefs_df[, c("Species", "HV", "IV")], 3))

    cat("Conversion coefficients preview:\n")
    print(head(coefs_df))

    # Initialisation de la colonne cible
    df_result[[to_col]] <- NA_real_

    # Application ligne par ligne
    conversions_tentees <- 0
    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]

      if (!is.na(essence_arbre)) {
        from_value <- df_result[[from_col]][i]

        if (!is.na(from_value)) {
          conversions_tentees <- conversions_tentees + 1
          coef_row <- coefs_df[coefs_df$Species == essence_arbre, ]

          if (i <= 5) {
            cat("Row", i, "- Species:", essence_arbre, "- Coefficients found:", nrow(coef_row), "\n")
            if (nrow(coef_row) > 0) {
              cat("  HV:", coef_row$HV[1], "IV:", coef_row$IV[1], "\n")
            }
          }

          if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
            HV_coef <- coef_row$HV[1]
            IV_coef <- coef_row$IV[1]

            result_value <- if (direction == "C150_to_C130") {
              HV_coef * from_value + IV_coef
            } else {
              (from_value - IV_coef) / HV_coef
            }

            df_result[[to_col]][i] <- result_value

            if (i <= 5) {
              cat("  Conversion:", from_value, "->", result_value, "\n")
            }
          } else {
            warning(paste("Unable to convert", from_col, "to", to_col, "for species:",
                          essence_arbre, "at row", i, ". Missing coefficients."))
          }
        }
      }
    }

    #debug
    conversions_reussies <- sum(!is.na(df_result[[to_col]]))
    cat("[DEBUG] Conversions à tenter:", conversions_tentees, "\n")
    cat("[DEBUG] ✅ Conversions", from_col, "→", to_col, "réussies:", conversions_reussies, "\n")
    if (conversions_tentees > conversions_reussies) {
      cat("[DEBUG] ⚠️  Conversions échouées:", conversions_tentees - conversions_reussies, "\n")
    }

    # Vérification des conversions manquées
    failed_conversions <- sum(is.na(df_result[[to_col]]))
    if (failed_conversions > 0) {
      warning(paste(failed_conversions, paste(from_col, "to", to_col),
                    "conversions failed. Check the data."))
    }

    # Calcul ligne par ligne de D130
    if (!D130_exists && "C130" %in% colnames(df_result)) {
      df_result$D130 <- NA_real_
      for (i in seq_len(nrow(df_result))) {
        if (!is.na(df_result$C130[i])) {
          df_result$D130[i] <- df_result$C130[i] / pi
          if (i <= 5) cat("D130 ligne", i, ":", df_result$D130[i], "\n")
        }
      }
    }

    # Calcul ligne par ligne de D150
    if (!D150_exists && C150 %in% colnames(df_result)) {
      df_result$D150 <- NA_real_
      for (i in seq_len(nrow(df_result))) {
        if (!is.na(df_result[[C150]][i])) {
          df_result$D150[i] <- df_result[[C150]][i] / pi
          if (i <= 5) cat("D150 ligne", i, ":", df_result$D150[i], "\n")
        }
      }
    }

    #debug
    if ("D130" %in% colnames(df_result) && !"D130" %in% colnames(df)) {
      cat("[DEBUG] ✅ Colonne D130 créée à partir de C130\n")
    }
    if ("D150" %in% colnames(df_result) && !"D150" %in% colnames(df)) {
      cat("[DEBUG] ✅ Colonne D150 créée à partir de C150\n")
    }
    cat("[DEBUG] ✅ Conversions circonférence terminées\n\n")

    return(df_result)
  }

  # Calcul des surfaces terrières
  calculer_surfaces_terrieres <- function(df_result) {
    #debug
    cat("[DEBUG] ==================== CALCUL SURFACES TERRIÈRES ====================\n")
    surfaces_calculees <- c()

    # Calculate G130 if necessary
    if (!"G130" %in% colnames(df_result) && "C130" %in% colnames(df_result)) {
      df_result$G130 <- (df_result$C130^2) / ((4 * pi) * 10000)
      valeurs_g130 <- sum(!is.na(df_result$G130))
      cat("[DEBUG] ✅ G130 calculée pour", valeurs_g130, "arbres\n")
      cat("[DEBUG] Exemple G130:", paste(round(head(df_result$G130[!is.na(df_result$G130)], 3), 6), collapse = ", "), "\n")
      surfaces_calculees <- c(surfaces_calculees, "G130")
    }

    # Calculate G150 if necessary
    C150_exists_local <- C150 %in% colnames(df_result)
    if (!"G150" %in% colnames(df_result) && C150_exists_local) {
      df_result$G150 <- (df_result[[C150]]^2) / ((4 * pi) * 10000)
      valeurs_g150 <- sum(!is.na(df_result$G150))
      cat("[DEBUG] ✅ G150 calculée pour", valeurs_g150, "arbres\n")
      cat("[DEBUG] Exemple G150:", paste(round(head(df_result$G150[!is.na(df_result$G150)], 3), 6), collapse = ", "), "\n")
      surfaces_calculees <- c(surfaces_calculees, "G150")
    }

    if (length(surfaces_calculees) == 0) {
      cat("[DEBUG] ℹ️  Aucune surface terrière à calculer (déjà présentes)\n")
    } else {
      cat("[DEBUG] Surfaces terrières calculées:", paste(surfaces_calculees, collapse = ", "), "\n")
    }
    cat("[DEBUG] Formule utilisée: G = C²/(4π×10000)\n")
    cat("[DEBUG] ✅ Calcul des surfaces terrières terminé\n\n")

    return(df_result)
  }

  # =========================================================================
  # EXECUTION PRINCIPALE
  # =========================================================================

  # 1. Validation des paramètres
  valider_parametres()

  # 2. Initialisation
  equations_df <- equations
  cat("Selected volume type:", type_volume, "\n")

  # 3. Établissement des correspondances espèces
  df_result <- etablir_correspondance_especes(df)

  # 4. Gestion des conversions de diamètres
  df_result <- conversions_diametre(df_result)

  # 5. Conversion des circonférences si nécessaire
  if ((!C130_exists && C150_exists) || (!C150_exists && C130_exists)) {
    df_result <- convertir_circonference(df_result, C150, C130)
  }

  # 6. Calcul des surfaces terrières
  df_result <- calculer_surfaces_terrieres(df_result)

  cat("[DEBUG] Début fonction calculer_volumes\n")
  cat("[DEBUG] Nombre de lignes du dataframe d'entrée:", nrow(df), "\n")
  cat("[DEBUG] Colonnes du dataframe d'entrée:", paste(colnames(df), collapse = ", "), "\n")

  # =========================================================================
  # Calcul des volumes
  # =========================================================================

    # Filter equations
    eqs_volume <- equations_df[equations_df$Y == type_volume, ]
    # DEBUG: Display found equations
    cat("Number of equations found for", type_volume, ":", nrow(eqs_volume), "\n")
    cat("Preview of available equations:\n")
    print(head(eqs_volume[, c("Essences", "Y", "A0")]))

    if (nrow(eqs_volume) == 0) {
      stop(paste("No equation found for volume type:", type_volume))
    }

    # Ensure that coefficients b0 to b5 are numeric
    colonnes_b <- paste0("b", 0:5)
    eqs_volume[colonnes_b] <- lapply(eqs_volume[colonnes_b], as.numeric)

    # Initialization
    df_result$Equation_Utilisee <- NA_character_

    # Initialize the volume column specified by the user
    if (!(type_volume %in% names(df_result))) {
      df_result[[type_volume]] <- NA_real_
    }

    # Modification of the evaluer_expression function for more debugging
    evaluer_expression <- function(expr_text, variables) {
      if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)

      # Check if the expression is syntactically valid
      if (expr_text == "/") {
        warning("Invalid expression detected: '/'")
        return(NA)
      }

      # Check that all necessary variables are present and not NA
      var_names <- all.vars(parse(text = expr_text))
      for (v in var_names) {
        if (!v %in% names(variables) || is.na(variables[[v]])) {
          warning(paste("Missing or NA variable:", v, "for expression:", expr_text))
          return(NA)
        }
      }

      env <- list2env(variables)
      tryCatch({
        eval(parse(text = expr_text), envir = env)
      }, error = function(e) {
        warning(paste("Error during expression evaluation:", expr_text, "-", e$message))
        return(NA)
      })
    }

    # Calculation for each row
    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]  # Using Species instead of Essence

      # Initialize volume variable for this row
      volume <- 0

      # For volume type "E", dynamically determine the equation to use
      # according to species, always equation 4 or 5 specific to each species
      if (type_volume == "E") {
        # Find all available equations for this species and volume type
        eq_candidates_E <- eqs_volume[eqs_volume$Essences == essence_arbre, ]

        # Filter only equations with A0 = 4 or A0 = 5
        eq_candidates_E_filtered <- eq_candidates_E[eq_candidates_E$A0 %in% c(4, 5), ]

        if (nrow(eq_candidates_E_filtered) > 0) {
          # Determine which equation (4 or 5) is associated with this species
          if (any(eq_candidates_E_filtered$A0 == 4)) {
            local_id_equation <- 4
            cat(" Using equation A0 = 4 for", essence_arbre, "\n")
          } else {
            local_id_equation <- 5
            cat(" Using equation A0 = 5 for", essence_arbre, "\n")
          }
        } else {
          # If no equation 4 or 5 is found for this species, keep the original id_equation
          cat(" No equation of type 4 or 5 found for", essence_arbre, ". Using default equation.\n")
        }
      } else {
        # For other volume types, keep the equation specified by the user
        local_id_equation <- id_equation
      }

      eq_candidates <- eqs_volume[eqs_volume$Essences == essence_arbre, ]

      # DEBUG: Display candidate equations for this species
      if (i <= 5) {
        cat("  Number of candidate equations:", nrow(eq_candidates), "\n")
      }

      if (nrow(eq_candidates) == 0) {
        # If no specific equation is found, warning.
        warning(paste("No equation found for species:", essence_arbre))
        next
        }

      # For type "E", use the equation with A0 corresponding to local_id_equation
      if (type_volume == "E" && local_id_equation %in% c(4, 5)) {
        eq_by_a0 <- eq_candidates[eq_candidates$A0 == local_id_equation, ]
        if (nrow(eq_by_a0) > 0) {
          eq <- eq_by_a0[1, , drop = FALSE]  # Take the first equation if several match
          cat("  Using equation A0 =", local_id_equation, "for", essence_arbre, "\n")
        } else {
          # If no equation with the specific A0 is found
          warning(paste("No equation with A0 =", local_id_equation, "found for species", essence_arbre))

          # Try with the other equation type (4 or 5)
          other_a0 <- if (local_id_equation == 4) 5 else 4
          eq_by_other_a0 <- eq_candidates[eq_candidates$A0 == other_a0, ]

          if (nrow(eq_by_other_a0) > 0) {
            eq <- eq_by_other_a0[1, , drop = FALSE]
            cat("  Using alternative equation A0 =", other_a0, "for", essence_arbre, "\n")
          } else {
            # If still no equation, use the first available
            if (nrow(eq_candidates) > 0) {
              eq <- eq_candidates[1, , drop = FALSE]
              cat("  Using default equation for", essence_arbre, "\n")
            } else {
              warning(paste("No equation found for species", essence_arbre))
              next
            }
          }
        }
      } else {
        # For other volume types, standard behavior
        if (local_id_equation > nrow(eq_candidates)) {
          warning(paste("Equation with id", local_id_equation, "does not exist for species", essence_arbre,
                        ". Using equation 1 instead."))
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
          stop(paste("Variable", v, "is used in an equation but missing from the data."))
        }
      }

      a0_value <- eq$A0[1]

      # DEBUG: Display equation details
      if (is.na(a0_value)) {
        warning(paste("Missing A0 value for species", essence_arbre, "at row", i))
        next
      } else if (a0_value %in% c(1, 2, 3, 5)) {
        # For standard linear equations, start with b0
        volume <- eq$b0[1]
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
              evaluer_expression(eq[[x_col]][1], variables)
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
            volume <- volume + b_val * x_val
          }
        }
      } else if (a0_value == 4) {
        C130 <- evaluer_expression(eq$X1[1], variables)
        if (C130 <= 0) {
          warning(paste("Negative or zero value for logarithm at row", i))
          next
        }
        volume <- 10^(1*eq$b0[1] + eq$b1[1] * log10(C130))
      } else {
        warning(paste("Unknown equation type (A0 =", a0_value, ") for row", i))
        next
      }


      if (is.na(volume) || !is.finite(volume)) {
        warning(paste("Invalid volume result at row", i, ":", volume))
        next
      }

      # DEBUG: Display calculated volume
      if (i <= 5) {
        cat("  Calculated volume:", volume, "\n")
      }

      # Store volume only for this row, in the column specified by type_volume
      df_result[[type_volume]][i] <- volume

      if (i <= 5) {
        cat("  Volume stored at row", i, ":", df_result[[type_volume]][i], "\n")
      }
    }

    # Final cleanup if requested
    if (remove_na) {
      df_result <- df_result[!is.na(df_result[[type_volume]]), ]
    }

    return(df_result)

}

