#' Calcule la biomasse aérienne et le carbone associé à partir des volumes et coefficients
#'
#' @param donnees_arbres Un data frame contenant les colonnes 'Essence', 'VC22' ou 'VC22B'
#' @param essence Essence ciblée (ou NULL pour utiliser celle de chaque ligne)
#' @param equations_df Le data frame contenant les coefficients BEF, EF et Carbone pour chaque essence
#' @param colonne_volume Nom de la colonne de volume à utiliser (par défaut NULL - utilise celle spécifiée dans equations_df)
#' @param afficher_warnings Booléen indiquant si les warnings doivent être affichés (TRUE) ou stockés (FALSE)
#' @return Le data frame d'entrée avec les colonnes supplémentaires 'Biomasse', 'Carbone' et 'Messages' si afficher_warnings=FALSE
#' @export
#'
#' @examples
#' donnees <- data.frame(
#'   Essence = c("Chene", "Hetre", "Pin"),
#'   VC22 = c(120, 150, 100)
#' )
#'
#' equations <- data.frame(
#'   Y = rep("Biomasse", 3),
#'   Essences = c("Chene", "Hetre", "General"),
#'   BEF = c(0.7, 0.8, 0.75),
#'   EF = c(0.9, 0.85, 0.88),
#'   Carbone = c(0.5, 0.48, 0.49),
#'   VC_source = c("VC22", "VC22", "VC22")
#' )
#'
#' calculer_biomasse(donnees, equations_df = equations)
calculer_biomasse <- function(donnees_arbres,
                              essence = NULL,
                              equations_df = equations_allometriques,
                              colonne_volume = NULL,
                              afficher_warnings = TRUE) {

  # Vérification de la colonne Essence
  if (!"Essence" %in% names(donnees_arbres)) {
    stop("La colonne 'Essence' est requise dans les données")
  }

  # Vérification des colonnes requises dans equations_df
  colonnes_requises <- c("Y", "Essences", "BEF", "EF", "Carbone")
  manquantes <- colonnes_requises[!colonnes_requises %in% names(equations_df)]
  if (length(manquantes) > 0) {
    stop(paste("Colonnes manquantes dans equations_df:", paste(manquantes, collapse = ", ")))
  }

  # Initialisation des colonnes de résultats
  donnees_arbres$Biomasse <- NA
  donnees_arbres$Carbone <- NA

  if (!afficher_warnings) {
    donnees_arbres$Messages <- ""
  }

  for (i in 1:nrow(donnees_arbres)) {
    # Déterminer l'essence à utiliser
    ess <- if (is.null(essence)) donnees_arbres$Essence[i] else essence

    # Message d'erreur
    message_erreur <- NULL

    # Chercher l'équation correspondant à l'essence
    eq <- equations_df[equations_df$Y == "Biomasse" & equations_df$Essences == ess, ]
    if (nrow(eq) == 0) {
      eq <- equations_df[equations_df$Y == "Biomasse" & equations_df$Essences == "General", ]
    }

    if (nrow(eq) == 0) {
      message_erreur <- paste("Pas d'équation de biomasse pour l'essence:", ess)
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[i] <- paste0(donnees_arbres$Messages[i], message_erreur, "; ")
      }
      next
    }

    eq <- eq[1, ]

    # Déterminer quelle colonne de volume utiliser (VC22 ou VC22B)
    volume_col <- colonne_volume
    if (is.null(volume_col)) {
      volume_col <- if (!is.null(eq$VC_source)) eq$VC_source else "VC22"
    }

    if (!(volume_col %in% names(donnees_arbres))) {
      message_erreur <- paste("Colonne de volume", volume_col, "absente pour l'essence:", ess)
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[i] <- paste0(donnees_arbres$Messages[i], message_erreur, "; ")
      }
      next
    }

    volume <- donnees_arbres[[volume_col]][i]
    bef <- eq$BEF
    ef <- eq$EF
    carbone_facteur <- eq$Carbone

    if (is.na(volume) || is.na(bef) || is.na(ef)) {
      message_erreur <- paste("Valeurs manquantes pour le calcul de la biomasse (essence:", ess, ")")
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[i] <- paste0(donnees_arbres$Messages[i], message_erreur, "; ")
      }
      next
    }

    # Vérification des valeurs négatives ou nulles
    if (volume <= 0 || bef <= 0 || ef <= 0) {
      message_erreur <- paste("Valeurs invalides (≤ 0) pour le calcul de la biomasse (essence:", ess, ")")
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[i] <- paste0(donnees_arbres$Messages[i], message_erreur, "; ")
      }
      next
    }

    biomasse <- volume * bef * ef
    donnees_arbres$Biomasse[i] <- biomasse

    if (!is.na(carbone_facteur)) {
      donnees_arbres$Carbone[i] <- biomasse * carbone_facteur
    }
  }

  return(donnees_arbres)
}

