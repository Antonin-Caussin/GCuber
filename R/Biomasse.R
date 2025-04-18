#' Calcule la biomasse aérienne et le carbone associé à partir des volumes et coefficients
#'
#' @param donnees_arbres Un data frame contenant les colonnes 'Essence', 'VC22' ou 'VC22B'
#' @param essence Essence ciblée (ou NULL pour utiliser celle de chaque ligne)
#' @param colonne_volume Nom de la colonne de volume à utiliser (NULL utilise celle spécifiée dans les équations internes)
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
#' calculer_biomasse(donnees)
calculer_biomasse <- function(donnees_arbres,
                              essence = NULL,
                              colonne_volume = NULL,
                              afficher_warnings = TRUE) {

  # Vérification de la colonne Essence
  if (!"Essence" %in% names(donnees_arbres)) {
    stop("La colonne 'Essence' est requise dans les données")
  }

  # Utilisation du data frame interne equations
  equations_df <- equations

  # Initialisation des colonnes de résultats
  donnees_arbres$Biomasse <- NA_real_
  donnees_arbres$Carbone <- NA_real_

  if (!afficher_warnings) {
    donnees_arbres$Messages <- character(nrow(donnees_arbres))
  }

  # Optimisation: traiter toutes les lignes avec la même essence en une fois
  if (!is.null(essence)) {
    # Si une essence spécifique est fournie, l'utiliser pour toutes les lignes
    essences_uniques <- essence
  } else {
    # Sinon, obtenir les essences uniques du jeu de données
    essences_uniques <- unique(donnees_arbres$Essence)
  }

  for (ess in essences_uniques) {
    # Identifier les lignes pour cette essence
    if (!is.null(essence)) {
      indices <- 1:nrow(donnees_arbres)
    } else {
      indices <- which(donnees_arbres$Essence == ess)
    }

    if (length(indices) == 0) next

    # Chercher l'équation correspondant à l'essence
    eq <- equations_df[equations_df$Y == "BIOMASSE" & equations_df$Essences == ess, ]
    if (nrow(eq) == 0) {
      eq <- equations_df[equations_df$Y == "BIOMASSE" & equations_df$Essences == "General", ]
    }

    if (nrow(eq) == 0) {
      message_erreur <- paste("Pas d'équation de biomasse pour l'essence:", ess)
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[indices] <- paste0(donnees_arbres$Messages[indices], message_erreur, "; ")
      }
      next
    }

    eq <- eq[1, ]  # Prendre la première équation si plusieurs correspondances

    # Déterminer quelle colonne de volume utiliser
    volume_col <- colonne_volume
    if (is.null(volume_col)) {
      # Vérifier si VC_source existe dans equations_df avant de l'utiliser
      if ("VC_source" %in% names(eq) && !is.na(eq$VC_source)) {
        volume_col <- eq$VC_source
      } else {
        volume_col <- "VC22"  # Valeur par défaut
      }
    }

    if (!(volume_col %in% names(donnees_arbres))) {
      message_erreur <- paste("Colonne de volume", volume_col, "absente pour l'essence:", ess)
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[indices] <- paste0(donnees_arbres$Messages[indices], message_erreur, "; ")
      }
      next
    }

    volumes <- donnees_arbres[[volume_col]][indices]
    BF <- eq$BF
    ef <- eq$EF
    carbone_facteur <- eq$Carbone

    # Vérification des valeurs manquantes
    indices_na <- which(is.na(volumes) | is.na(BF) | is.na(ef))
    if (length(indices_na) > 0) {
      indices_na_orig <- indices[indices_na]
      message_erreur <- paste("Valeurs manquantes pour le calcul de la biomasse (essence:", ess, ")")
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[indices_na_orig] <- paste0(donnees_arbres$Messages[indices_na_orig], message_erreur, "; ")
      }
    }

    # Vérification des valeurs négatives ou nulles (seulement sur les valeurs non-NA)
    volumes_valides <- volumes[!is.na(volumes)]
    indices_valides_na <- which(!is.na(volumes))

    if (length(volumes_valides) > 0 && (!is.na(BF) && !is.na(ef))) {
      indices_invalides <- indices_valides_na[volumes_valides <= 0 | BF <= 0 | ef <= 0]

      if (length(indices_invalides) > 0) {
        indices_invalides_orig <- indices[indices_invalides]
        message_erreur <- paste("Valeurs invalides (≤ 0) pour le calcul de la biomasse (essence:", ess, ")")
        if (afficher_warnings) {
          warning(message_erreur)
        } else {
          donnees_arbres$Messages[indices_invalides_orig] <- paste0(donnees_arbres$Messages[indices_invalides_orig], message_erreur, "; ")
        }
      }

      # Calcul pour les valeurs valides (non NA et > 0)
      indices_calcul <- setdiff(indices_valides_na, indices_invalides)
      if (length(indices_calcul) > 0) {
        indices_calcul_orig <- indices[indices_calcul]
        biomasses <- volumes[indices_calcul] * BF * ef

        donnees_arbres$Biomasse[indices_calcul_orig] <- biomasses

        if (!is.na(carbone_facteur)) {
          donnees_arbres$Carbone[indices_calcul_orig] <- biomasses * carbone_facteur
        }
      }
    }
  }

  return(donnees_arbres)
}
