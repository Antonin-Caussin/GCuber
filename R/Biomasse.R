#' Calcule la biomasse aerienne et le carbone associe à partir des volumes et coefficients
#'
#' @param donnees_arbres Un data frame contenant les colonnes 'Essence', 'VC22' ou 'VC22B'
#' @param essence Essence ciblee (ou NULL pour utiliser celle de chaque ligne)
#' @param volume Nom de la colonne de volume à utiliser (NULL utilise celle specifiee dans les equations internes)
#' @param afficher_warnings Booleen indiquant si les warnings doivent etre affiches (TRUE) ou stockes (FALSE)
#' @return Le data frame d'entree avec les colonnes supplementaires 'Biomasse', 'Carbone' et 'Messages' si afficher_warnings=FALSE
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
                              volume = NULL,
                              afficher_warnings = TRUE) {

  # Verification de la colonne Essence
  if (!"Essence" %in% names(donnees_arbres)) {
    stop("La colonne 'Essence' est requise dans les donnees")
  }

  # Utilisation du data frame interne equations
  equations_df <- equations

  # Initialisation des colonnes de resultats
  donnees_arbres$Biomasse <- NA_real_
  donnees_arbres$Carbone <- NA_real_

  if (!afficher_warnings) {
    donnees_arbres$Messages <- character(nrow(donnees_arbres))
  }

  # Optimisation: traiter toutes les lignes avec la meme essence en une fois
  if (!is.null(essence)) {
    # Si une essence specifique est fournie, l'utiliser pour toutes les lignes
    essences_uniques <- essence
  } else {
    # Sinon, obtenir les essences uniques du jeu de donnees
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

    # Chercher l'equation correspondant à l'essence
    eq <- equations_df[equations_df$Y == "BIOMASSE" & equations_df$Essences == ess, ]
    if (nrow(eq) == 0) {
      eq <- equations_df[equations_df$Y == "BIOMASSE" & equations_df$Essences == "General", ]
    }

    if (nrow(eq) == 0) {
      message_erreur <- paste("Pas d'equation de biomasse pour l'essence:", ess)
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[indices] <- paste0(donnees_arbres$Messages[indices], message_erreur, "; ")
      }
      next
    }

    eq <- eq[1, ]  # Prendre la premiere equation si plusieurs correspondances

    # Recuperer les facteurs specifiques à cette essence
    bf <- eq$BF
    ef <- eq$EF
    carbone_facteur <- eq$Carbone

    # Verifier si les facteurs necessaires sont disponibles
    if (is.na(bf) || is.na(ef) || bf <= 0 || ef <= 0) {
      message_erreur <- paste("Facteurs BF ou EF manquants ou invalides pour l'essence:", ess)
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[indices] <- paste0(donnees_arbres$Messages[indices], message_erreur, "; ")
      }
      next
    }

    # Determiner quelle colonne de volume utiliser
    volume_col <- volume
    if (is.null(volume_col)) {
      # Utiliser "VC22" comme valeur par defaut
      volume_col <- "VC22"

      # Si "VC22" n'existe pas mais "VC22B" existe, utiliser "VC22B"
      if (!("VC22" %in% names(donnees_arbres)) && "VC22B" %in% names(donnees_arbres)) {
        volume_col <- "VC22B"
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

    # Recuperer les valeurs de volume pour les indices actuels
    volumes <- donnees_arbres[[volume_col]][indices]

    # Verifier s'il y a des valeurs NA
    indices_na <- which(is.na(volumes))
    if (length(indices_na) > 0) {
      message_erreur <- paste("Valeurs manquantes pour le calcul de la biomasse (essence:", ess, ")")
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[indices[indices_na]] <- paste0(
          donnees_arbres$Messages[indices[indices_na]], message_erreur, "; ")
      }
    }

    # Verifier les valeurs <= 0
    indices_valides <- which(!is.na(volumes))
    indices_invalides <- indices_valides[volumes[indices_valides] <= 0]

    if (length(indices_invalides) > 0) {
      message_erreur <- paste("Valeurs negatives ou nulles pour le calcul de la biomasse (essence:", ess, ")")
      if (afficher_warnings) {
        warning(message_erreur)
      } else {
        donnees_arbres$Messages[indices[indices_invalides]] <- paste0(
          donnees_arbres$Messages[indices[indices_invalides]], message_erreur, "; ")
      }
    }

    # Calculer la biomasse pour les valeurs valides
    indices_calcul <- setdiff(indices_valides, indices_invalides)

    if (length(indices_calcul) > 0) {
      # Calcul: volume * BF * EF
      biomasses <- volumes[indices_calcul] * bf * ef
      donnees_arbres$Biomasse[indices[indices_calcul]] <- biomasses

      # Si le facteur carbone est disponible, calculer le stock de carbone
      if (!is.na(carbone_facteur)) {
        donnees_arbres$Carbone[indices[indices_calcul]] <- biomasses * carbone_facteur
      }
    }
  }

  return(donnees_arbres)
}
