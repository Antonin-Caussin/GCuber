#' Liste les équations disponibles pour une essence et un type de volume donné
#'
#' @param essence (Optionnel) Le nom de l'essence à filtrer (ex : `"Bouleau"`). Si NULL, toutes les essences sont affichées.
#' @param type_volume (Optionnel) Le type de volume à filtrer (ex : `"VC22"`, `"E"`, `"VC22B"`). Si NULL, tous les types de volume sont affichés.
#'
#' @return Un data frame avec un identifiant (`ID`) et les colonnes utiles pour visualiser les équations :
#' `Y`, `Essences`, `X1:X5`, `b0:b5`.
#'
#' @export
liste_eq <- function(essence = NULL, type_volume = NULL) {
  equations

  eqs <- equations
  if (!is.null(essence)) eqs <- eqs[eqs$Essences == essence, ]

  # Utilisation d'un vecteur pour les types de volume spécifiques
  if (!is.null(type_volume)) {
    volumes_autorises <- c("VC22", "E", "VC22B") # Par exemple
    eqs <- eqs[eqs$Y %in% volumes_autorises, ]
  }

  eqs$ID <- seq_len(nrow(eqs))
  return(eqs[, c("ID", "Y", "Essences", "X1", "X2", "X3", "X4", "X5",
                 "b0", "b1", "b2", "b3", "b4", "b5")])
}
