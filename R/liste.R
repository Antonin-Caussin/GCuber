#' Liste les équations disponibles pour une essence et un type de volume donné
#'
#' Cette fonction permet à l'utilisateur d'afficher les équations disponibles dans la base
#' en fonction d'une essence et/ou d'un type de volume. Elle attribue également un identifiant
#' unique à chaque équation pour permettre une sélection plus facile (ex. via l'argument `id_equation`
#' dans une autre fonction).
#'
#' @param equations_df Le data frame contenant les équations allométriques (ex : `equations_allometriques`)
#' @param essence (Optionnel) Le nom de l'essence à filtrer (ex : `"Bouleau"`). Si NULL, toutes les essences sont affichées.
#' @param type_volume (Optionnel) Le type de volume à filtrer (ex : `"VC22"`). Si NULL, tous les types de volume sont affichés.
#'
#' @return Un data frame avec un identifiant (`ID`) et les colonnes utiles pour visualiser les équations :
#' `Y`, `Essences`, `X1:X5`, `b0:b5`.
#'
#' @examples
#' # Liste toutes les équations disponibles pour le bouleau et le volume VC22
#' liste_eq(equations_allometriques, essence = "Bouleau", type_volume = "VC22")
#'
#' # Liste toutes les équations, sans filtre
#' liste_eq(equations_allometriques)
#'
#' @export
liste_eq <- function(equations_allometriques, essence = NULL, type_volume = NULL) {
  eqs <- equations_allometriques
  if (!is.null(essence)) eqs <- eqs[eqs$Essences == essence, ]
  if (!is.null(type_volume)) eqs <- eqs[eqs$Y == type_volume, ]
  eqs$ID <- seq_len(nrow(eqs))
  return(eqs[, c("ID", "Y", "Essences", "X1", "X2", "X3", "X4", "X5",
                 "b0", "b1", "b2", "b3", "b4", "b5")])
}
