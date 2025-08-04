#' Equations allometriques forestieres
#'
#' Ce jeu de donnees contient les coefficients des equations allometriques pour differentes essences d'arbres,
#' permettant d'estimer le volume, la biomasse ou le stock de carbone en fonction de variables dendrometriques
#' comme la circonference a 1,30 m (C130).
#'
#' @format Un data frame avec X lignes et 22 colonnes:
#' \describe{
#'   \item{Y}{Variable binaire/indicatrice (presence/absence)}
#'   \item{A0}{Identifiant de l'equation}
#'   \item{X0}{Premier terme de l'equation}
#'   \item{X1}{Deuxieme terme de l'equation, generalement C130}
#'   \item{X2}{Troisieme terme de l'equation, generalement C130^2}
#'   \item{X3}{Quatrieme terme de l'equation, generalement C130^3}
#'   \item{X4}{Cinquieme terme de l'equation (non utilise dans certains cas)}
#'   \item{X5}{Sixieme terme de l'equation (non utilise dans certains cas)}
#'   \item{Essences}{Nom de l'essence forestiere (ex: Bouleau)}
#'   \item{b0}{Coefficient pour le terme X0}
#'   \item{b1}{Coefficient pour le terme X1}
#'   \item{b2}{Coefficient pour le terme X2}
#'   \item{b3}{Coefficient pour le terme X3}
#'   \item{b4}{Coefficient pour le terme X4}
#'   \item{b5}{Coefficient pour le terme X5}
#'   \item{Source}{Source des equations allometriques (ex: Dagnellie)}
#'   \item{Region}{Region de validite de l'equation (ex: Wallonie)}
#'   \item{Equation}{Type d'equation (ex: Arbre, Peuplement)}
#'   \item{ID}{Infra-densite de l'essence}
#'   \item{EF}{Facteur d'expansion (Expansion Factor)}
#'   \item{BF}{Facteur de biomasse}
#'   \item{Carbone}{Teneur en carbone}
#'   \item{VC22}{Variable de controle ou d'identification}
#' }
#'
#'
#' @details
#' Ces equations permettent d'estimer differentes caracteristiques des arbres (volume, biomasse, carbone)
#' en fonction de mesures dendrometriques. L'equation generale a la forme:
#' Y = b0*X0 + b1*X1 + b2*X2 + b3*X3 + b4*X4 + b5*X5
#'
#' Ou X1, X2, X3 representent generalement C130, C130^2, C130^3 (circonference a 1,30 m et ses puissances).
#'
#' @source Diverses sources bibliographiques, dont les travaux de Dagnellie pour la Wallonie.
#'
"equations"
