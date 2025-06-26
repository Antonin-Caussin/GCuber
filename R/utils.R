#' Évalue une expression mathématique en toute sécurité
#' @description Cette fonction prend une chaîne de caractères représentant une expression mathématique
#' et l'évalue dans un environnement contenant les variables nécessaires.
#' Elle gère les erreurs, les valeurs manquantes et les cas non finis.
#' @param expr Expression mathématique sous forme de chaîne
#' @param variables Liste nommée de variables utilisées dans l'expression
#' @return Valeur numérique évaluée ou NA en cas d'erreur
#' @export
evaluate_expression <- function(expr, variables) {
  if (is.na(expr) || expr == "0" || expr == 0) return(0)
  if (expr == "/") {
    warning("Expression invalide détectée: '/'")
    return(NA)
  }

  var_names <- all.vars(parse(text = expr))
  for (v in var_names) {
    if (!v %in% names(variables)) {
      warning(paste("Variable manquante:", v, "dans l'expression:", expr))
      return(NA)
    }
    if (is.na(variables[[v]]) || is.null(variables[[v]])) {
      warning(paste("Variable NA ou NULL:", v, "dans l'expression:", expr))
      return(NA)
    }
    if (!is.finite(variables[[v]])) {
      warning(paste("Variable non finie:", v, "=", variables[[v]], "dans l'expression:", expr))
      return(NA)
    }
  }

  env <- list2env(variables)
  tryCatch({
    result <- eval(parse(text = expr), envir = env)
    if (!is.finite(result)) {
      warning(paste("Résultat non fini pour l'expression:", expr, "=", result))
      return(NA)
    }
    return(result)
  }, error = function(e) {
    warning(paste("Erreur lors de l'évaluation de l'expression:", expr, "-", e$message))
    return(NA)
  })
}
