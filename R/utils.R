#' Évalue une expression mathématique en toute sécurité
#' @description Cette fonction prend une chaîne de caractères représentant une expression mathématique
#' et l'évalue dans un environnement contenant les variables nécessaires.
#' Elle gère les erreurs, les valeurs manquantes et les cas non finis.
#' @param expr Expression mathématique sous forme de chaîne
#' @param variables Liste nommée de variables utilisées dans l'expression
#' @return Valeur numérique évaluée ou NA en cas d'erreur
#' @export
evaluate_expression <- function(expr_text, variables) {
  if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
  if (expr_text == "/") {
    warning("Invalid expression detected: '/'")
    return(NA)
  }

  # Encapsuler le parse dans tryCatch pour capturer les erreurs de syntaxe
  expr_parsed <- tryCatch(
    parse(text = expr_text),
    error = function(e) {
      warning(paste("Error during expression evaluation:", expr_text, "-", e$message))
      return(NULL)
    }
  )
  if (is.null(expr_parsed)) return(NA)

  var_names <- all.vars(expr_parsed)
  for (v in var_names) {
    # Ne teste que les variables personnalisées (celles qui ne sont pas dans baseenv())
    if (!v %in% names(variables) && !exists(v, envir = baseenv())) {
      warning(paste("Variable not found:", v, "for expression:", expr_text))
      return(NA)
    }
    if (v %in% names(variables)) {
      if (is.na(variables[[v]]) || is.null(variables[[v]])) {
        warning(paste("Variable is NA or NULL:", v, "for expression:", expr_text))
        return(NA)
      }
      if (!is.finite(variables[[v]])) {
        warning(paste("Variable is not finite:", v, "=", variables[[v]], "for expression:", expr_text))
        return(NA)
      }
    }
  }


  env <- list2env(variables)
  tryCatch({
    result <- eval(expr_parsed, envir = env)
    if (!is.finite(result)) {
      warning(paste("Non-finite result for expression:", expr_text, "=", result))
      return(NA)
    }
    return(result)
  }, error = function(e) {
    warning(paste("Error during expression evaluation:", expr_text, "-", e$message))
    return(NA)
  })
}

