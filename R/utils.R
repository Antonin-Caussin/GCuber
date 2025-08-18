# =====================================================================
#                    evaluate_expression()
# =====================================================================

#' Safely evaluate a mathematical expression
#'
#' @description
#' This function takes a character string representing a mathematical expression
#' and evaluates it in a safe environment using the provided named variables.
#' It handles syntax errors, missing or invalid values, and non-finite results.
#'
#' @param expr_text A character string representing the mathematical expression to evaluate.
#' @param variables A named list of variables used within the expression.
#'
#' @return A numeric value resulting from the evaluation, or `NA` in case of error.
#'
#' @examples
#' # Basic multiplication with provided variables
#' evaluate_expression("D130 * HTOT", list(D130 = 30, HTOT = 20))
#' #> [1] 600
#'
#' # Using a built-in constant (pi) and a formula
#' evaluate_expression("pi * (D130/2)^2", list(D130 = 25))
#' #> [1] 490.8739
#'
#' # Missing variable produces a warning and returns NA
#' evaluate_expression("D130 * HTOT", list(D130 = 25))
#' #> Warning: Variable not found: HTOT in expression: D130 * HTOT
#' #> [1] NA
#'
#' # Invalid expression detected
#' evaluate_expression("/", list())
#' #> Warning: Invalid expression detected: '/'
#' #> [1] NA
#'
#' # Variable with NA value
#' evaluate_expression("D130 * HTOT", list(D130 = NA, HTOT = 20))
#' #> Warning: Variable is NA or NULL: D130 in expression: D130 * HTOT
#' #> [1] NA
#'
#' @export

evaluate_expression <- function(expr_text, variables) {
  if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
  if (expr_text == "/") {
    warning("Invalid expression detected: '/'")
    return(NA)
  }

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
    if (!v %in% names(variables) && !exists(v, envir = baseenv())) {
      warning(paste("Variable not found:", v, "in expression:", expr_text))
      return(NA)
    }
    if (v %in% names(variables)) {
      if (is.na(variables[[v]]) || is.null(variables[[v]])) {
        warning(paste("Variable is NA or NULL:", v, "in expression:", expr_text))
        return(NA)
      }
      if (!is.finite(variables[[v]])) {
        warning(paste("Variable is not finite:", v, "=", variables[[v]], "in expression:", expr_text))
        return(NA)
      }
    }
  }


  env <- list2env(variables)
  tryCatch({
    result <- eval(expr_parsed, envir = env)
    if (!is.finite(result)) {
      warning(paste("Non-finite result from expression:", expr_text, "=", result))
      return(NA)
    }
    return(result)
  }, error = function(e) {
    warning(paste("Error during expression evaluation:", expr_text, "-", e$message))
    return(NA)
  })
}
