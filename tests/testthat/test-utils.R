
# Définition de la fonction à tester
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

# Tests complets pour evaluate_expression
test_that("Cas de base - expressions simples", {
  vars <- list(x = 5, y = 10, z = 2)

  # Addition
  expect_equal(evaluate_expression("x + y", vars), 15)

  # Soustraction
  expect_equal(evaluate_expression("y - x", vars), 5)

  # Multiplication
  expect_equal(evaluate_expression("x * z", vars), 10)

  # Division
  expect_equal(evaluate_expression("y / z", vars), 5)

  # Puissance
  expect_equal(evaluate_expression("x^z", vars), 25)

  # Expression complexe
  expect_equal(evaluate_expression("(x + y) * z", vars), 30)
})

test_that("Cas spéciaux - valeurs zéro", {
  vars <- list(x = 5, y = 10)

  # Expression texte "0"
  expect_equal(evaluate_expression("0", vars), 0)

  # Expression numérique 0
  expect_equal(evaluate_expression(0, vars), 0)

  # Expression NA
  expect_equal(evaluate_expression(NA, vars), 0)

  # Expression NA_character_
  expect_equal(evaluate_expression(NA_character_, vars), 0)
})

test_that("Expressions mathématiques avancées", {
  vars <- list(a = 4, b = 9, c = 16)

  # Racine carrée
  expect_equal(evaluate_expression("sqrt(a)", vars), 2)

  # Logarithme
  expect_equal(evaluate_expression("log(a)", vars), log(4))

  # Fonctions trigonométriques
  expect_equal(evaluate_expression("sin(0)", vars), 0)
  expect_equal(evaluate_expression("cos(0)", vars), 1)

  # Valeur absolue
  expect_equal(evaluate_expression("abs(-a)", vars), 4)

  # Maximum/minimum
  expect_equal(evaluate_expression("max(a, b)", vars), 9)
  expect_equal(evaluate_expression("min(a, b)", vars), 4)
})

test_that("Gestion des erreurs - variables manquantes", {
  vars <- list(x = 5, y = 10)

  # Variable inexistante
  expect_warning(result <- evaluate_expression("x + z", vars))
  expect_true(is.na(result))
  expect_match(
    capture_warnings(evaluate_expression("x + z", vars)),
    "Variable not found: z for expression: x \\+ z"
  )
})

test_that("Gestion des erreurs - variables NA ou NULL", {
  # Variable NA
  vars_na <- list(x = 5, y = NA)
  expect_warning(result <- evaluate_expression("x + y", vars_na))
  expect_true(is.na(result))
  expect_match(
    capture_warnings(evaluate_expression("x + y", vars_na)),
    "Variable is NA or NULL: y for expression: x \\+ y"
  )

  # Variable NULL
  vars_null <- list(x = 5, y = NULL)
  expect_warning(result <- evaluate_expression("x + y", vars_null))
  expect_true(is.na(result))
  expect_match(
    capture_warnings(evaluate_expression("x + y", vars_null)),
    "Variable is NA or NULL: y for expression: x \\+ y"
  )
})

test_that("Gestion des erreurs - valeurs non-finies", {
  # Variable Inf
  vars_inf <- list(x = 5, y = Inf)
  expect_warning(result <- evaluate_expression("x + y", vars_inf))
  expect_true(is.na(result))
  expect_match(
    capture_warnings(evaluate_expression("x + y", vars_inf)),
    "Variable is not finite: y = Inf for expression: x \\+ y"
  )

  # Variable -Inf
  vars_neg_inf <- list(x = 5, y = -Inf)
  expect_warning(result <- evaluate_expression("x + y", vars_neg_inf))
  expect_true(is.na(result))

  # Variable NaN
  vars_nan <- list(x = 5, y = NaN)
  expect_warning(result <- evaluate_expression("x + y", vars_nan))
  expect_true(is.na(result))
})

test_that("Gestion des erreurs - expressions invalides", {
  vars <- list(x = 5, y = 10)

  # Expression "/" seule
  expect_warning(result <- evaluate_expression("/", vars))
  expect_true(is.na(result))
  expect_match(
    capture_warnings(evaluate_expression("/", vars)),
    "Invalid expression detected: '/'"
  )

  # Syntaxe invalide
  expect_warning(result <- evaluate_expression("x +", vars))
  expect_true(is.na(result))
  expect_match(
    capture_warnings(evaluate_expression("x +", vars)),
    "Error during expression evaluation: x \\+"
  )

  # Parenthèses non fermées
  expect_warning(result <- evaluate_expression("(x + y", vars))
  expect_true(is.na(result))
})

test_that("Gestion des résultats non-finis", {
  vars <- list(x = 5, y = 0)

  # Division par zéro
  expect_warning(result <- evaluate_expression("x / y", vars))
  expect_true(is.na(result))
  expect_match(
    capture_warnings(evaluate_expression("x / y", vars)),
    "Non-finite result for expression: x / y = Inf"
  )

  # Logarithme de nombre négatif
  vars_neg <- list(x = -5)
  expect_warning(result <- evaluate_expression("log(x)", vars_neg))
  expect_true(is.na(result))
})

test_that("Expressions avec constantes uniquement", {
  vars <- list()

  # Constante numérique
  expect_equal(evaluate_expression("42", vars), 42)

  # Expression mathématique constante
  expect_equal(evaluate_expression("2 + 3", vars), 5)

  # Expression avec pi
  expect_equal(evaluate_expression("pi", vars), pi)

  # Expression avec fonction intégrée
  expect_equal(evaluate_expression("sqrt(16)", vars), 4)
})

test_that("Variables avec noms spéciaux", {
  # Variables avec points
  vars_dots <- list(var.1 = 10, var.2 = 20)
  expect_equal(evaluate_expression("var.1 + var.2", vars_dots), 30)

  # Variables avec underscores
  vars_underscore <- list(var_1 = 10, var_2 = 20)
  expect_equal(evaluate_expression("var_1 * var_2", vars_underscore), 200)

  # Variables avec nombres
  vars_num <- list(x1 = 5, x2 = 10, x10 = 100)
  expect_equal(evaluate_expression("x1 + x2 + x10", vars_num), 115)
})

test_that("Expressions booléennes et logiques", {
  vars <- list(x = 5, y = 10, z = 5)

  # Comparaisons
  expect_equal(evaluate_expression("x < y", vars), TRUE)
  expect_equal(evaluate_expression("x > y", vars), FALSE)
  expect_equal(evaluate_expression("x == z", vars), TRUE)
  expect_equal(evaluate_expression("x != y", vars), TRUE)

  # Opérateurs logiques
  expect_equal(evaluate_expression("x < y & x == z", vars), TRUE)
  expect_equal(evaluate_expression("x > y | x == z", vars), TRUE)
})

test_that("Expressions avec conditions", {
  vars <- list(x = 5, y = 10, z = 2)

  # ifelse
  expect_equal(evaluate_expression("ifelse(x < y, x * z, y * z)", vars), 10)
  expect_equal(evaluate_expression("ifelse(x > y, x * z, y * z)", vars), 20)
})

test_that("Types de données divers", {
  # Variables entières
  vars_int <- list(x = 5L, y = 10L)
  expect_equal(evaluate_expression("x + y", vars_int), 15)

  # Variables décimales
  vars_double <- list(x = 5.5, y = 10.2)
  expect_equal(evaluate_expression("x + y", vars_double), 15.7)

  # Mélange de types
  vars_mixed <- list(x = 5L, y = 10.5)
  expect_equal(evaluate_expression("x + y", vars_mixed), 15.5)
})

test_that("Performance et edge cases", {
  # Expression très longue
  vars <- list(a = 1, b = 2, c = 3, d = 4, e = 5)
  long_expr <- "a + b * c - d / e + sqrt(a) + log(b) + sin(c) + cos(d) + abs(e)"
  result <- evaluate_expression(long_expr, vars)
  expect_true(is.finite(result))

  # Variables avec valeurs extrêmes (mais finies)
  vars_extreme <- list(x = .Machine$double.xmax / 2, y = .Machine$double.xmin * 2)
  expect_true(is.finite(evaluate_expression("x + y", vars_extreme)))

  # Expression vide (doit échouer)
  expect_warning(result <- evaluate_expression("", vars))
  expect_true(is.na(result))
})

test_that("Gestion des warnings multiples", {
  vars <- list(x = 5)

  # Plusieurs variables manquantes
  warnings <- capture_warnings(evaluate_expression("x + y + z", vars))
  expect_length(warnings, 1) # Premier warning arrête l'évaluation
  expect_match(warnings[1], "Variable not found: y")
})

test_that("Variables avec valeurs limites", {
  # Zéro
  vars_zero <- list(x = 0, y = 5)
  expect_equal(evaluate_expression("x + y", vars_zero), 5)
  expect_equal(evaluate_expression("x * y", vars_zero), 0)

  # Valeurs négatives
  vars_neg <- list(x = -5, y = 10)
  expect_equal(evaluate_expression("x + y", vars_neg), 5)
  expect_equal(evaluate_expression("abs(x)", vars_neg), 5)
})

test_that("Expressions avec parenthèses non fermées", {
  vars <- list(x = 1, y = 2)
  expect_warning(result <- evaluate_expression("(x + y", vars))
  expect_true(is.na(result))
})

