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

library(testthat)

test_that("Basic cases - simple expressions", {
  vars <- list(x = 5, y = 10, z = 2)
  expect_equal(evaluate_expression("x + y", vars), 15)
  expect_equal(evaluate_expression("y - x", vars), 5)
  expect_equal(evaluate_expression("x * z", vars), 10)
  expect_equal(evaluate_expression("y / z", vars), 5)
  expect_equal(evaluate_expression("x^z", vars), 25)
  expect_equal(evaluate_expression("(x + y) * z", vars), 30)
})

test_that("Special cases - zero values", {
  vars <- list(x = 5, y = 10)
  expect_equal(evaluate_expression("0", vars), 0)
  expect_equal(evaluate_expression(0, vars), 0)
  expect_equal(evaluate_expression(NA, vars), 0)
  expect_equal(evaluate_expression(NA_character_, vars), 0)
})

test_that("Advanced mathematical expressions", {
  vars <- list(a = 4, b = 9, c = 16)
  expect_equal(evaluate_expression("sqrt(a)", vars), 2)
  expect_equal(evaluate_expression("log(a)", vars), log(4))
  expect_equal(evaluate_expression("sin(0)", vars), 0)
  expect_equal(evaluate_expression("cos(0)", vars), 1)
  expect_equal(evaluate_expression("abs(-a)", vars), 4)
  expect_equal(evaluate_expression("max(a, b)", vars), 9)
  expect_equal(evaluate_expression("min(a, b)", vars), 4)
})

test_that("Error handling - missing variables", {
  vars <- list(x = 5, y = 10)
  expect_warning(result <- evaluate_expression("x + z", vars))
  expect_true(is.na(result))
  expect_match(capture_warnings(evaluate_expression("x + z", vars)),
               "Variable not found: z for expression: x \\+ z")
})

test_that("Error handling - NA or NULL variables", {
  vars_na <- list(x = 5, y = NA)
  expect_warning(result <- evaluate_expression("x + y", vars_na))
  expect_true(is.na(result))
  expect_match(capture_warnings(evaluate_expression("x + y", vars_na)),
               "Variable is NA or NULL: y for expression: x \\+ y")
  vars_null <- list(x = 5, y = NULL)
  expect_warning(result <- evaluate_expression("x + y", vars_null))
  expect_true(is.na(result))
  expect_match(capture_warnings(evaluate_expression("x + y", vars_null)),
               "Variable is NA or NULL: y for expression: x \\+ y")
})

test_that("Error handling - non-finite variable values", {
  vars_inf <- list(x = 5, y = Inf)
  expect_warning(result <- evaluate_expression("x + y", vars_inf))
  expect_true(is.na(result))
  expect_match(capture_warnings(evaluate_expression("x + y", vars_inf)),
               "Variable is not finite: y = Inf for expression: x \\+ y")
  vars_neg_inf <- list(x = 5, y = -Inf)
  expect_warning(result <- evaluate_expression("x + y", vars_neg_inf))
  expect_true(is.na(result))
  vars_nan <- list(x = 5, y = NaN)
  expect_warning(result <- evaluate_expression("x + y", vars_nan))
  expect_true(is.na(result))
})

test_that("Error handling - invalid expressions", {
  vars <- list(x = 5, y = 10)
  expect_warning(result <- evaluate_expression("/", vars))
  expect_true(is.na(result))
  expect_match(capture_warnings(evaluate_expression("/", vars)),
               "Invalid expression detected: '/'")
  expect_warning(result <- evaluate_expression("x +", vars))
  expect_true(is.na(result))
  expect_match(capture_warnings(evaluate_expression("x +", vars)),
               "Error during expression evaluation: x \\+")
  expect_warning(result <- evaluate_expression("(x + y", vars))
  expect_true(is.na(result))
})

test_that("Error handling - non-finite results", {
  vars <- list(x = 5, y = 0)
  expect_warning(result <- evaluate_expression("x / y", vars))
  expect_true(is.na(result))
  expect_match(capture_warnings(evaluate_expression("x / y", vars)),
               "Non-finite result for expression: x / y = Inf")
  vars_neg <- list(x = -5)
  expect_warning(result <- evaluate_expression("log(x)", vars_neg))
  expect_true(is.na(result))
})

test_that("Constant-only expressions", {
  vars <- list()
  expect_equal(evaluate_expression("42", vars), 42)
  expect_equal(evaluate_expression("2 + 3", vars), 5)
  expect_equal(evaluate_expression("pi", vars), pi)
  expect_equal(evaluate_expression("sqrt(16)", vars), 4)
})

test_that("Variables with special names", {
  vars_dots <- list(var.1 = 10, var.2 = 20)
  expect_equal(evaluate_expression("var.1 + var.2", vars_dots), 30)
  vars_underscore <- list(var_1 = 10, var_2 = 20)
  expect_equal(evaluate_expression("var_1 * var_2", vars_underscore), 200)
  vars_num <- list(x1 = 5, x2 = 10, x10 = 100)
  expect_equal(evaluate_expression("x1 + x2 + x10", vars_num), 115)
})

test_that("Boolean and logical expressions", {
  vars <- list(x = 5, y = 10, z = 5)
  expect_equal(evaluate_expression("x < y", vars), TRUE)
  expect_equal(evaluate_expression("x > y", vars), FALSE)
  expect_equal(evaluate_expression("x == z", vars), TRUE)
  expect_equal(evaluate_expression("x != y", vars), TRUE)
  expect_equal(evaluate_expression("x < y & x == z", vars), TRUE)
  expect_equal(evaluate_expression("x > y | x == z", vars), TRUE)
})

test_that("Expressions with conditions", {
  vars <- list(x = 5, y = 10, z = 2)
  expect_equal(evaluate_expression("ifelse(x < y, x * z, y * z)", vars), 10)
  expect_equal(evaluate_expression("ifelse(x > y, x * z, y * z)", vars), 20)
})

test_that("Diverse data types", {
  vars_int <- list(x = 5L, y = 10L)
  expect_equal(evaluate_expression("x + y", vars_int), 15)
  vars_double <- list(x = 5.5, y = 10.2)
  expect_equal(evaluate_expression("x + y", vars_double), 15.7)
  vars_mixed <- list(x = 5L, y = 10.5)
  expect_equal(evaluate_expression("x + y", vars_mixed), 15.5)
})

test_that("Performance and edge cases", {
  vars <- list(a = 1, b = 2, c = 3, d = 4, e = 5)
  long_expr <- "a + b * c - d / e + sqrt(a) + log(b) + sin(c) + cos(d) + abs(e)"
  result <- evaluate_expression(long_expr, vars)
  expect_true(is.finite(result))
  vars_extreme <- list(x = .Machine$double.xmax / 2, y = .Machine$double.xmin * 2)
  expect_true(is.finite(evaluate_expression("x + y", vars_extreme)))
  expect_warning(result <- evaluate_expression("", vars))
  expect_true(is.na(result))
})

test_that("Multiple warnings handling", {
  vars <- list(x = 5)
  warnings <- capture_warnings(evaluate_expression("x + y + z", vars))
  expect_length(warnings, 1)
  expect_match(warnings[1], "Variable not found: y")
})

test_that("Boundary variable values", {
  vars_zero <- list(x = 0, y = 5)
  expect_equal(evaluate_expression("x + y", vars_zero), 5)
  expect_equal(evaluate_expression("x * y", vars_zero), 0)
  vars_neg <- list(x = -5, y = 10)
  expect_equal(evaluate_expression("x + y", vars_neg), 5)
  expect_equal(evaluate_expression("abs(x)", vars_neg), 5)
})

test_that("Unclosed parentheses expression", {
  vars <- list(x = 1, y = 2)
  expect_warning(result <- evaluate_expression("(x + y", vars))
  expect_true(is.na(result))
})

test_that("Non-finite result NaN", {
  vars <- list()
  expect_warning(result <- evaluate_expression("0/0", vars),
                 regexp = "Non-finite result for expression: 0/0 = NaN")
  expect_true(is.na(result))
})

# Additional error scenarios
test_that("Evaluation error - missing function call", {
  vars <- list(x = 1)
  w <- capture_warnings(result <- evaluate_expression("foo(1)", vars))
  expect_length(w, 1)
  expect_match(w, "Error during expression evaluation: foo\\(1\\) -.*could not find function \"foo\"")
  expect_true(is.na(result))
})

test_that("Evaluation error - explicit stop() call", {
  vars <- list()
  w <- capture_warnings(result <- evaluate_expression("stop('oops')", vars))
  expect_length(w, 1)
  expect_match(w, "Error during expression evaluation: stop\\('oops'\\) - oops")
  expect_true(is.na(result))
})

test_that("evaluates built-in constants without variables", {
  expect_equal(evaluate_expression("pi * 2", list()), 2 * pi)
  expect_equal(evaluate_expression("exp(1)", list()), exp(1))
})

test_that("empty string expression returns NA with warning", {
  warn <- capture_warnings(res <- evaluate_expression("", list()))
  expect_true(is.na(res))
  expect_true(length(warn) >= 1)
  expect_match(warn, "Error during expression evaluation:")
})

test_that("syntax error with invalid operator returns NA and warning", {
  warn <- capture_warnings(res <- evaluate_expression("1 +* 2", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Error during expression evaluation: 1 \\+\\* 2", warn)))
})

test_that("missing function call produces warning and NA", {
  warn <- capture_warnings(res <- evaluate_expression("foo(1)", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("could not find function \"foo\"", warn)))
})

test_that("explicit stop() inside expression returns NA with the stop message", {
  warn <- capture_warnings(res <- evaluate_expression("stop('oops')", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("oops", warn)))
})

test_that("division by zero yields NA with appropriate warning", {
  warn <- capture_warnings(res <- evaluate_expression("1/0", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Non-finite result for expression: 1/0 = Inf", warn)))
})

test_that("NaN result yields NA with appropriate warning", {
  warn <- capture_warnings(res <- evaluate_expression("0/0", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Non-finite result for expression: 0/0 = NaN", warn)))
})

test_that("logical expressions are evaluated correctly", {
  vars <- list(x = 2, y = 3)
  expect_equal(evaluate_expression("x < y", vars), TRUE)
  expect_equal(evaluate_expression("x > y", vars), FALSE)
  expect_equal(evaluate_expression("x == 2", vars), TRUE)
})

test_that("ifelse expressions work as expected", {
  vars <- list(x = 1, y = 2)
  expect_equal(evaluate_expression("ifelse(x < y, x + 1, y + 1)", vars), 2)
  expect_equal(evaluate_expression("ifelse(x > y, x + 1, y + 1)", vars), 3)
})

test_that("handles underscores and dots in variable names", {
  vars <- list(var_1 = 10, var.2 = 5)
  expect_equal(evaluate_expression("var_1 - var.2", vars), 5)
})

test_that("mixing integer and numeric variables works", {
  vars <- list(i = 3L, d = 4.5)
  expect_equal(evaluate_expression("i + d", vars), 7.5)
})

test_that("string \"0\" returns 0 without warnings", {
  expect_silent(res1 <- evaluate_expression("0", list()))
  expect_equal(res1, 0)
})

test_that("parse error from unmatched quote returns NA with warning", {
  warn <- capture_warnings(res <- evaluate_expression("'unclosed", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Error during expression evaluation: 'unclosed", warn)))
})

test_that("unknown symbol not in variables or baseenv returns NA with warning", {
  warn <- capture_warnings(res <- evaluate_expression("foobar", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Variable not found: foobar for expression: foobar", warn)))
})

test_that("built-in functions are recognized without missing-variable warnings", {
  expect_silent(res <- evaluate_expression("sin(pi/2)", list()))
  expect_equal(res, 1)
})

test_that("eval error inside tryCatch returns NA with warning", {
  warn <- capture_warnings(res <- evaluate_expression("log('a')", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Error during expression evaluation: log\\('\\'a'\\)", warn) |
                    grepl("Error during expression evaluation: log\\('a'\\)", warn)))
})

test_that("expression using only variables with baseenv constants works", {
  vars <- list(x = 3)
  expect_silent(res <- evaluate_expression("x + pi", vars))
  expect_equal(res, 3 + pi)
})

test_that("multiple distinct missing-variable warnings only emit one warning", {
  warn <- capture_warnings(evaluate_expression("a + b + c", list(a = 1)))
  expect_length(warn, 1)
  expect_true(grepl("Variable not found: b", warn))
})

test_that("evaluates nested functions without warnings", {
  expect_silent(res <- evaluate_expression("sqrt(abs(-16))", list()))
  expect_equal(res, 4)
})

test_that("expression returning non-finite NaN triggers NA and warning", {
  warn <- capture_warnings(res <- evaluate_expression("0/0", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Non-finite result for expression: 0/0 = NaN", warn)))
})

test_that("expression with valid variables and no operators returns variable value", {
  vars <- list(foo = 42)
  expect_equal(evaluate_expression("foo", vars), 42)
})

test_that("numeric non-zero expr_text returns its numeric value without warnings", {
  expect_silent(res <- evaluate_expression(1, list()))
  expect_equal(res, 1)
})

test_that("empty string returns NA with parse warning", {
  warn <- capture_warnings(res <- evaluate_expression("", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Error during expression evaluation:", warn)))
})

test_that("baseenv function length works without warnings, toupper returns NA with warning", {
  expect_silent(res1 <- evaluate_expression("length(c(1,2,3))", list()))
  expect_equal(res1, 3)
  warn2 <- capture_warnings(res2 <- evaluate_expression("toupper('a')", list()))
  expect_true(is.na(res2))
  expect_true(any(grepl("Non-finite result for expression: toupper\\('a'\\)", warn2)))
})

test_that("assignment expressions are treated as missing-variable errors", {
  warn <- capture_warnings(res <- evaluate_expression("x <- 5", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Variable not found: x for expression: x <- 5", warn)))
})

test_that("variable name can shadow base function", {
  res <- evaluate_expression("mean + 1", list(mean = 5))
  expect_equal(res, 6)
})

test_that("complex arithmetic expression without variables computes correctly", {
  expect_silent(res <- evaluate_expression("(2+3)*sqrt(4)", list()))
  expect_equal(res, 10)
})

test_that("numeric literal returns its value without warnings", {
  expect_silent(res <- evaluate_expression(1, list()))
  expect_equal(res, 1)
})

test_that("complex arithmetic expression without variables computes correctly", {
  expect_silent(res <- evaluate_expression("(2+3)*sqrt(4)", list()))
  expect_equal(res, 10)
})

test_that("toupper('a') returns NA with warning", {
  warn <- capture_warnings(res <- evaluate_expression("toupper('a')", list()))
  expect_true(is.na(res))
  expect_true(length(warn) > 0)
})


test_that("LETTERS[2] returns NA with warning", {
  warn <- capture_warnings(res <- evaluate_expression("LETTERS[2]", list()))
  expect_true(is.na(res))
  expect_true(length(warn) > 0)
})

test_that("empty string returns NA with parse error warning", {
  warn <- capture_warnings(res <- evaluate_expression("", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Error during expression evaluation:", warn)))
})

test_that("sequence expression '1:3' returns NA with evaluation warning", {
  warn <- capture_warnings(res <- evaluate_expression("1:3", list()))
  expect_true(is.na(res))
  expect_true(length(warn) >= 1)
})

context("evaluate_expression() exhaustive coverage")

test_that("expr_text NA, '0' or 0 returns 0 silently", {
  expect_silent(expect_equal(evaluate_expression(NA,   list()), 0))
  expect_silent(expect_equal(evaluate_expression(NA_character_, list()), 0))
  expect_silent(expect_equal(evaluate_expression("0",  list()), 0))
  expect_silent(expect_equal(evaluate_expression(0,    list()), 0))
})

test_that("slash returns NA with Invalid expression warning", {
  w <- capture_warnings(res <- evaluate_expression("/", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("^Invalid expression detected: '/'", w)))
})

test_that("parse error returns NA with parse-warning", {
  w <- capture_warnings(res <- evaluate_expression("1 +* 2", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Error during expression evaluation: 1 \\+\\* 2 -", w)))
})

test_that("missing variable yields NA with variable-not-found warning", {
  w <- capture_warnings(res <- evaluate_expression("a + b", list(a = 1)))
  expect_true(is.na(res))
  expect_true(any(grepl("Variable not found: b for expression: a \\+ b", w)))
})

test_that("NA or NULL variable yields NA with appropriate warning", {
  w1 <- capture_warnings(r1 <- evaluate_expression("x + 1", list(x = NA)))
  expect_true(is.na(r1))
  expect_true(any(grepl("Variable is NA or NULL: x for expression: x \\+ 1", w1)))

  w2 <- capture_warnings(r2 <- evaluate_expression("y + 1", list(y = NULL)))
  expect_true(is.na(r2))
  expect_true(any(grepl("Variable is NA or NULL: y for expression: y \\+ 1", w2)))
})

test_that("non-finite variable yields NA with appropriate warning", {
  w <- capture_warnings(r <- evaluate_expression("x * 2", list(x = Inf)))
  expect_true(is.na(r))
  expect_true(any(grepl("Variable is not finite: x = Inf for expression: x \\* 2", w)))
})

test_that("undefined function in eval returns NA with eval-error warning", {
  w <- capture_warnings(r <- evaluate_expression("foo(1)", list()))
  expect_true(is.na(r))
  expect_true(any(grepl("Error during expression evaluation: foo\\(1\\) -", w)))
})

test_that("stop() inside expr returns NA with stop-message warning", {
  w <- capture_warnings(r <- evaluate_expression("stop('oops')", list()))
  expect_true(is.na(r))
  expect_true(any(grepl("Error during expression evaluation: stop\\('oops'\\) - oops", w)))
})

test_that("division by zero yields NA with non-finite-result warning", {
  w1 <- capture_warnings(r1 <- evaluate_expression("1/0", list()))
  expect_true(is.na(r1))
  expect_true(any(grepl("Non-finite result for expression: 1/0 = Inf", w1)))

  w2 <- capture_warnings(r2 <- evaluate_expression("0/0", list()))
  expect_true(is.na(r2))
  expect_true(any(grepl("Non-finite result for expression: 0/0 = NaN", w2)))
})

test_that("logical and comparison expressions work", {
  expect_equal(evaluate_expression("TRUE & FALSE", list()), FALSE)
  expect_equal(evaluate_expression("1 < 2", list()), TRUE)
  expect_equal(evaluate_expression("2 > 1", list()), TRUE)
})

test_that("baseenv constants and functions work without warnings", {
  expect_silent(r1 <- evaluate_expression("pi * 2", list()))
  expect_equal(r1, 2 * pi)

  expect_silent(r2 <- evaluate_expression("sqrt(9)", list()))
  expect_equal(r2, 3)

  expect_silent(r3 <- evaluate_expression("log(1)", list()))
  expect_equal(r3, 0)
})

test_that("sequence operator '1:3' triggers evaluation warning and NA", {
  w <- capture_warnings(r <- evaluate_expression("1:3", list()))
  expect_true(is.na(r))
  expect_true(length(w) >= 1)
})

test_that("simple arithmetic with variables computes correctly", {
  expect_equal(evaluate_expression("x + y*2", list(x = 2, y = 3)), 8)
})

test_that("parentheses and operator precedence are respected", {
  expect_equal(evaluate_expression("(1+2)*3", list()), 9)
  expect_equal(evaluate_expression("1+(2*3)", list()), 7)
})

context("evaluate_expression() corrected coverage")

test_that("literal NA, '0', 0 and '0' return 0 silently", {
  expect_silent(expect_equal(evaluate_expression(NA, list()), 0))
  expect_silent(expect_equal(evaluate_expression(NA_character_, list()), 0))
  expect_silent(expect_equal(evaluate_expression("0", list()), 0))
  expect_silent(expect_equal(evaluate_expression(0, list()), 0))
})

test_that("slash '/' returns NA with specific warning", {
  w <- capture_warnings(res <- evaluate_expression("/", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Invalid expression detected: '/'", w)))
})

test_that("parse error returns NA with parse-warning", {
  w <- capture_warnings(res <- evaluate_expression("1 +* 2", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("Error during expression evaluation: 1 \\+\\* 2 -", w)))
})
context("evaluate_expression – real behavior tests")

test_that("c(1,2,3) returns NA with length>1 warning", {
  warnings <- capture_warnings(res <- evaluate_expression("c(1,2,3)", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("the condition has length > 1", warnings)))
})

test_that("sequence 1:3 returns NA with length>1 warning", {
  warnings <- capture_warnings(res <- evaluate_expression("1:3", list()))
  expect_true(is.na(res))
  expect_true(any(grepl("the condition has length > 1", warnings)))
})

test_that("sqrt(4) returns 2 silently", {
  expect_silent(res <- evaluate_expression("sqrt(4)", list()))
  expect_equal(res, 2)
})

test_that("length(c(1,2)) returns 2 silently", {
  expect_silent(res <- evaluate_expression("length(c(1,2))", list()))
  expect_equal(res, 2)
})

test_that("toupper('a') returns NA with at least one warning", {
  warnings <- capture_warnings(res <- evaluate_expression("toupper('a')", list()))
  expect_true(is.na(res))
  expect_true(length(warnings) >= 1)
})

test_that("LETTERS[2] returns NA with at least one warning", {
  warnings <- capture_warnings(res <- evaluate_expression("LETTERS[2]", list()))
  expect_true(is.na(res))
  expect_true(length(warnings) >= 1)
})


