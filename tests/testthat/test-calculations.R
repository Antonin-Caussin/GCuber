# ============================================================================
# SETUP - Test data and mocks
# ============================================================================
n <- 20
# Utility function to create test data
create_test_data <- function(n) {
  data.frame(
    Species = rep(c("Hetre", "Chene pedoncule", "Epicea commun"), length.out = n),
    D130 = runif(n, 15, 80),
    DHB = runif(n, 15, 80), # Alias for D130
    C130 = runif(n, 47, 251),
    HTOT = runif(n, 8, 35),
    HDOM = runif(n, 8, 35),
    G130 = runif(n, 0.02, 0.5),
    stringsAsFactors = FALSE
  )
}

# Utility function to create test equations
create_test_equations <- function() {
  data.frame(
    Species = c("Hetre", "Chene pedoncule", "Epicea commun"),
    Y = rep("V22", 3),
    A0 = c(1, 1, 1),
    b0 = c(-0.5, -0.4, -0.6),
    b1 = c(1.8, 1.7, 2.0),
    b2 = c(0.8, 0.7, 1.0),
    b3 = rep(0, 3),
    b4 = rep(0, 3),
    b5 = rep(0, 3),
    X1 = rep("D130", 3),
    X2 = rep("HTOT", 3),
    X3 = rep("0", 3),
    X4 = rep("0", 3),
    X5 = rep("0", 3),
    D130_Min = rep(10, 3),
    D130_Max = rep(80, 3),
    C130_min = rep(30, 3),
    C130_Max = rep(240, 3),
    HTOT = rep(25,3),
    sigma = rep(0.15, 3),
    n = rep(150, 3),
    x_mean_D130 = rep(35, 3),
    SCE_D130 = rep(1500, 3),
    Source_Eq = rep("Dagnelie", 3),
    stringsAsFactors = FALSE
  )
}

# Utility function to create bark equations
create_bark_equations <<- function() {
  data.frame(
    Species = c("Hetre", "Chene pedoncule"),
    Y = c("E", "E"),
    A0 = c(1, 4),
    b0 = c(0.5, -1.2),
    b1 = c(0.02, 0.8),
    b2 = c(0, 0),
    b3 = c(0, 0),
    b4 = c(0, 0),
    b5 = c(0, 0),
    X1 = c("D130", "D130"),
    X2 = c("0", "0"),
    X3 = c("0", "0"),
    X4 = c("0", "0"),
    X5 = c("0", "0"),
    stringsAsFactors = FALSE
  )
}

# Utility function to create biomass equations
create_biomass_equations <<- function() {
  data.frame(
    Species = c("Hetre", "Hetre", "Chene pedoncule"),
    Y = c("BIOMASS", "BIOMASS", "BIOMASS"),
    A0 = c(1, 4, 1),
    b0 = c(0.1, -2.5, 0.2),
    b1 = c(2.5, 2.8, 2.3),
    b2 = c(0, 0.5, 0),
    b3 = c(0, 0, 0),
    b4 = c(0, 0, 0),
    b5 = c(0, 0, 0),
    X1 = c("D130^2", "D130", "D130^2"),
    X2 = c("0", "HTOT", "0"),
    X3 = c("0", "0", "0"),
    X4 = c("0", "0", "0"),
    X5 = c("0", "0", "0"),
    stringsAsFactors = FALSE
  )
}

# Mock of the evaluate_expression function
evaluate_expression <- function(expr_text, variables) {
  if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
  if (expr_text == "/") {
    warning("Invalid expression detected: '/'")
    return(NA)
  }

  # Wrap the parse in tryCatch to capture syntax errors
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
    # Only test custom variables (those not in baseenv())
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

test_that("DIAGNOSTIC - Current state of equations", {
  cat("\n=== COMPLETE DIAGNOSTIC ===\n")

  # Check existence of global equations
  if (exists("equations", envir = .GlobalEnv)) {
    current_eq <- get("equations", envir = .GlobalEnv)
    cat("'equations' object found with", nrow(current_eq), "rows\n")
    cat("Columns:", paste(names(current_eq), collapse = ", "), "\n")
    cat("Y types:", paste(unique(current_eq$Y), collapse = ", "), "\n")
    cat("Species for V22:\n")
    v22_species <- unique(current_eq$Species[current_eq$Y == "V22"])
    print(v22_species)

    # Compare with our test data
    test_data <- create_test_data(3)
    test_species <- unique(test_data$Species)
    cat("\nSpecies in create_test_data():", paste(test_species, collapse = ", "), "\n")

    missing <- setdiff(test_species, v22_species)
    cat("Missing species in equations:", paste(missing, collapse = ", "), "\n")

  } else {
    cat("NO 'equations' object found in global environment\n")
  }

  expect_true(TRUE)  # Dummy test
})

# ============================================================================
# TESTS FOR calculate_volume()
# ============================================================================

test_that("calculate_volume - Basic functionality", {
  # Setup
  test_data <- create_test_data(5)
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data, equations = test_equations, volume_type = "V22", equation_id = 1)

  # Checks
  expect_s3_class(result, "data.frame")
  expect_true("V22 [m^3]" %in% names(result))
  expect_true("Validity Status" %in% names(result))
  expect_true("Equation Used" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))

  # Verify that volumes are not all NA
  expect_true(sum(!is.na(result[["V22 [m^3]"]])) > 0)
})

test_that("calculate_volume - Handling species without equation", {
  # Setup
  test_data <- data.frame(
    Species = "Unknown_Species",
    D130 = 30,
    HTOT = 20,
    stringsAsFactors = FALSE
  )
  test_equations <<- create_test_equations()

  # Test with expected warning
  expect_warning(
    result <- calculate_volume(test_data, equations = test_equations, volume_type = "V22"),
    "No equation found for species"
  )

  expect_equal(result[["Validity Status"]][1], "NO EQUATION")
})

test_that("calculate_volume - Validity domain validation", {
  # Setup
  test_data <- create_test_data(n)
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data, equations = test_equations, volume_type = "V22", D130 = "D130")

  # Checks
  expect_equal(result[["Validity Status"]][1], "VALID")
  expect_equal(result[["Validity Status"]][2], "VALID")
  expect_equal(result[["Validity Status"]][3], "VALID")
})

test_that("calculate_volume - Custom D130 parameter", {
  # Setup
  test_data <- create_test_data(3)
  test_data$DHB_custom <- test_data$D130
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data, equations = test_equations, D130 = "DHB_custom", volume_type = "V22")

  # Checks
  expect_true(sum(!is.na(result[["V22 [m^3]"]])) > 0)
})

test_that("calculate_volume - Handling missing values", {
  # Setup
  test_data <- create_test_data(3)
  test_data$D130[2] <- NA
  test_data$HTOT[3] <- NA
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data, equations = test_equations, volume_type = "V22")

  # Checks
  expect_true(is.na(result[["V22 [m^3]"]][2]) || is.na(result[["V22 [m^3]"]][3]))
})

# ============================================================================
# TESTS FOR calculate_bark_thickness()
# ============================================================================

test_that("calculate_bark_thickness - Basic functionality", {
  # Setup
  test_data <- create_test_data(3)
  test_data[["V22 [m^3]"]] <- c(0.5, 0.8, 1.2)

  eq1 <- create_test_equations()
  eq2 <- create_bark_equations()

  # Harmonize columns for rbind
  common_cols <- intersect(names(eq1), names(eq2))
  eq1 <- eq1[, common_cols, drop = FALSE]
  eq2 <- eq2[, common_cols, drop = FALSE]

  test_equations <- rbind(eq1, eq2)

  # Test
  result <- calculate_bark_thickness(
    test_data,
    equations = test_equations,
    total_volume_col =NULL,
    volume_type = "V22",
    source = "Dagnelie"
  )

  # Checks
  expect_true("Bark Volume [m^3]" %in% names(result))
  expect_true("Wood Volume [m^3]" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
})

test_that("calculate_bark_thickness - Absence of bark equations", {
  test_data <- create_test_data(2)
  test_data[["V22 [m^3]"]] <- c(0.5, 0.8)
  test_equations <- create_test_equations()

  expect_warning(
    result <- calculate_bark_thickness(
      test_data,
      equations      = test_equations,
      total_volume_col = NULL,
      volume_type    = "V22",
      source         = "Dagnelie"
    ),
    regexp = "No bark.*thickness.*equations.*found"
  )

  expect_true(all(c("Bark Volume [m^3]", "Wood Volume [m^3]") %in% names(result)))

  expect_true(all(is.na(result[["Bark Volume [m^3]"]])))

  expect_equal(
    result[, names(test_data)],
    test_data
  )

  expect_equal(
    result[, setdiff(names(test_data), "V22 [m^3]")],
    test_data[, setdiff(names(test_data), "V22 [m^3]")]
  )
})

test_that("calculate_bark_thickness - Volume conservation", {
  test_data <- data.frame(
    Species = "Hetre",
    D130    = 30,
    V22     = 1.0,
    stringsAsFactors = FALSE
  )

  eq1 <- create_test_equations()
  eq2 <- create_bark_equations()
  common_cols <- intersect(names(eq1), names(eq2))
  equations <- rbind(eq1[, common_cols], eq2[, common_cols])

  result <- calculate_bark_thickness(
    test_data,
    equations         = equations,
    total_volume_col  = "V22",
    source            = "Dagnelie"
  )

  expect_false(is.na(result[["Bark Volume [m^3]"]]))
  expect_false(is.na(result[["Wood Volume [m^3]"]]))

  expect_equal(
    result[["Bark Volume [m^3]"]] + result[["Wood Volume [m^3]"]],
    result[["V22"]],
    tolerance = 1e-6
  )
})


# ============================================================================
# TESTS FOR calculate_biomass()
# ============================================================================

test_that("calculate_biomass - Basic functionality", {
  test_data <- create_test_data(3)
  equations_df <- create_biomass_equations()
  equations_df$A0      <- 16
  equations_df$X0      <- "0"
  equations_df$Type_Eq <- NA_character_

  result <- calculate_biomass(
    test_data,
    equations = equations_df,
    method    = "equation"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Biomass_AB16", "Biomass Root [kg]", "Biomass Total [kg]") %in% names(result)))
  expect_equal(nrow(result), nrow(test_data))
  expect_true(sum(!is.na(result[["Biomass Total [kg]"]])) > 0)
})


test_that("calculate_biomass - Absence of biomass equations", {
  test_data <- create_test_data(2)
  equations_df <- data.frame()

  expect_warning(
    result <- calculate_biomass(
      test_data,
      equations = equations_df,
      method    = "equation"
    ),
    regexp = "No valid biomass equations found"
  )

  expect_equal(result, test_data)
})

test_that("calculate_biomass - Logarithmic equations (A0=18)", {
  test_data <- data.frame(
    Species = "Hetre",
    D130    = 30,
    stringsAsFactors = FALSE
  )

  equations_df <- data.frame(
    Species  = "Hetre",
    Y        = "BIOMASS",
    A0       = 18,
    Type_Eq  = NA_character_,
    b0       = -2.5,
    b1       = 2.8,
    b2       = 0, b3 = 0, b4 = 0, b5 = 0,
    X0       = "0",
    X1       = "D130",
    X2       = "0", X3 = "0", X4 = "0", X5 = "0",
    stringsAsFactors = FALSE
  )

  result <- calculate_biomass(
    test_data,
    equations = equations_df,
    method    = "equation"
  )

  expected_biomass <- exp(-2.5 + 2.8 * log(30)) / 1000

  expect_equal(result[["Biomass_AB18"]][1], expected_biomass, tolerance = 1e-8)
  expect_equal(result[["Biomass Root [kg]"]][1], expected_biomass * 0.2, tolerance = 1e-8)
  expect_equal(result[["Biomass Total [kg]"]][1], expected_biomass * 1.2, tolerance = 1e-8)
})

test_that("calculate_biomass - Volume x infra-density mode", {
  test_data <- create_test_data(3)
  test_data[["V22"]] <- c(1.2, 2.5, 0.8)

  id_table <- data.frame(
    Species = unique(test_data$Species),
    ID      = c(0.5, 0.6, 0.4),
    stringsAsFactors = FALSE
  )

  result <- calculate_biomass(
    test_data,
    equations = id_table,
    method    = "volume"
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Biomass V22", "Biomass Root [kg]", "Biomass Total [kg]") %in% names(result)))

  # On intègre le *1000 appliqué à l'ID dans la fonction
  expected_aboveground <- test_data[["V22"]] * id_table$ID * 1000
  expect_equal(
    result[["Biomass V22"]],
    expected_aboveground
  )
  expect_equal(
    result[["Biomass Root [kg]"]],
    expected_aboveground * 0.2
  )
  expect_equal(
    result[["Biomass Total [kg]"]],
    expected_aboveground * 1.2
  )
})



# ============================================================================
# TESTS FOR calculate_carbon()
# ============================================================================

test_that("calculate_carbon - Basic functionality", {
  # On crée d'abord avec un nom temporaire valide
  test_data <- data.frame(
    Species         = rep("Hetre", 3),
    Biomass_kg      = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  # Puis on renomme proprement la colonne
  names(test_data)[names(test_data) == "Biomass_kg"] <- "Biomass Total [kg]"

  result <- calculate_carbon(test_data)

  expect_s3_class(result, "data.frame")
  expect_true("Carbon Total [kg]" %in% names(result))
  expect_equal(result[["Carbon Total [kg]"]], c(47, 94, 141))
})



test_that("calculate_carbon - Missing Biomass_Total column", {
  test_data <- data.frame(Species = "Hetre", stringsAsFactors = FALSE)

  expect_warning(
    result <- calculate_carbon(test_data),
    regexp = "Biomass_Total.*missing"
  )

  # La branche warning crée " Carbon Total [kg]" (avec espace initial)
  expect_true("Carbon Total [kg]" %in% names(result))
  expect_true(is.na(result[["Carbon Total [kg]"]][1]))
})


# ============================================================================
# TESTS FOR calculate_prediction_interval()
# ============================================================================

test_that("calculate_prediction_interval - Univariate case", {
  test_data <- data.frame(
    Species = "Hetre",
    D130 = 35,
    V22 = 0.8,  # volume column
    stringsAsFactors = FALSE
  )

  eqs_volume <- data.frame(
    Species = "Hetre",
    Y = "V22",
    sigma = 0.15,
    n = 150,
    x_mean_D130 = 35,
    SCE_D130 = 1500,
    X1 = "D130",
    X2 = "0", X3 = "0", X4 = "0", X5 = "0",
    stringsAsFactors = FALSE
  )

  result <- calculate_prediction_interval(
    x = test_data,
    equations = eqs_volume,
    volume_type = "V22",
    equation_id = 1,
    summarize = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("Relative Width" %in% names(result))
  expect_true("Reliability" %in% names(result))
  expect_true(is.numeric(result [["Relative Width"]]))
  expect_false(is.na(result [["Relative Width"]]))
})

test_that("calculate_prediction_interval - Multivariate case", {
  test_data <- data.frame(
    Species = "Hetre",
    D130 = 35,
    HTOT = 20,
    V22 = 0.8,  # note: volume column must match volume_type
    stringsAsFactors = FALSE
  )

  eqs_volume <- data.frame(
    Species = "Hetre",
    Y = "V22",
    sigma = 0.15,
    n = 150,
    x_mean_D130 = 35,
    x_mean_HTOT = 20,
    SCE_D130 = 1500,
    SCE_HTOT = 800,
    COV_D130_HTOT = 200,
    X1 = "D130",
    X2 = "HTOT",
    X3 = "0", X4 = "0", X5 = "0",
    stringsAsFactors = FALSE
  )

  result <- calculate_prediction_interval(
    x = test_data,
    equations = eqs_volume,
    volume_type = "V22",
    equation_id = 1,
    summarize = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("Relative Width" %in% names(result))
  expect_true("Reliability" %in% names(result))
  expect_true(is.numeric(result [["Relative Width"]]))
})

# ============================================================================
# INTEGRATION TESTS
# ============================================================================

test_that("Complete pipeline - Volume -> Bark -> Biomass -> Carbon", {

  test_data <- data.frame(
    Species = c("Hetre", "Chene pedoncule"),
    D130    = c(30, 40),
    HTOT    = c(25, 30),
    stringsAsFactors = FALSE
  )

  eq_vol     <- create_test_equations()
  eq_bark    <- create_bark_equations()
  eq_biomass <- create_biomass_equations()

  result <- calculate_volume(
    test_data,
    equations   = eq_vol,
    volume_type = "V22",
    equation_id = 1
  )

  result <- calculate_bark_thickness(
    result,
    equations   = eq_bark,
    volume_type = "V22",
    source      = NULL
  )

  eq_biomass$A0      <- 16
  eq_biomass$X0      <- "0"
  eq_biomass$Type_Eq <- NA_character_
  result <- calculate_biomass(
    result,
    equations = eq_biomass,
    method    = "equation"
  )
  result <- calculate_carbon(result)

  expect_true("V22 [m^3]"          %in% names(result))
  expect_true("Bark Volume [m^3]"  %in% names(result))
  expect_true("Biomass Total [kg]" %in% names(result))
  expect_true("Carbon Total [kg]"  %in% names(result))
})

test_that("Error handling - Corrupted data", {
  test_data <- data.frame(
    Species = c("Hetre", "Hetre"),
    D130    = c(-10, Inf),
    HTOT    = c(NA, 25),
    stringsAsFactors = FALSE
  )
  eq_vol <- create_test_equations()

  expect_warning(
    result <- calculate_volume(
      test_data,
      equations   = eq_vol,
      volume_type = "V22"
    ),
    regexp = ".*"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})


# ============================================================================
# PERFORMANCE TESTS
# ============================================================================

test_that("Performance - Large dataset", {
  # Setup
  large_data <- create_test_data(1000)
  equations <- create_test_equations()

  # Performance test
  start_time <- Sys.time()
  result <- calculate_volume(large_data, equations = equations, volume_type = "V22")
  end_time <- Sys.time()


  # Checks
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1000)

  # Calculation should not take more than 30 seconds
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(time_taken, 30)
})

