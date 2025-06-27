
# ============================================================================
# SETUP - Données de test et mocks
# ============================================================================
n <- 20
# Fonction utilitaire pour créer des données de test
create_test_data <- function(n) {
  data.frame(
    Species = rep(c("Hetre", "Chene pedoncule", "Epicea commun"), length.out = n),
    D130 = runif(n, 15, 80),
    DHB = runif(n, 15, 80), # Alias pour D130
    C130 = runif(n, 47, 251),
    HTOT = runif(n, 8, 35),
    HDOM = runif(n, 8, 35),
    G130 = runif(n, 0.02, 0.5),
    stringsAsFactors = FALSE
  )
}

# Fonction utilitaire pour créer des équations de test
create_test_equations <<- function() {
  data.frame(
    Species = c("Hetre", "Hetre", "Chene pedoncule", "Epicea commun"),
    Y = c("V22", "V22", "V22", "V22"),
    A0 = c(1, 1, 1, 1),
    b0 = c(-0.5, -0.6, -0.4, -0.7),
    b1 = c(1.8, 1.9, 1.7, 2.0),
    b2 = c(0.8, 0.9, 0.7, 1.0),
    b3 = c(0, 0, 0, 0),
    b4 = c(0, 0, 0, 0),
    b5 = c(0, 0, 0, 0),
    X0 = c(1,1,1,1),
    X1 = c("D130", "D130", "D130", "D130"),
    X2 = c("HTOT", "HTOT", "HTOT", "HTOT"),
    X3 = c("0", "0", "0", "0"),
    X4 = c("0", "0", "0", "0"),
    X5 = c("0", "0", "0", "0"),
    D_Min = c(10, 10, 12, 8),
    D_Max = c(70, 70, 80, 90),
    sigma = c(0.15, 0.16, 0.14, 0.18),
    n = c(150, 120, 180, 200),
    x_mean_D130 = c(35, 32, 40, 28),
    SCE_D130 = c(1500, 1200, 1800, 2000),
    stringsAsFactors = FALSE
  )
}

# Fonction utilitaire pour créer des équations d'écorce
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

# Fonction utilitaire pour créer des équations de biomasse
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

# Mock de la fonction evaluate_expression
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

test_that("DIAGNOSTIC - État actuel des équations", {
  cat("\n=== DIAGNOSTIC COMPLET ===\n")

  # Vérifier l'existence d'equations global
  if (exists("equations", envir = .GlobalEnv)) {
    current_eq <- get("equations", envir = .GlobalEnv)
    cat("Objet 'equations' trouvé avec", nrow(current_eq), "lignes\n")
    cat("Colonnes:", paste(names(current_eq), collapse = ", "), "\n")
    cat("Types de Y:", paste(unique(current_eq$Y), collapse = ", "), "\n")
    cat("Espèces pour V22:\n")
    v22_species <- unique(current_eq$Species[current_eq$Y == "V22"])
    print(v22_species)

    # Comparer avec nos données de test
    test_data <- create_test_data(3)
    test_species <- unique(test_data$Species)
    cat("\nEspèces dans create_test_data():", paste(test_species, collapse = ", "), "\n")

    missing <- setdiff(test_species, v22_species)
    cat("Espèces manquantes dans equations:", paste(missing, collapse = ", "), "\n")

  } else {
    cat("AUCUN objet 'equations' trouvé dans l'environnement global\n")
  }

  expect_true(TRUE)  # Test factice
})

# ============================================================================
# TESTS POUR calculate_volume()
# ============================================================================

test_that("calculate_volume - Fonctionnement de base", {
  # Setup
  test_data <- create_test_data(5)
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data, equations = test_equations ,volume_type = "V22", equation_id = 1)

  # Vérifications
  expect_s3_class(result, "data.frame")
  expect_true("V22" %in% names(result))
  expect_true("Validity_Status" %in% names(result))
  expect_true("Equation_Used" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))

  # Vérifier que les volumes ne sont pas tous NA
  expect_true(sum(!is.na(result$V22)) > 0)
})

test_that("calculate_volume - Gestion des espèces sans équation", {
  # Setup
  test_data <- data.frame(
    Species = "Espece_Inconnue",
    D130 = 30,
    HTOT = 20,
    stringsAsFactors = FALSE
  )
  test_equations <<- create_test_equations()

  # Test avec avertissement attendu
  expect_warning(
    result <- calculate_volume(test_data, equations = test_equations,volume_type = "V22"),
    "No equation found for species"
  )

  expect_equal(result$Validity_Status[1], "NO_EQUATION")
})

test_that("calculate_volume - Validation du domaine de validité", {
  # Setup
  test_data <- create_test_data(n)
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data, equations = test_equations,volume_type = "V22", D130 = "D130")

  # Vérifications
  expect_equal(result$Validity_Status[1], "VALID")
  expect_equal(result$Validity_Status[2], "VALID")
  expect_equal(result$Validity_Status[3], "VALID")
})

test_that("calculate_volume - Paramètre D130 personnalisé", {
  # Setup
  test_data <- create_test_data(3)
  test_data$DHB_custom <- test_data$D130
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data,equations = test_equations, D130 = "DHB_custom", volume_type = "V22")

  # Vérifications
  expect_true(sum(!is.na(result$V22)) > 0)
})

test_that("calculate_volume - Gestion des valeurs manquantes", {
  # Setup
  test_data <- create_test_data(3)
  test_data$D130[2] <- NA
  test_data$HTOT[3] <- NA
  test_equations <<- create_test_equations()

  # Test
  result <- calculate_volume(test_data,equations = test_equations, volume_type = "V22")

  # Vérifications
  expect_true(is.na(result$V22[2]) || is.na(result$V22[3]))
})

# ============================================================================
# TESTS POUR calculate_bark_thickness()
# ============================================================================

test_that("calculate_bark_thickness - Fonctionnement de base", {
  # Setup
  test_data <- create_test_data(3)
  test_data$V22 <- c(0.5, 0.8, 1.2)

  # Harmonisation des colonnes des tables
  eq1 <- create_test_equations()
  eq2 <- create_bark_equations()

  common_cols <- intersect(names(eq1), names(eq2))
  eq1 <- eq1[, common_cols]
  eq2 <- eq2[, common_cols]

  test_equations <- rbind(eq1, eq2)

  # Test
  result <- calculate_bark_thickness(
    test_data,
    equations = test_equations,
    total_volume_col = "V22"
  )

  # Vérifications
  expect_true("E" %in% names(result))
  expect_true("Bark_Volume" %in% names(result))
  expect_true("Wood_Volume" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))
})


test_that("calculate_bark_thickness - Absence d'équations d'écorce", {
  # Setup
  test_data <- create_test_data(2)
  test_data$V22 <- c(0.5, 0.8)
  test_equations <<- create_test_equations() # Pas d'équations E

  # Test avec avertissement
  expect_warning(
    result <- calculate_bark_thickness(test_data, equations = test_equations),
    "Aucune équation d'écorce"
  )

  expect_true(all(is.na(result$E)))
})

test_that("calculate_bark_thickness - Conservation du volume", {
  # Setup
  test_data <- data.frame(
    Species = "Hetre",
    D130 = 30,
    V22 = 1.0,
    stringsAsFactors = FALSE
  )

  # Harmoniser les colonnes des équations avant rbind
  eq1 <- create_test_equations()
  eq2 <- create_bark_equations()
  common_cols <- intersect(names(eq1), names(eq2))
  eq1 <- eq1[, common_cols, drop = FALSE]
  eq2 <- eq2[, common_cols, drop = FALSE]
  equations <<- rbind(eq1, eq2)

  # Test
  result <- calculate_bark_thickness(test_data, equations = equations, total_volume_col = "V22")


  # Vérification que Volume_Bark + Volume_Wood = Volume_Total
  if (!is.na(result$Bark_Volume[1]) && !is.na(result$Wood_Volume[1])) {
    total_calc <- result$Bark_Volume[1] + result$Wood_Volume[1]
    expect_equal(total_calc, result$V22[1], tolerance = 1e-6)
  }
})


# ============================================================================
# TESTS POUR calculate_biomass()
# ============================================================================

test_that("calculate_biomass - Fonctionnement de base", {
  # Setup
  test_data <- create_test_data(3)
  equations_df <- create_biomass_equations()

  # Test
  result <- calculate_biomass(test_data, equations = equations_df)

  # Vérifications
  expect_true("Biomass_Total" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))

  # Vérifier la création des colonnes spécifiques par espèce
  biomass_cols <- names(result)[grepl("_Biomass_", names(result))]
  expect_true(length(biomass_cols) > 0)
})

test_that("calculate_biomass - Absence d'équations de biomasse", {
  # Setup
  test_data <- create_test_data(2)
  equations_df <- data.frame() # Pas d'équations

  # Test avec avertissement
  expect_warning(
    result <- calculate_biomass(test_data, equations = equations_df),
    "No biomass equations found"
  )

  expect_true(all(is.na(result$Biomass_Total)))
})

test_that("calculate_biomass - Équations logarithmiques (A0=4)", {
  # Setup
  test_data <- data.frame(
    Species = "Hetre",
    D130 = 30,
    HTOT = 20,
    stringsAsFactors = FALSE
  )

  equations_df <- data.frame(
    Species = "Hetre",
    Y = "BIOMASS",
    A0 = 4,
    b0 = -2.5,
    b1 = 2.8,
    b2 = 0.5,
    b3 = 0, b4 = 0, b5 = 0,
    X1 = "D130",
    X2 = "HTOT",
    X3 = "0", X4 = "0", X5 = "0",
    stringsAsFactors = FALSE
  )

  # Test
  result <- calculate_biomass(test_data, equations = equations_df)

  # Vérifications
  expect_true(!is.na(result$Biomass_Total[1]))
  expect_true(result$Biomass_Total[1] > 0)
})

# ============================================================================
# TESTS POUR calculate_carbon()
# ============================================================================

test_that("calculate_carbon - Fonctionnement de base", {
  # Setup
  test_data <- data.frame(
    Species = "Hetre",
    Biomass_Total = c(100, 200, 300)
  )

  # Test
  result <- calculate_carbon(test_data)

  # Vérifications
  expect_true("Carbon_Total" %in% names(result))
  expect_equal(result$Carbon_Total, c(50, 100, 150))
})

test_that("calculate_carbon - Absence de colonne Biomass_Total", {
  # Setup
  test_data <- data.frame(Species = "Hetre")

  # Test avec avertissement
  expect_warning(
    result <- calculate_carbon(test_data),
    "La colonne 'Biomass_Total' est absente"
  )

  expect_true("Carbon_Total" %in% names(result))
  expect_true(is.na(result$Carbon_Total[1]))
})

# ============================================================================
# TESTS POUR calculate_prediction_interval()
# ============================================================================

test_that("calculate_prediction_interval - Cas univarié", {
  # Setup
  test_data <- data.frame(
    Species = "Hetre",
    D130 = 35,
    Volume = 0.8,
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

  # Test
  result <- calculate_prediction_interval(test_data, eqs_volume, equation_id = 1)

  # Vérifications
  expect_length(result, nrow(test_data))
  expect_true(is.numeric(result))
})

test_that("calculate_prediction_interval - Cas multivarié", {
  # Setup
  test_data <- data.frame(
    Species = "Hetre",
    D130 = 35,
    HTOT = 20,
    Volume = 0.8,
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
    COV_D130_HTOT = 0.1,
    X1 = "D130",
    X2 = "HTOT",
    X3 = "0", X4 = "0", X5 = "0",
    stringsAsFactors = FALSE
  )

  # Test
  result <- calculate_prediction_interval(test_data, eqs_volume, equation_id = 1)

  # Vérifications
  expect_length(result, nrow(test_data))
  expect_true(is.numeric(result))
})

# ============================================================================
# TESTS POUR interpret_relative_width()
# ============================================================================

test_that("interpret_relative_width - Classification correcte", {
  # Setup
  relative_widths <- c(0.05, 0.15, 0.35, 0.75, NA)

  # Test
  result <- interpret_relative_width(relative_widths)

  # Vérifications
  expect_equal(result[1], "Very narrow -> Very reliable ")
  expect_equal(result[2], "Acceptable -> Rather reliable ")
  expect_equal(result[3], "Wide -> Uncertain ")
  expect_equal(result[4], "Very wide -> Risky ")
  expect_equal(result[5], "No calculation")
})

# ============================================================================
# TESTS POUR summarize_relative_intervals()
# ============================================================================

test_that("summarize_relative_intervals - Résumé statistique", {
  # Setup
  test_data <- data.frame(
    Species = rep(c("Hetre", "Chene pedoncule"), each = 3)
  )
  relative_widths <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)

  # Test (capture la sortie console)
  expect_output(
    summarize_relative_intervals(relative_widths, test_data),
    "SUMMARY OF RELATIVE INTERVAL WIDTHS"
  )

  expect_output(
    summarize_relative_intervals(relative_widths, test_data),
    "Mean relative width"
  )
})

test_that("summarize_relative_intervals - Gestion des valeurs vides", {
  # Setup
  test_data <- data.frame(Species = "Test")
  relative_widths <- c(NA, NA, NA)

  # Test
  expect_output(
    summarize_relative_intervals(relative_widths, test_data),
    "No prediction intervals calculated"
  )
})

# ============================================================================
# TESTS D'INTÉGRATION
# ============================================================================

test_that("Pipeline complet - Volume -> Écorce -> Biomasse -> Carbone", {
  # Setup
  test_data <- create_test_data(5)

  eq_volume <- create_test_equations()
  eq_bark   <- create_bark_equations()
  eq_biomass <- create_biomass_equations()

  # Pipeline complet
  result <- test_data
  result <- calculate_volume(result, equations = eq_volume, volume_type = "V22")
  result <- calculate_bark_thickness(result, equations = eq_bark, total_volume_col = "V22")
  result <- calculate_biomass(result, equations = eq_biomass)
  result <- calculate_carbon(result)

  # Vérifications finales
  expect_true("V22" %in% names(result))
  expect_true("E" %in% names(result))
  expect_true("Biomass_Total" %in% names(result))
  expect_true("Carbon_Total" %in% names(result))
  expect_equal(nrow(result), 5)

  # Vérifier la cohérence des calculs de carbone
  valid_biomass <- !is.na(result$Biomass_Total)
  if (any(valid_biomass)) {
    expect_equal(
      result$Carbon_Total[valid_biomass],
      result$Biomass_Total[valid_biomass] * 0.5,
      tolerance = 1e-10
    )
  }
})



test_that("Gestion des erreurs - Données corrompues", {
  # Setup avec données problématiques
  test_data <- data.frame(
    Species = c("Hetre", "Hetre"),
    D130 = c(-10, Inf),  # Valeurs invalides
    HTOT = c(NA, 25),
    stringsAsFactors = FALSE
  )
  equations <- create_test_equations()

  # Test - autorise tout warning
  expect_warning(
    result <- calculate_volume(test_data, equations = equations, volume_type = "V22"),
    regexp = ".*"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})



# ============================================================================
# TESTS DE PERFORMANCE
# ============================================================================

test_that("Performance - Gros dataset", {
  # Setup
  large_data <- create_test_data(1000)
  equations <- create_test_equations()

  # Test de performance
  start_time <- Sys.time()
  result <- calculate_volume(large_data, equations = equations, volume_type = "V22")
  end_time <- Sys.time()

  # Vérifications
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1000)

  # Le calcul ne devrait pas prendre plus de 30 secondes
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_lt(time_taken, 30)
})


# Nettoie les objets globaux créés pour les tests
teardown({
  if (exists("equations")) rm(equations, envir = .GlobalEnv)
  if (exists("equations_df")) rm(equations_df, envir = .GlobalEnv)
})
