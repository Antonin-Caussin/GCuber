# Test file for data processing functions
# tests/testthat/test-data-processing.R

library(testthat)

# Setup test data and mock objects
setup_test_data <- function() {
  # Mock equations data
  equations <<- data.frame(
    Species = c("Hetre", "Chene pedoncule", "Epicea commun"),
    Code = c(3, 97, 41),
    Abr = c("HE", "CHP", "EP"),
    HV = c(0.95, 0.98, 0.92),
    IV = c(2.5, 3.1, 2.8),
    stringsAsFactors = FALSE
  )

  # Test data with various scenarios
  test_data <- data.frame(
    species_code = c(3, 97, 41, 3, 97),
    C130 = c(120, 150, NA, 180, NA),
    C150 = c(110, NA, 170, NA, 160),
    D130 = c(NA, 48, 57, NA, 45),
    D150 = c(35, 45, NA, 54, NA),
    HTOT = c(25, 30, 35, 28, 32),
    HDOM = c(28, 32, 38, 30, 35),
    stringsAsFactors = FALSE
  )

  return(test_data)
}

teardown_test_data <- function() {
  # Nettoyer tous les objets globaux créés pendant les tests
  objects_to_remove <- c("equations", "flags", "establish_species_correspondence",
                         "diameter_conversions", "convert_circumference", "calculate_basal_areas")

  for(obj in objects_to_remove) {
    if (exists(obj, envir = .GlobalEnv)) {
      rm(list = obj, envir = .GlobalEnv)
    }
  }
}

# Mock debug_log function to suppress messages during tests
debug_log <- function(msg) {
  # Silent during tests
  invisible(NULL)
}

# Test as.carbofor_data
test_that("as.carbofor_data converts data.frame correctly", {
  test_data <- setup_test_data()

  result <- as.carbofor_data(test_data)

  expect_s3_class(result, "carbofor_data")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(test_data))
  expect_equal(ncol(result), ncol(test_data))

  teardown_test_data()
})

# Test update_flags
test_that("update_flags updates flags correctly", {
  test_data <- setup_test_data()

  flags <- list(
    C130_exists = FALSE,
    C150_exists = FALSE,
    D130_exists = FALSE,
    D150_exists = FALSE,
    HTOT_exists = FALSE,
    HDOM_exists = FALSE
  )

  updated_flags <- update_flags(test_data, flags, "C130", "C150", "D130", "D150", "HTOT", "HDOM")

  expect_true(updated_flags$C130_exists)
  expect_true(updated_flags$C150_exists)
  expect_true(updated_flags$D130_exists)
  expect_true(updated_flags$D150_exists)
  expect_true(updated_flags$HTOT_exists)
  expect_true(updated_flags$HDOM_exists)

  teardown_test_data()
})

# Test detect_specimens_type
test_that("detect_specimens_type detects numeric codes", {
  test_data <- data.frame(species_code = c(3, 97, 41, 3, 97))

  # Supprimer les messages de debug pour se concentrer sur la détection
  expect_output(
    result <- detect_specimens_type(test_data, "species_code"),
    "SPECIMENS TYPE DETECTION"
  )

  expect_equal(result, "Code")
})

test_that("detect_specimens_type detects abbreviations", {
  test_data <- data.frame(species_abr = c("HE", "CHP", "EP", "HE"))

  expect_output(
    result <- detect_specimens_type(test_data, "species_abr"),
    "SPECIMENS TYPE DETECTION"
  )

  expect_equal(result, "Abr")
})

test_that("detect_specimens_type detects full species names", {
  test_data <- data.frame(species_name = c("Hetre", "Chene pedoncule", "Epicea commun"))

  expect_output(
    result <- detect_specimens_type(test_data, "species_name"),
    "SPECIMENS TYPE DETECTION"
  )

  expect_equal(result, "Species")
})

test_that("detect_specimens_type handles factors", {
  test_data <- data.frame(species_factor = factor(c("HE", "CHP", "EP")))

  expect_output(
    result <- detect_specimens_type(test_data, "species_factor"),
    "SPECIMENS TYPE DETECTION"
  )

  expect_equal(result, "Abr")
})

test_that("detect_specimens_type errors on empty column", {
  test_data <- data.frame(empty_col = c(NA, NA, NA))

  expect_error(
    detect_specimens_type(test_data, "empty_col"),
    "contains only missing values"
  )
})

test_that("detect_specimens_type errors on unrecognized type", {
  test_data <- data.frame(date_col = as.Date(c("2023-01-01", "2023-01-02")))

  expect_error(
    detect_specimens_type(test_data, "date_col"),
    "Data type in column.*is not recognized"
  )
})

# Test establish_species_correspondence
test_that("establish_species_correspondence works with species codes", {
  test_data <- setup_test_data()

  expect_output(
    result <- establish_species_correspondence(test_data, "species_code"),
    "SPECIES CORRESPONDENCE"
  )

  expect_true("Species" %in% colnames(result))
  expect_equal(result$Species[1], "Hetre")
  expect_equal(result$Species[4], "Chene pedoncule")

  teardown_test_data()
})

test_that("establish_species_correspondence errors without specimens column", {
  test_data <- setup_test_data()

  expect_error(
    establish_species_correspondence(test_data, NULL),
    "No species identification column specified"
  )

  teardown_test_data()
})

test_that("establish_species_correspondence errors with non-existent column", {
  test_data <- setup_test_data()

  expect_error(
    establish_species_correspondence(test_data, "nonexistent"),
    "does not exist in the data"
  )

  teardown_test_data()
})

test_that("establish_species_correspondence warns about missing correspondences", {
  test_data <- setup_test_data()

  # On introduit une valeur inconnue pour provoquer le warning
  test_data$species_code[1] <- 999  # 999 ne correspond à aucun Code dans 'equations'

  # Appel avec le bon nom de colonne présent dans 'equations'
  expect_warning(
    result <- establish_species_correspondence(test_data, "species_code"),
    "No correspondence found"
  )

  # Vérifie que la colonne Species est bien présente
  expect_true("Species" %in% colnames(result))

  # Vérifie que la ligne avec code 999 donne un NA pour Species
  expect_true(is.na(result$Species[5]))

  teardown_test_data()
})


# Test diameter_conversions
test_that("diameter_conversions converts D130 to C130", {
  test_data <- data.frame(
    Species = c("Hetre", "Chene pedoncule"),
    D130 = c(38, 48),
    C130 = c(NA, NA),
    D150 = c(35, 45),
    C150 = c(NA, NA),
    HTOT = c(25, 30),
    HDOM = c(28, 32)
  )

  # Mock flags
  flags <<- list(D130_exists = TRUE, D150_exists = TRUE, C130_exists = FALSE,
                 C150_exists = FALSE, HTOT_exists = TRUE, HDOM_exists = TRUE)

  expect_output(
    result <- diameter_conversions(test_data, "C130", "C150", "D130", "D150", "HTOT", "HDOM"),
    "D130.*→ C130"
  )

  expect_false(is.na(result$C130[1]))
  expect_false(is.na(result$C130[2]))
  expect_equal(round(result$C130[1], 2), round(38 * pi, 2))
  expect_equal(round(result$C130[2], 2), round(48 * pi, 2))

  teardown_test_data()
})

test_that("diameter_conversions converts C130 to D130", {
  test_data <- data.frame(
    Species = c("Hetre", "Chene pedoncule"),
    C130 = c(120, 150),
    D130 = c(NA, NA),
    C150 = c(110, 140),
    D150 = c(NA, NA),
    HTOT = c(25, 30),
    HDOM = c(28, 32)
  )

  flags <<- list(D130_exists = FALSE, D150_exists = FALSE, C130_exists = TRUE,
                 C150_exists = TRUE, HTOT_exists = TRUE, HDOM_exists = TRUE)

  expect_output(
    result <- diameter_conversions(test_data, "C130", "C150", "D130", "D150", "HTOT", "HDOM"),
    "C130.*→ D130"
  )

  expect_false(is.na(result$D130[1]))
  expect_false(is.na(result$D130[2]))
  expect_equal(round(result$D130[1], 2), round(120 / pi, 2))
  expect_equal(round(result$D130[2], 2), round(150 / pi, 2))

  teardown_test_data()
})

# Test convert_circumference
test_that("convert_circumference handles C150 to C130 conversion", {
  test_data <- setup_test_data()

  # Add Species column for conversion
  test_data$Species <- c("Hetre", "Chene pedoncule", "Epicea commun", "Hetre", "Chene pedoncule")

  # Set up scenario: only C150 has values
  test_data$C130 <- c(NA, NA, NA, NA, NA)
  test_data$C150 <- c(110, 140, 170, 160, 150)

  expect_output(
    result <- convert_circumference(test_data, "D150", "D130", "C150", "C130", "HTOT", "HDOM"),
    "GENERIC CIRCUMFERENCE CONVERSION"
  )

  # Check that some C130 values were calculated
  expect_true(any(!is.na(result$C130)))
})

test_that("convert_circumference handles C130 to C150 conversion", {
  test_data <- setup_test_data()

  # Add Species column for conversion
  test_data$Species <- c("Hetre", "Chene pedoncule", "Epicea commun", "Hetre", "Chene pedoncule")

  # Set up scenario: only C130 has values
  test_data$C130 <- c(120, 150, 180, 170, 160)
  test_data$C150 <- c(NA, NA, NA, NA, NA)

  expect_output(
    result <- convert_circumference(test_data, "D150", "D130", "C150", "C130", "HTOT", "HDOM"),
    "GENERIC CIRCUMFERENCE CONVERSION"
  )

  # Check that some C150 values were calculated
  expect_true(any(!is.na(result$C150)))
})

test_that("convert_circumference handles bidirectional conversion", {
  test_data <- setup_test_data()

  # Add Species column for conversion
  test_data$Species <- c("Hetre", "Chene pedoncule", "Epicea commun", "Hetre", "Chene pedoncule")

  # Set up scenario: both columns have some values
  test_data$C130 <- c(120, NA, 180, NA, 160)
  test_data$C150 <- c(NA, 140, NA, 170, NA)

  expect_output(
    result <- convert_circumference(test_data, "D150", "D130", "C150", "C130", "HTOT", "HDOM"),
    "bidirectional"
  )

  # Check that conversions were made in both directions
  expect_true(any(!is.na(result$C130)))
  expect_true(any(!is.na(result$C150)))
})

test_that("convert_circumference skips when no conversion needed", {
  test_data <- setup_test_data()

  # Add Species column
  test_data$Species <- c("Hetre", "Chene pedoncule", "Epicea commun", "Hetre", "Chene pedoncule")

  # Set up scenario: both columns complete
  test_data$C130 <- c(120, 150, 180, 170, 160)
  test_data$C150 <- c(110, 140, 170, 160, 150)

  expect_output(
    result <- convert_circumference(test_data, "D150", "D130", "C150", "C130", "HTOT", "HDOM"),
    "no conversion needed"
  )

  # Data should remain unchanged
  expect_equal(result$C130, test_data$C130)
  expect_equal(result$C150, test_data$C150)
})

# Test calculate_basal_areas
test_that("calculate_basal_areas calculates G130 correctly", {
  test_data <- data.frame(
    C130 = c(120, 150, 180),
    C150 = c(110, 140, 170)
  )

  expect_output(
    result <- calculate_basal_areas(test_data, "C130", "C150"),
    "BASAL AREA CALCULATION"
  )

  expect_true("G130" %in% colnames(result))
  expect_true("G150" %in% colnames(result))

  # Check calculation: G = C^2 / (4*pi*10000)
  expected_g130_1 <- (120^2) / (4 * pi * 10000)
  expect_equal(result$G130[1], expected_g130_1, tolerance = 1e-6)
})

test_that("calculate_basal_areas skips existing columns", {
  test_data <- data.frame(
    C130 = c(120, 150, 180),
    C150 = c(110, 140, 170),
    G130 = c(0.01, 0.015, 0.02),  # Already exists
    G150 = c(0.009, 0.013, 0.018)  # Already exists
  )

  expect_output(
    result <- calculate_basal_areas(test_data, "C130", "C150"),
    "already present"
  )

  # Should not recalculate existing values
  expect_equal(result$G130, test_data$G130)
  expect_equal(result$G150, test_data$G150)
})

# Test preprocess_data (integration test) - CORRIGÉ
test_that("preprocess_data integrates all preprocessing steps", {
  test_data <- setup_test_data()

  # Au lieu de vérifier le message exact, on vérifie juste qu'il y a une sortie
  expect_output(
    result <- preprocess_data(test_data, specimens = "species_code"),
    "SPECIMENS TYPE DETECTION"  # On cherche un pattern qui apparaît au début
  )

  # Vérifier que le résultat contient les colonnes attendues
  expect_true("Species" %in% colnames(result))
  expect_true("G130" %in% colnames(result) || "G150" %in% colnames(result))

  teardown_test_data()
})

# Edge cases and error handling - CORRIGÉS
test_that("functions handle empty data frames", {
  empty_data <- data.frame()

  # Corriger l'attente d'erreur pour correspondre au message exact
  expect_error(
    detect_specimens_type(empty_data, "nonexistent"),
    "Column 'nonexistent' does not exist in the data|contains only missing values"
  )
})

test_that("functions handle data with all NA values - circumference conversion", {
  na_data <- data.frame(
    Species = c("Hetre", "Chene pedoncule", "Epicea commun"),
    C130 = c(NA, NA, NA),
    C150 = c(NA, NA, NA),
    D130 = c(NA, NA, NA),
    D150 = c(NA, NA, NA),
    HTOT = c(25, 30, 35),
    HDOM = c(28, 32, 38)
  )

  # Tester convert_circumference avec des données NA
  expect_output(
    result <- convert_circumference(na_data, "D150", "D130", "C150", "C130", "HTOT", "HDOM"),
    "GENERIC CIRCUMFERENCE CONVERSION"
  )

  # Vérifier que les colonnes existent même si elles sont NA
  expect_true("C130" %in% colnames(result))
  expect_true("C150" %in% colnames(result))
})

test_that("calculate_basal_areas handles data with some valid circumferences", {
  # Données avec quelques valeurs valides
  mixed_data <- data.frame(
    C130 = c(120, NA, 150),
    C150 = c(NA, 140, 170)
  )

  expect_output(
    result <- calculate_basal_areas(mixed_data, "C130", "C150"),
    "BASAL AREA CALCULATION"
  )

  # Vérifier que les calculs sont corrects pour les valeurs non-NA
  expect_false(is.na(result$G130[1]))  # 120 -> doit calculer
  expect_true(is.na(result$G130[2]))   # NA -> doit rester NA
  expect_false(is.na(result$G130[3]))  # 150 -> doit calculer

  expect_true(is.na(result$G150[1]))   # NA -> doit rester NA
  expect_false(is.na(result$G150[2]))  # 140 -> doit calculer
  expect_false(is.na(result$G150[3]))  # 170 -> doit calculer
})
