# Test file for validate_parameters function
# tests/testthat/test-validate-parameters.R

library(testthat)

# Test data setup
create_test_data <- function() {
  data.frame(
    C130 = c(120, 150, 180),
    C150 = c(110, 140, 170),
    D130 = c(38, 48, 57),
    D150 = c(35, 45, 54),
    HTOT = c(25, 30, 35),
    HDOM = c(28, 32, 38),
    species = c("Fagus", "Quercus", "Picea")
  )
}

test_that("validate_parameters works with valid inputs", {
  test_data <- create_test_data()

  # Test with default parameters
  result <- validate_parameters(test_data)
  expect_identical(result, test_data)

  # Test with all valid parameters
  result <- validate_parameters(
    x = test_data,
    volume_type = "V22",
    equation_id = 1,
    source = "Dagnelie",
    specimens = "species"
  )
  expect_identical(result, test_data)
})

test_that("validate_parameters handles invalid source", {
  test_data <- create_test_data()

  expect_error(
    validate_parameters(test_data, source = "InvalidSource"),
    regexp = "Invalid source: InvalidSource"
  )

  expect_error(
    validate_parameters(test_data, source = "Dagnellie"),  # case sensitive
    regexp = "Invalid source: Dagnellie"
  )
})

test_that("validate_parameters handles invalid volume_type", {
  test_data <- create_test_data()

  expect_error(
    validate_parameters(test_data, volume_type = "InvalidType"),
    regexp = "Invalid volume type: InvalidType"
  )

  expect_error(
    validate_parameters(test_data, volume_type = "v22"),  # case sensitive
    regexp = "Invalid volume type: v22"
  )
})

test_that("validate_parameters handles equation_id validation for V22", {
  test_data <- create_test_data()

  # Valid equation_ids for V22
  expect_silent(validate_parameters(test_data, volume_type = "V22", equation_id = 1))
  expect_silent(validate_parameters(test_data, volume_type = "V22", equation_id = 2))
  expect_silent(validate_parameters(test_data, volume_type = "V22", equation_id = 3))

  # Invalid equation_ids for V22
  expect_error(
    validate_parameters(test_data, volume_type = "V22", equation_id = 0),
    regexp = "For volume type 'V22', equation_id must be 1, 2, or 3"
  )

  expect_error(
    validate_parameters(test_data, volume_type = "V22", equation_id = 4),
    regexp = "For volume type 'V22', equation_id must be 1, 2, or 3"
  )
})

test_that("validate_parameters handles equation_id validation for V22B", {
  test_data <- create_test_data()

  # Valid equation_id for V22B
  expect_silent(validate_parameters(test_data, volume_type = "V22B", equation_id = 1))

  # Invalid equation_ids for V22B
  expect_error(
    validate_parameters(test_data, volume_type = "V22B", equation_id = 2),
    regexp = "For volume type 'V22B', only equation_id = 1 is allowed"
  )

  expect_error(
    validate_parameters(test_data, volume_type = "V22B", equation_id = 3),
    regexp = "For volume type 'V22B', only equation_id = 1 is allowed"
  )
})

test_that("validate_parameters handles equation_id validation for V22_HA", {
  test_data <- create_test_data()

  # Valid equation_id for V22_HA
  expect_silent(validate_parameters(test_data, volume_type = "V22_HA", equation_id = 1))

  # Invalid equation_ids for V22_HA
  expect_error(
    validate_parameters(test_data, volume_type = "V22_HA", equation_id = 2),
    regexp = "For volume type 'V22_HA', only equation_id = 1 is allowed"
  )
})


test_that("validate_parameters handles missing columns with warnings", {
  # Test data missing some columns
  incomplete_data <- data.frame(
    C130 = c(120, 150),
    D130 = c(38, 48),
    HTOT = c(25, 30)
    # Missing C150, D150, HDOM
  )

  expect_warning(
    validate_parameters(incomplete_data),
    regexp = "Missing columns in data: C150, D150, HDOM"
  )
})

test_that("validate_parameters handles missing specimens column", {
  test_data <- create_test_data()

  # Remove species column but specify it as specimens
  test_data_no_species <- test_data[, !names(test_data) %in% "species"]

  expect_warning(
    validate_parameters(test_data_no_species, specimens = "species"),
    regexp = "Missing columns in data: species"
  )
})


test_that("validate_parameters errors when no diameter/circumference columns exist", {
  # Data with no diameter or circumference columns
  invalid_data <- data.frame(
    HTOT = c(25, 30, 35),
    HDOM = c(28, 32, 38),
    other_col = c(1, 2, 3)
  )

  expect_error(
    validate_parameters(invalid_data),
    regexp = "No diameter or circumference column found in the data"
  )
})


test_that("validate_parameters works with custom column names", {
  # Test data with custom column names
  custom_data <- data.frame(
    circ_130 = c(120, 150, 180),
    circ_150 = c(110, 140, 170),
    diam_130 = c(38, 48, 57),
    diam_150 = c(35, 45, 54),
    height_total = c(25, 30, 35),
    height_dominant = c(28, 32, 38)
  )

  result <- validate_parameters(
    x = custom_data,
    C130 = "circ_130",
    C150 = "circ_150",
    D130 = "diam_130",
    D150 = "diam_150",
    HTOT = "height_total",
    HDOM = "height_dominant"
  )

  expect_identical(result, custom_data)
})

test_that("validate_parameters validates all source options", {
  test_data <- create_test_data()

  # Test all valid sources
  expect_silent(validate_parameters(test_data, source = "Dagnelie"))
  expect_silent(validate_parameters(test_data, source = "Algan"))  # <- CORRECT
  expect_silent(validate_parameters(test_data, source = "Vallet"))
  expect_silent(validate_parameters(test_data, source = "Bouvard"))
  expect_silent(validate_parameters(test_data, source = "Courbet"))
  expect_silent(validate_parameters(test_data, source = "Rondeu"))
})


test_that("validate_parameters validates all volume_type options", {
  test_data <- create_test_data()

  expect_silent(validate_parameters(test_data, volume_type = "V22", equation_id = 1))
  expect_silent(validate_parameters(test_data, volume_type = "V22B", equation_id = 1))
  expect_silent(validate_parameters(test_data, volume_type = "V22_HA", equation_id = 1))
  expect_silent(validate_parameters(test_data, volume_type = "Aboveground", equation_id = 1))  # corrigé ici
})


test_that("validate_parameters handles edge cases", {
  test_data <- create_test_data()

  # Test with NULL specimens (default)
  expect_silent(validate_parameters(test_data, specimens = NULL))

  # Test with empty data frame
  empty_data <- data.frame()
  expect_error(
    validate_parameters(empty_data),
    regexp = "No diameter or circumference column found in the data"
  )
})

