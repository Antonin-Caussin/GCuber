test_that("as.carbofor_data adds class correctly", {
  df <- data.frame(x = 1:3)
  result <- as.carbofor_data(df)

  expect_s3_class(result, "carbofor_data")
  expect_s3_class(result, "data.frame")
})

test_that("update_flags correctly detects existing columns", {
  df <- data.frame(C130 = 1, C150 = 2, D130 = 3, D150 = 4, HTOT = 5, HDOM = 6)

  flags <- list(
    C130_exists = FALSE, C150_exists = FALSE,
    D130_exists = FALSE, D150_exists = FALSE,
    HTOT_exists = FALSE, HDOM_exists = FALSE
  )

  result <- update_flags(df, flags, "C130", "C150", "D130", "D150", "HTOT", "HDOM")
  expect_true(all(unlist(result)))
})

test_that("detect_specimens_type detects numeric codes", {
  df <- data.frame(species_code = c(1, 2, 3))
  expect_equal(detect_specimens_type(df, "species_code"), "Code")
})

test_that("detect_specimens_type detects abbreviations", {
  df <- data.frame(species_abr = c("HE", "CHP", "EP"))
  expect_equal(detect_specimens_type(df, "species_abr"), "Abr")
})

test_that("detect_specimens_type detects full names", {
  df <- data.frame(species_name = c("Hetre", "Chene pedoncule", "Epicea commun"))
  expect_equal(detect_specimens_type(df, "species_name"), "Species")
})

test_that("detect_specimens_type fails with empty column", {
  df <- data.frame(empty = c(NA, NA, NA))
  expect_error(detect_specimens_type(df, "empty"), "contains only missing values")
})


test_that("detect_specimens_type fails with unknown type", {
  df <- data.frame(date = as.Date(c("2023-01-01", "2023-01-02")))
  expect_error(detect_specimens_type(df, "date"), "Data type in column.*not recognized")
})

test_that("preprocess_data returns expected structure", {
  equations <<- data.frame(
    Species = c("Hetre", "Chene pedoncule", "Epicea commun"),
    Code = c(3, 97, 41),
    HV = c(0.95, 0.98, 0.92),
    IV = c(2.5, 3.1, 2.8),
    stringsAsFactors = FALSE
  )

  df <- data.frame(
    Code = c(3, 97, 41),
    D130 = c(30, 40, 50),
    HTOT = c(25, 28, 30),
    HDOM = c(28, 30, 32)
  )

  result <- preprocess_data(df, specimens = "Code")

  expect_s3_class(result, "data.frame")
  expect_true("Species" %in% names(result))
  expect_true("G130" %in% names(result) || "G150" %in% names(result))
})
