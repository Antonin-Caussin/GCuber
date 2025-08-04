

test_that("Test avec C130 et HTOT", {
  data <- data.frame(
    Species = c("Picea abies", "Fagus sylvatica"),
    C130 = c(94.2, 125.6),
    HTOT = c(25.5, 28.2)
  )
  result <- carbofor(data, specimens = "Species", volume_type = "V22", equation_id = 1)
  expect_true("V22" %in% colnames(result))
  expect_true("G130" %in% colnames(result))
  expect_true("D130" %in% colnames(result))
})

test_that("Test avec D130 uniquement", {
  data <- data.frame(
    Species = c("Epicea commun", "Hetre"),
    D130 = c(30, 40),
    HTOT = c(25, 28)
  )
  result <- carbofor(data,specimens = "Species", volume_type = "V22B", equation_id = 1)
  expect_true("V22B" %in% colnames(result))
  expect_true("C130" %in% colnames(result))
  expect_true("G130" %in% colnames(result))
})


test_that("Test avec C150 uniquement", {
  data <- data.frame(
    Species = c("Picea abies", "Fagus sylvatica"),
    C150 = c(100, 130),
    HTOT = c(25, 28)
  )
  result <- carbofor(data,specimens = "Species", volume_type = "V22", equation_id = 1)
  expect_true("V22" %in% colnames(result))
  expect_true("G150" %in% colnames(result))
  expect_true("D150" %in% colnames(result))
})

test_that("Test avec abréviations d'espèces", {
  data <- data.frame(
    Abr = c("PIAB", "FASY"),
    C130 = c(94.2, 125.6),
    HTOT = c(25.5, 28.2)
  )
  result <- carbofor(data, specimens = "Abr", volume_type = "V22", equation_id = 1)
  expect_s3_class(result, "data.frame")
  expect_true("V22" %in% colnames(result))
  expect_true("G130" %in% colnames(result))
  expect_true("D130" %in% colnames(result))
})


test_that("Test avec données mixtes (D130 et C150)", {
  data <- data.frame(
    Species = c("Picea abies", "Fagus sylvatica"),
    D130 = c(30, NA),
    C150 = c(NA, 125.6),
    HTOT = c(25.5, 28.2)
  )
  result <- carbofor(data, specimens = "Species",volume_type = "V22", equation_id = 1)
  expect_true("V22" %in% colnames(result))
  expect_true("D130" %in% colnames(result))
  expect_true("C130" %in% colnames(result))
  expect_true("D150" %in% colnames(result))
  expect_true("G130" %in% colnames(result))
})

