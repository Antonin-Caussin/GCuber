test_that("carbofor_species returns expected structure", {
  res <- carbofor_species()
  expect_s3_class(res, "data.frame")
  expect_true(ncol(res) >= 3)
  expect_true(all(c("Species", "Available equations", "Sources") %in% names(res)))
})

test_that("carbofor_species filters correctly by species name in French", {
  res <- carbofor_species(species = "Chene pedoncule")
  expect_true(all(grepl("Chene pedoncule", res$Species, ignore.case = TRUE)))
})

test_that("carbofor_species filters correctly by equation_type", {
  res <- carbofor_species(equation_type = "V22")
  expect_true(all(res[["Available_equations"]] == "V22"))
})


test_that("carbofor_species handles invalid species gracefully", {
  res <- carbofor_species(species = "Essence imaginaire")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("carbofor_species handles multiple filters with French name", {
  res <- carbofor_species(species = "Chene sessile", equation_type = "V22")
  expect_true(all(grepl("Chene sessile", res$Species, ignore.case = TRUE)))
  expect_true(all(res[["Available_equations"]] == "V22"))
})

test_that("species is a working alias for carbofor_species", {
  res1 <- carbofor_species("Chene pedoncule")
  res2 <- species("Chene pedoncule")
  expect_equal(res1, res2)
})

test_that("carbofor_species returns all data with no arguments", {
  all1 <- carbofor_species()
  all2 <- species()
  expect_equal(nrow(all1), nrow(all2))
  expect_gt(nrow(all1), 0)
})

test_that("build_equation formats a simple equation with intercept and variable", {
  eq <- build_equation(c("1", "X"), c(2, 3), "Y")
  expect_equal(eq, "Y = 2.0000 + 3.0000*X")
})

test_that("build_equation handles multiple variables and negative signs", {
  eq <- build_equation(c("1", "A", "B"), c(1, -0.5, 2), "Z")
  expect_equal(eq, "Z = 1.0000 - 0.5000*A + 2.0000*B")
})

test_that("build_equation returns 'No valid equation' when all coefficients are zero or NA", {
  eq1 <- build_equation(c("1", "X"), c(0, 0), "Y")
  eq2 <- build_equation(c("1", "X"), c(NA, NA), "Y")
  expect_equal(eq1, "No valid equation")
  expect_equal(eq2, "No valid equation")
})

test_that("detailed mode returns expected columns when both species and equation_type are provided", {
  res <- carbofor_species(species = "Chene pedoncule", equation_type = "V22", plot = FALSE)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("Species", "Equation_ID", "Volume_Type", "Source", "Equation") %in% names(res)))
})

test_that("equation() works as an alias for carbofor_species", {
  r1 <- carbofor_species(equation_type = "V22", plot = FALSE)
  r2 <- equation("V22", plot = FALSE)
  expect_equal(r1, r2)
})

test_that("biomass alias filters for 'biomass' via equation()", {
  r_bio1 <- equation("biomass", plot = FALSE)
  expect_true(all(r_bio1$Volume_Type == "biomass"))
})

test_that("list_species() returns an invisible character vector and prints total count", {
  out <- capture.output(ss <- list_species())
  expect_silent(invisible(ss))
  expect_type(ss, "character")
  expect_gt(length(ss), 0)
  expect_match(out[length(out)], "^Total: \\d+ species$")
})

test_that("list_equation_types() returns an invisible character vector and prints total count", {
  out <- capture.output(tt <- list_equation_types())
  expect_silent(invisible(tt))
  expect_type(tt, "character")
  expect_gt(length(tt), 0)
  expect_match(out[length(out)], "^Total: \\d+ equation types$")
})

test_that("carbofor_species(plot = FALSE) prints summary header and returns a data.frame", {
  out <- capture.output(res <- carbofor_species(plot = FALSE))
  lines_trim <- trimws(out)
  sep_lines <- lines_trim[lines_trim == paste(rep("=", 60), collapse = "")]
  expect_true(length(sep_lines) >= 2)
  expect_true(any(grepl("AVAILABLE SPECIES AND EQUATIONS IN CARBOFOR", out)))
  expect_s3_class(res, "data.frame")
})




test_that("invalid equation_type filter returns zero rows", {
  res <- carbofor_species(equation_type = "TYPE_INEXISTANT", plot = FALSE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})


