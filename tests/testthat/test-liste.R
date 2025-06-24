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
