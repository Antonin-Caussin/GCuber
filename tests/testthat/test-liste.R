# =====================================================================
#                         Build_equations()
# =====================================================================

# tests/testthat/test-build_equation.R
test_that("formats intercept + positive/negative terms correctly", {
  res <- build_equation(
    x_cols = c("1", "DBH", "DBH^2"),
    b_cols = c(1.5, 0.02, -0.001),
    y_var  = "Volume"
  )
  expect_equal(res, "Volume = 1.5000 + 0.0200*DBH - 0.0010*DBH^2")
})

test_that("drops zero and NA coefficients and NA variables", {
  res <- build_equation(
    x_cols = c("1", "DBH", NA, "H"),
    b_cols = c(0, 0.02, 5, NA),
    y_var  = "V"
  )
  expect_equal(res, "V = 0.0200*DBH")
})

test_that("first valid term is not prefixed with a sign when index 1 is invalid", {
  res <- build_equation(
    x_cols = c("1", "DBH"),
    b_cols = c(0, 0.02),
    y_var  = "Y"
  )
  expect_equal(res, "Y = 0.0200*DBH")
})

test_that("works with intercept only", {
  res <- build_equation(
    x_cols = c("1"),
    b_cols = c(3),
    y_var  = "M"
  )
  expect_equal(res, "M = 3.0000")
})

test_that("returns 'No valid equation' when nothing is valid", {
  res1 <- build_equation(
    x_cols = c("1", "X"),
    b_cols = c(0, 0),
    y_var  = "Z"
  )
  res2 <- build_equation(
    x_cols = c(NA, NA),
    b_cols = c(NA, NA),
    y_var  = "Z"
  )
  expect_equal(res1, "No valid equation")
  expect_equal(res2, "No valid equation")
})

test_that("formats subsequent constant terms (var == '1' or empty)", {
  res <- build_equation(
    x_cols = c("DBH", "1"),
    b_cols = c(0.02, -5),
    y_var  = "Q"
  )
  expect_equal(res, "Q = 0.0200*DBH - 5.0000")

  res2 <- build_equation(
    x_cols = c("DBH", ""),
    b_cols = c(0.02, 5),
    y_var  = "Q"
  )
  expect_equal(res2, "Q = 0.0200*DBH + 5.0000")
})

test_that("ignores '0' variable but accepts empty string as constant", {
  res <- build_equation(
    x_cols = c("0", ""),
    b_cols = c(10, 2),
    y_var  = "R"
  )
  expect_equal(res, "R = 2.0000")
})

test_that("uses fixed 4-decimal formatting and spacing", {
  res <- build_equation(
    x_cols = c("1", "X"),
    b_cols = c(1, 2),
    y_var  = "S"
  )
  expect_match(res, "^S = ")
  expect_match(res, "1\\.0000 \\+ 2\\.0000\\*X$")
})

# =====================================================================
#                         carbofor_species()
# =====================================================================

# skip if the internal dataset is not found in the package namespace
skip_if_no_equations <- function() {
  pkg <- "GCuber"
  skip_if_not(requireNamespace(pkg, quietly = TRUE), "GCuber namespace not available")
  skip_if_not(exists("equations", envir = asNamespace(pkg), inherits = FALSE),
              "'equations' not found in the package namespace")
}

# retrieve the internal 'equations' data frame from the package namespace
get_equations <- function() {
  get("equations", envir = asNamespace("GCuber"))
}

# real column name for "Available equations" (tolerate space/points)
avail_col_name <- function(df) {
  nm <- names(df)
  idx <- which(gsub("\\.", " ", nm) == "Available equations")
  if (length(idx) == 1) nm[idx] else NULL
}

# ensure optional columns exist (mimics the function behavior)
prep_equations_for_summary <- function(eq) {
  if (!"Latin"     %in% names(eq)) eq$Latin     <- NA_character_
  if (!"Code"      %in% names(eq)) eq$Code      <- NA_character_
  if (!"Abr"       %in% names(eq)) eq$Abr       <- NA_character_
  if (!"Source_Eq" %in% names(eq)) eq$Source_Eq <- NA_character_
  eq
}

# --------------------------------------------------------------------
# Prerequisites
# --------------------------------------------------------------------

test_that("carbofor_species() - prerequisites: internal dataset 'equations' available and key columns present", {
  skip_if_no_equations()
  equations <- get_equations()
  req_cols <- c("Species", "Latin", "Y", "A0")
  miss <- setdiff(req_cols, names(equations))
  expect_true(
    all(req_cols %in% names(equations)),
    paste("Missing columns:", paste(miss, collapse = ", "))
  )
})

# --------------------------------------------------------------------
# Summary mode (no filters)
# --------------------------------------------------------------------

test_that("summary mode: grouping by Latin/Code/Abr/Source and listing Y values", {
  skip_if_no_equations()
  equations <- get_equations()

  out <- carbofor_species(plot = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("Species", "Code", "Abreviation", "Source") %in% names(out)))

  acn <- avail_col_name(out)
  expect_false(is.null(acn))
  expect_true(is.character(out[[acn]]) || is.factor(out[[acn]]))

  eq <- prep_equations_for_summary(equations)

  # Every (Latin, Code, Abr, Source_Eq) combination must be present in the summary
  expected <- unique(
    data.frame(
      Species     = eq$Latin,
      Code        = eq$Code,
      Abreviation = eq$Abr,
      Source      = eq$Source_Eq,
      stringsAsFactors = FALSE
    )
  )
  merged <- merge(
    expected,
    unique(out[c("Species", "Code", "Abreviation", "Source")]),
    by = c("Species", "Code", "Abreviation", "Source"),
    all.x = TRUE
  )
  expect_false(any(is.na(merged$Species)),
               "Some Latin/Code/Abr/Source combinations are missing in the summary output")

  # Targeted check on one non-NA combination
  ref <- subset(eq, !is.na(Latin) & !is.na(Source_Eq))[1, , drop = TRUE]
  if (length(ref)) {
    row_out <- subset(out,
                      Species == ref[["Latin"]] &
                        (is.na(Code) | Code == ref[["Code"]]) &
                        (is.na(Abreviation) | Abreviation == ref[["Abr"]]) &
                        (is.na(Source) | Source == ref[["Source_Eq"]]))
    expect_gt(nrow(row_out), 0)

    y_exp <- sort(unique(subset(eq,
                                Latin == ref[["Latin"]] &
                                  (is.na(Code) | Code == ref[["Code"]]) &
                                  (is.na(Abr)  | Abr  == ref[["Abr"]])  &
                                  (is.na(Source_Eq) | Source_Eq == ref[["Source_Eq"]]))$Y))
    y_exp <- paste(y_exp, collapse = ", ")
    expect_true(any(as.character(row_out[[acn]]) == y_exp))
  }
})

# --------------------------------------------------------------------
# Filter by species (detailed mode)
# --------------------------------------------------------------------

test_that("filter by species: consistent rows and detailed columns if present", {
  skip_if_no_equations()
  equations <- get_equations()

  sp_candidates <- unique(na.omit(equations$Species))
  skip_if(length(sp_candidates) == 0, "No species available")
  species_sel <- NULL
  for (sp in sp_candidates) {
    if (any(!is.na(subset(equations, Species == sp)$Latin))) { species_sel <- sp; break }
  }
  skip_if(is.null(species_sel), "No species with non-NA Latin name")

  out <- carbofor_species(species = species_sel, plot = FALSE)
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(c("Species", "Code", "Abreviation", "Source") %in% names(out)))

  lat_ok <- unique(subset(equations, Species == species_sel)$Latin)
  lat_ok <- lat_ok[!is.na(lat_ok)]
  expect_true(all(out$Species %in% lat_ok))

  det_cols <- c("Equation_ID", "Volume_Type")
  if (all(det_cols %in% names(out))) {
    src <- subset(equations, Species == species_sel)
    expect_true(all(out$Equation_ID %in% src$A0))
    expect_true(all(out$Volume_Type %in% src$Y))
  }
})

# --------------------------------------------------------------------
# Filter by equation type (detailed mode)
# --------------------------------------------------------------------

test_that("filter by equation type: Volume_Type matches filter and IDs are consistent", {
  skip_if_no_equations()
  equations <- get_equations()

  y_vals <- unique(na.omit(equations$Y))
  skip_if(length(y_vals) == 0, "No equation type available in `equations`")
  y_sel <- y_vals[1]

  out <- carbofor_species(equation_type = y_sel, plot = FALSE)
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)

  if ("Volume_Type" %in% names(out)) {
    expect_true(all(out$Volume_Type == y_sel))
  }
  if ("Equation_ID" %in% names(out)) {
    src_ids <- subset(equations, Y == y_sel)$A0
    expect_true(all(out$Equation_ID %in% src_ids))
  }
})

# --------------------------------------------------------------------
# Double filter (species + type)
# --------------------------------------------------------------------

test_that("double filter (species + type): output respects both filters", {
  skip_if_no_equations()
  equations <- get_equations()

  ok <- with(equations, !is.na(Species) & !is.na(Y))
  skip_if(!any(ok), "No (Species, Y) pair in `equations`")
  found <- FALSE
  for (i in which(ok)) {
    sp <- equations$Species[i]; yv <- equations$Y[i]
    if (any(!is.na(subset(equations, Species == sp)$Latin))) {
      species_sel <- sp; y_sel <- yv; found <- TRUE; break
    }
  }
  skip_if_not(found, "No valid (Species, Y) pair with non-NA Latin name")

  out <- carbofor_species(species = species_sel, equation_type = y_sel, plot = FALSE)
  expect_s3_class(out, "data.frame")

  if ("Volume_Type" %in% names(out)) {
    expect_true(all(out$Volume_Type == y_sel))
  }
  latins <- unique(subset(equations, Species == species_sel)$Latin)
  latins <- latins[!is.na(latins)]
  if (length(latins)) {
    expect_true(all(out$Species %in% latins))
  }
  if ("Equation_ID" %in% names(out)) {
    src_ids <- subset(equations, Species == species_sel & Y == y_sel)$A0
    expect_true(all(out$Equation_ID %in% src_ids))
  }
})

# --------------------------------------------------------------------
# Case "no result" for unknown species
# --------------------------------------------------------------------

test_that("unknown species -> empty data.frame with summary columns", {
  skip_if_no_equations()

  species_abs <- paste0("xxx_", as.integer(runif(1, 1e6, 2e6)))

  # Function prints messages via cat(); capture output instead of expect_silent
  out_lines <- capture.output({
    out <- carbofor_species(species = species_abs, plot = FALSE)
    assign("out_res", out, inherits = TRUE) # keep result for checks
  })

  out <- get("out_res", inherits = TRUE)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)

  # Expected columns (tolerate space/points for Available equations)
  expect_true(all(c("Species", "Code", "Abreviation", "Source") %in% names(out)))
  acn <- avail_col_name(out)
  expect_false(is.null(acn))

  # Check that informative messages were printed
  expect_true(any(grepl("^No equations found for species:", out_lines)))
  expect_true(any(grepl("^Available species:", out_lines)))
})

# --------------------------------------------------------------------
# Call without display
# --------------------------------------------------------------------

test_that("plot = FALSE returns a data.frame without warnings", {
  skip_if_no_equations()
  expect_silent({
    out <- carbofor_species(plot = FALSE)
    expect_s3_class(out, "data.frame")
    expect_gt(nrow(out), 0)
  })
})


# =====================================================================
#                         species() alias
# =====================================================================

test_that("species() is a transparent alias of carbofor_species()", {
  skip_if_no_equations()

  # No filters: strict identity with carbofor_species()
  expect_silent({
    out_alias <- species(plot = FALSE)
    out_base  <- carbofor_species(plot = FALSE)
  })
  expect_identical(out_alias, out_base)

  # With a real species picked from internal dataset
  eq <- get_equations()
  sp_candidates <- unique(na.omit(eq$Species))
  skip_if(length(sp_candidates) == 0, "No species available in equations")
  sp_sel <- sp_candidates[1]

  expect_silent({
    out_alias <- species(sp_sel, plot = FALSE)
    out_base  <- carbofor_species(sp_sel, plot = FALSE)
  })
  expect_identical(out_alias, out_base)
})

# =====================================================================
#                         equation() helper
# =====================================================================

test_that("equation() presets equation_type identically to carbofor_species(equation_type=)", {
  skip_if_no_equations()
  eq <- get_equations()
  skip_if(!("Y" %in% names(eq)), "Column Y missing in equations")
  y_vals <- unique(na.omit(eq$Y))
  skip_if(length(y_vals) == 0, "No equation types in equations$Y")

  y_sel <- y_vals[1]

  expect_silent({
    out_wrap <- equation(y_sel, plot = FALSE)
    out_base <- carbofor_species(equation_type = y_sel, plot = FALSE)
  })
  expect_identical(out_wrap, out_base)

  # Optional combo: species that actually has this Y
  sp_with_y <- unique(eq$Species[eq$Y == y_sel & !is.na(eq$Species)])
  if (length(sp_with_y) > 0) {
    sp_sel <- sp_with_y[1]
    expect_silent({
      out_wrap <- equation(y_sel, species = sp_sel, plot = FALSE)
      out_base <- carbofor_species(species = sp_sel, equation_type = y_sel, plot = FALSE)
    })
    expect_identical(out_wrap, out_base)
  }
})

# =====================================================================
#                         list_species()
# =====================================================================

test_that("list_species() prints a numbered list and returns the unique Species vector", {
  skip_if_no_equations()
  eq <- get_equations()
  skip_if(!("Species" %in% names(eq)), "Column Species missing in equations")

  printed <- capture.output({
    res <- list_species()
    assign("res_species_vec", res, inherits = TRUE)
  })

  expected_vec <- sort(unique(eq$Species))
  expect_identical(get("res_species_vec", inherits = TRUE), expected_vec)

  expect_true(any(grepl("^Available species in carbofor:", printed)))
  expect_true(any(grepl("^\\d+\\.\\s+.+", printed)))         # at least one "1. Name"
  expect_true(any(grepl("^\\s*Total:\\s*\\d+\\s*species\\s*$", printed)))
})

# =====================================================================
#                    list_equation_types()
# =====================================================================

test_that("list_equation_types() prints a numbered list and returns the unique Y vector", {
  skip_if_no_equations()
  eq <- get_equations()
  skip_if(!("Y" %in% names(eq)), "Column Y missing in equations")

  printed <- capture.output({
    res <- list_equation_types()
    assign("res_types_vec", res, inherits = TRUE)
  })

  expected_vec <- sort(unique(eq$Y))
  expect_identical(get("res_types_vec", inherits = TRUE), expected_vec)

  expect_true(any(grepl("^Available equation types in carbofor:", printed)))
  expect_true(any(grepl("^\\d+\\.\\s+.+", printed)))          # at least one "1. Type"
  expect_true(any(grepl("^\\s*Total:\\s*\\d+\\s*equation types\\s*$", printed)))
})
