test_that("Volume est bien calculé avec a0 = 5", {
  df_test <- data.frame(
    ID = 1:2,
    Essence = c("Chêne", "Chêne"),
    C130 = c(30, 35),
    Htot = c(20, 25)
  )

  result <- calculer_volumes(df = df_test, type_volume = "VC22")

  expect_true("Volume" %in% colnames(result))
  expect_true("Equation_Utilisee" %in% colnames(result))
  expect_false(any(is.na(result$Volume)))
  expect_true(all(result$Volume > 0))
})


test_that("Volume est bien calculé avec a0 = 4 (log)", {
  df_test <- data.frame(
    Essence = "Hêtre",
    C130 = 40,
    Htot = 30
  )

  result <- calculer_volumes(df = df_test, type_volume = "VC22")

  expect_true(result$Volume > 0)
})


test_that("Erreur si type_volume est invalide", {
  df_test <- data.frame(Essence = "Chêne", C130 = 30)

  expect_error(
    calculer_volumes(df = df_test, type_volume = "FAUX_VOLUME"),
    "Type de volume invalide"
  )
})


test_that("Erreur si variable requise manquante (ex: C130)", {
  df_test <- data.frame(Essence = "Chêne")

  expect_error(
    calculer_volumes(df = df_test, type_volume = "VC22"),
    "au moins une colonne 'C130' ou 'C150'"
  )
})


test_that("Conversion C150 vers C130 fonctionne", {
  df_test <- data.frame(Essence = "Chêne", C150 = 40)
  coefs_test <- data.frame(Essence = "Chêne", Coef_C150_C130 = 0.95)

  result <- calculer_volumes(df = df_test, coefs_conversion = coefs_test, type_volume = "VC22")

  expect_equal(result$C130, 40 * 0.95)
  expect_true(all(result$Volume > 0))
})


test_that("Erreur si coef de conversion manquant", {
  df_test <- data.frame(Essence = "Chêne", C150 = 40)
  coefs_test <- data.frame(Essence = "Hêtre", Coef_C150_C130 = 0.95)

  expect_error(
    calculer_volumes(df = df_test, coefs_conversion = coefs_test, type_volume = "VC22"),
    "Coefficient de conversion manquant"
  )
})


test_that("Warn si essence inconnue dans les équations", {
  df_test <- data.frame(Essence = "Inconnue", C130 = 30)

  expect_warning(
    result <- calculer_volumes(df = df_test, type_volume = "VC22"),
    "Pas d'équation trouvée pour l'essence"
  )
})


test_that("Warn si log <= 0 pour a0 = 4", {
  df_test <- data.frame(Essence = "Hêtre", C130 = -10, Htot = 30)

  expect_warning(
    result <- calculer_volumes(df = df_test, type_volume = "VC22"),
    "Valeur négative ou nulle pour logarithme"
  )
})


test_that("Warn si volume NA ou non-fini", {
  df_test <- data.frame(Essence = "Chêne", C130 = NA, Htot = 20)

  expect_warning(
    result <- calculer_volumes(df = df_test, type_volume = "VC22"),
    "Résultat de volume non valide"
  )
})


test_that("remove_na fonctionne", {
  df_test <- data.frame(Essence = "Chêne", C130 = NA)

  result <- calculer_volumes(df = df_test, type_volume = "VC22", remove_na = TRUE)

  expect_equal(nrow(result), 0)
})


test_that("Erreur si id_equation trop grand", {
  df_test <- data.frame(Essence = "Chêne", C130 = 30)

  expect_error(
    calculer_volumes(df = df_test, type_volume = "VC22", id_equation = 99),
    "id_equation = 99 dépasse"
  )
})


test_that("Erreur si variable utilisée dans l’équation absente des données", {
  df_test <- data.frame(Essence = "Chêne", Htot = 25)

  expect_error(
    calculer_volumes(df = df_test, type_volume = "VC22"),
    "est utilisée dans une équation mais absente des données"
  )
})
