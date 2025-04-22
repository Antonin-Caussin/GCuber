test_that("Volume est bien calcule avec a0 = 5", {
  df_test <- data.frame(
    ID = 1:2,
    Essence = c("Chene Ind", "Chene Ind"),
    C130 = c(30, 35),
    HDOM = c(20, 25),
    HTOT = c(19,26)
  )
  result <- calculer_volumes(df = df_test, type_volume = "E")

  expect_true("Volume" %in% colnames(result))
  expect_true("Equation_Utilisee" %in% colnames(result))
  expect_false(any(is.na(result$Volume)))
  expect_true(all(result$Volume > 0))
})


test_that("Volume est bien calcule avec a0 = 4 (log)", {
  df_test <- data.frame(
    Essence = "Hetre",
    C130 = 40,
    HTOT = 30,
    HDOM = 25
  )

  result <- calculer_volumes(df = df_test, type_volume = "VC22")

  expect_true(result$Volume > 0)
})


test_that("Erreur si type_volume est invalide", {
  df_test <- data.frame(Essence = "Chene Ind", C130 = 30)

  expect_error(
    calculer_volumes(df = df_test, type_volume = "FAUX_VOLUME"),
    "Type de volume invalide"
  )
})



test_that("Erreur si variable requise manquante (ex: C130)", {
  df_test <- data.frame(Essence = "Chene Ind")

  expect_error(
    calculer_volumes(df = df_test, type_volume = "VC22"),
    "au moins une colonne 'C130' ou 'C150'"
  )
})


test_that("Conversion C150 vers C130 fonctionne", {
  df_test <- data.frame(Essence = "Chene Ind", C150 = 40, HDOM= 30, HTOT = 28)
  coefs_test <- data.frame(Essence = "Chene Ind", Coef_C150_C130 = 0.95)

  result <- calculer_volumes(df = df_test, coefs_conversion = coefs_test, type_volume = "VC22")

  expect_equal(result$C130, 40 * 0.95)
  expect_true(all(result$Volume > 0))
})


test_that("Erreur si coef de conversion manquant", {
  df_test <- data.frame(Essence = "Chene Ind", C150 = 40)
  coefs_test <- data.frame(Essence = "Hetre", Coef_C150_C130 = 0.95)

  expect_error(
    calculer_volumes(df = df_test, coefs_conversion = coefs_test, type_volume = "VC22"),
    "Coefficient de conversion manquant"
  )
})


test_that("Warn si essence inconnue dans les equations", {
  df_test <- data.frame(Essence = "Inconnue", C130 = 30, HDOM = 25, HTOT =12)

  expect_warning(
    result <- calculer_volumes(df = df_test, type_volume = "VC22"),
    "Pas d'equation trouvee pour l'essence"
  )
})


test_that("Warn si log <= 0 pour a0 = 4", {
  df_test <- data.frame(Essence = "Pin sylvestre", C130 = -10, HTOT = 30, HDOM = 28)

  expect_warning(
    result <- calculer_volumes(df = df_test, type_volume = "E"),
    "Valeur negative ou nulle pour logarithme"
  )
})


test_that("Warn si volume NA ou non-fini", {
  df_test <- data.frame(Essence = "Chene Ind", C130 = NA, HTOT = 20, HDOM = 25)

  expect_warning(
    result <- calculer_volumes(df = df_test, type_volume = "VC22"),
    "Resultat de volume non valide"
  )
})


test_that("remove_na fonctionne", {
  df_test <- data.frame(Essence = "Chene Ind", C130 = NA, HDOM = 25, HTOT = 22)

  result <- calculer_volumes(df = df_test, type_volume = "VC22", remove_na = TRUE)

  expect_equal(nrow(result), 0)
})


test_that("Erreur si id_equation trop grand", {
  df_test <- data.frame(Essence = "Chene Ind", C130 = 30, HTOT = 22, HDOM = 25)

  expect_error(
    calculer_volumes(df = df_test, type_volume = "VC22", id_equation = 99),
    "id_equation = 99 depasse"
  )
})


test_that("Erreur si C130 et C150 sont absents des donnees", {
  df_test <- data.frame(Essence = "Chene Ind", HTOT = 25, HDOM = 27)
  expect_error(
    calculer_volumes(df = df_test, type_volume = "VC22"),
    "Le fichier doit contenir au moins une colonne 'C130' ou 'C150'"
  )
})
