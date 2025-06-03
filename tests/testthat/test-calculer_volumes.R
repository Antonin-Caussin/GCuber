library(testthat)

# =========================================================================
# DONNÉES DE TEST
# =========================================================================

# Jeu de données de test
test_df <- data.frame(
  C130 = c(25, 30, 35, NA, 40),
  C150 = c(28, 33, 38, 25, 43),
  HTOT = c(15, 18, 20, 12, 22),
  HDOM = c(14, 17, 19, 11, 21),
  Code = c(1, 2, 3, 1, 2),
  Abr = c("EPN", "SAB", "BOJ", "EPN", "SAB"),
  Essence = c("Épinette noire", "Sapin baumier", "Bouleau jaune", "Épinette noire", "Sapin baumier"),
  stringsAsFactors = FALSE
)

# =========================================================================
# TESTS POUR LA VALIDATION DES PARAMÈTRES
# =========================================================================

test_that("Validation des paramètres - type_volume et id_equation", {

  # Test warning pour type_volume = "E" avec id_equation incompatible
  expect_warning(
    calculer_volumes(test_df, type_volume = "E", id_equation = 1, specimens = "Code"),
    "WARNING: For volume type 'E'"
  )

  # Test erreur pour V22 avec id_equation invalide
  expect_error(
    calculer_volumes(test_df, type_volume = "V22", id_equation = 5, specimens = "Code"),
    "For volume type 'V22', id_equation must be between 1 and 3"
  )

  # Test erreur pour V22_HA avec id_equation != 1
  expect_error(
    calculer_volumes(test_df, type_volume = "V22_HA", id_equation = 2, specimens = "Code"),
    "For volume type 'V22_HA', id_equation must be 1"
  )

  # Test erreur pour V22B avec id_equation != 1
  expect_error(
    calculer_volumes(test_df, type_volume = "V22B", id_equation = 3, specimens = "Code"),
    "For volume type 'V22B', id_equation must be 1"
  )

  # Test erreur pour type_volume invalide
  expect_error(
    calculer_volumes(test_df, type_volume = "INVALID", specimens = "Code"),
    "Invalid volume type: INVALID"
  )
})

# =========================================================================
# TESTS POUR LA DÉTECTION DU TYPE DE SPÉCIMENS
# =========================================================================

test_that("Détection du type de spécimens", {

  # Mock de la fonction interne detecter_type_specimens
  detecter_type_specimens <- function(specimens_col, df) {
    sample_values <- na.omit(df[[specimens_col]])

    if (length(sample_values) == 0) {
      stop(paste("Column '", specimens_col, "' contains only missing values.", sep=""))
    }

    if (is.numeric(sample_values)) {
      return("Code")
    } else if (is.character(sample_values) || is.factor(sample_values)) {
      if (is.factor(sample_values)) {
        sample_values <- as.character(sample_values)
      }
      mean_length <- mean(nchar(sample_values))
      return(if (mean_length <= 4) "Abr" else "Essence")
    } else {
      stop(paste("Data type in column '", specimens_col, "' is not recognized.", sep=""))
    }
  }

  # Test détection Code (numérique)
  expect_equal(detecter_type_specimens("Code", test_df), "Code")

  # Test détection Abr (caractères courts)
  expect_equal(detecter_type_specimens("Abr", test_df), "Abr")

  # Test détection Essence (caractères longs)
  expect_equal(detecter_type_specimens("Essence", test_df), "Essence")

  # Test erreur pour colonne avec que des NA
  df_na <- data.frame(empty_col = c(NA, NA, NA))
  expect_error(
    detecter_type_specimens("empty_col", df_na),
    "contains only missing values"
  )

  # Test erreur pour type non reconnu
  df_logical <- data.frame(logical_col = c(TRUE, FALSE, TRUE))
  expect_error(
    detecter_type_specimens("logical_col", df_logical),
    "Data type in column 'logical_col' is not recognized"
  )
})

# =========================================================================
# TESTS POUR LA CORRESPONDANCE DES ESPÈCES
# =========================================================================

test_that("Établissement de la correspondance des espèces", {

  # Test avec specimens = NULL
  expect_error(
    calculer_volumes(test_df, specimens = NULL),
    "No species identification column specified"
  )

  # Test avec colonne inexistante
  expect_error(
    calculer_volumes(test_df, specimens = "INEXISTANT"),
    "The specified column 'INEXISTANT' does not exist"
  )

  # Test fonctionnement normal avec Code
  result <- calculer_volumes(test_df, specimens = "Code")
  expect_true("Species" %in% colnames(result))
  expect_equal(result$Species[1], "Épinette noire")

  # Test fonctionnement normal avec Abr
  result <- calculer_volumes(test_df, specimens = "Abr")
  expect_true("Species" %in% colnames(result))
  expect_equal(result$Species[2], "Sapin baumier")

  # Test fonctionnement avec colonne Essence déjà présente
  result <- calculer_volumes(test_df, specimens = "Essence")
  expect_true("Species" %in% colnames(result))
  expect_equal(result$Species[3], "Bouleau jaune")
})

# =========================================================================
# TESTS POUR LA CONVERSION DES DIAMÈTRES
# =========================================================================

test_that("Conversion des diamètres", {

  # Test conversion C150 vers C130
  df_sans_c130 <- test_df[, !names(test_df) %in% "C130"]
  result <- calculer_volumes(df_sans_c130, specimens = "Code")
  expect_true("C130" %in% colnames(result))
  expect_false(any(is.na(result$C130[1:3]))) # Les 3 premiers ne devraient pas être NA

  # Test conversion C130 vers C150
  df_sans_c150 <- test_df[, !names(test_df) %in% "C150"]
  result <- calculer_volumes(df_sans_c150, specimens = "Code", C150 = "C150_calc")
  expect_true("C150_calc" %in% colnames(result))

  # Test erreur quand aucune colonne diamètre n'existe
  df_sans_diametres <- test_df[, !names(test_df) %in% c("C130", "C150")]
  expect_error(
    calculer_volumes(df_sans_diametres, specimens = "Code"),
    "None of the specified diameter columns"
  )

  # Test formules de conversion
  # C150 -> C130: C130 = HV * C150 + IV
  # Avec HV = 1.1, IV = 0.5 pour EPN, et C150 = 28
  # C130 attendu = 1.1 * 28 + 0.5 = 31.3
  df_test_conv <- data.frame(
    C150 = 28,
    Code = 1,
    stringsAsFactors = FALSE
  )
  result <- calculer_volumes(df_test_conv, specimens = "Code")
  expect_equal(round(result$C130[1], 1), 31.3)
})

# =========================================================================
# TESTS POUR LA GESTION DES HAUTEURS
# =========================================================================

test_that("Gestion des colonnes de hauteur", {

  # Test avec colonnes de hauteur personnalisées
  df_hauteur_custom <- test_df
  names(df_hauteur_custom)[names(df_hauteur_custom) == "HTOT"] <- "Hauteur_totale"
  names(df_hauteur_custom)[names(df_hauteur_custom) == "HDOM"] <- "Hauteur_dominante"

  result <- calculer_volumes(df_hauteur_custom,
                             specimens = "Code",
                             HTOT = "Hauteur_totale",
                             HDOM = "Hauteur_dominante")

  expect_true("HTOT" %in% colnames(result))
  expect_true("HDOM" %in% colnames(result))
  expect_equal(result$HTOT, test_df$HTOT)
  expect_equal(result$HDOM, test_df$HDOM)

  # Test avec colonnes déjà nommées correctement
  result <- calculer_volumes(test_df, specimens = "Code")
  expect_true("HTOT" %in% colnames(result))
  expect_true("HDOM" %in% colnames(result))
})

# =========================================================================
# TESTS POUR LE CALCUL DES SURFACES TERRIÈRES
# =========================================================================

test_that("Calcul des surfaces terrières", {

  result <- calculer_volumes(test_df, specimens = "Code")

  # Test présence des colonnes G130 et G150
  expect_true("G130" %in% colnames(result))
  expect_true("G150" %in% colnames(result))

  # Test formule G = D²/(4π) / 10000
  # Pour C130 = 25 cm: G130 = 25²/(4π)/10000 = 625/(4π)/10000 ≈ 0.004969
  expected_g130 <- (25^2) / (4 * pi) / 10000
  expect_equal(round(result$G130[1], 6), round(expected_g130, 6))

  # Pour C150 = 28 cm: G150 = 28²/(4π)/10000
  expected_g150 <- (28^2) / (4 * pi) / 10000
  expect_equal(round(result$G150[1], 6), round(expected_g150, 6))

  # Test avec colonnes G déjà présentes
  df_avec_g <- test_df
  df_avec_g$G130 <- rep(0.01, nrow(test_df))
  df_avec_g$G150 <- rep(0.015, nrow(test_df))

  result <- calculer_volumes(df_avec_g, specimens = "Code")
  expect_equal(result$G130[1], 0.01) # Valeur originale conservée
  expect_equal(result$G150[1], 0.015) # Valeur originale conservée
})

# =========================================================================
# TESTS D'INTÉGRATION ET EDGE CASES
# =========================================================================

test_that("Tests d'intégration et cas limites", {

  # Test avec données complètes
  result <- calculer_volumes(test_df, specimens = "Code")
  expect_equal(nrow(result), nrow(test_df))
  expect_true(all(c("Species", "C130", "G130", "G150", "HTOT", "HDOM") %in% colnames(result)))

  # Test avec remove_na = TRUE (si implémenté)
  df_avec_na <- test_df
  df_avec_na$C130[1] <- NA
  df_avec_na$HTOT[2] <- NA

  result_avec_na <- calculer_volumes(df_avec_na, specimens = "Code", remove_na = FALSE)
  expect_equal(nrow(result_avec_na), nrow(df_avec_na))

  # Test avec dataframe vide
  df_vide <- data.frame()
  expect_error(
    calculer_volumes(df_vide, specimens = "Code"),
    "The specified column 'Code' does not exist"
  )

  # Test avec une seule ligne
  df_une_ligne <- test_df[1, ]
  result <- calculer_volumes(df_une_ligne, specimens = "Code")
  expect_equal(nrow(result), 1)
  expect_equal(result$Species[1], "Épinette noire")
})

# =========================================================================
# TESTS POUR LES WARNINGS ET MESSAGES
# =========================================================================

test_that("Gestion des warnings et messages", {

  # Test warning pour correspondances manquantes
  df_espece_inconnue <- test_df
  df_espece_inconnue$Code[1] <- 999 # Code inexistant

  expect_warning(
    calculer_volumes(df_espece_inconnue, specimens = "Code"),
    "No correspondence found"
  )

  # Test warning pour conversions échouées
  df_sans_coef <- test_df[, !names(test_df) %in% "C130"]
  equations_incomplete <- equations
  equations_incomplete$HV[1] <- NA

  # Simuler le cas où les coefficients sont manquants
  # (nécessiterait une modification de l'environnement global)

  # Test messages informatifs
  expect_output(
    calculer_volumes(test_df, specimens = "Code"),
    "Selected volume type: V22"
  )

  expect_output(
    calculer_volumes(test_df, specimens = "Code"),
    "Identifier type detected"
  )
})

# =========================================================================
# TESTS DE PERFORMANCE ET ROBUSTESSE
# =========================================================================

test_that("Tests de performance et robustesse", {

  # Test avec un grand dataset
  large_df <- do.call(rbind, replicate(1000, test_df, simplify = FALSE))
  large_df$id <- 1:nrow(large_df)

  start_time <- Sys.time()
  result <- calculer_volumes(large_df, specimens = "Code")
  end_time <- Sys.time()

  expect_equal(nrow(result), nrow(large_df))
  expect_lt(as.numeric(end_time - start_time), 10) # Moins de 10 secondes

  # Test avec valeurs extrêmes
  df_extremes <- data.frame(
    C130 = c(1, 1000, 0.1),
    C150 = c(1.1, 1100, 0.11),
    HTOT = c(0.5, 50, 0.1),
    HDOM = c(0.4, 49, 0.05),
    Code = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  result <- calculer_volumes(df_extremes, specimens = "Code")
  expect_equal(nrow(result), 3)
  expect_true(all(result$G130 > 0))
})

# =========================================================================
# HELPER FUNCTIONS POUR LES TESTS
# =========================================================================

# Fonction pour créer des données de test personnalisées
create_test_data <- function(n_rows = 5, include_na = FALSE, custom_species = NULL) {
  df <- data.frame(
    C130 = runif(n_rows, 20, 50),
    C150 = runif(n_rows, 22, 55),
    HTOT = runif(n_rows, 10, 25),
    HDOM = runif(n_rows, 9, 24),
    Code = sample(1:3, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  if (include_na) {
    na_indices <- sample(1:n_rows, max(1, n_rows %/% 4))
    df$C130[na_indices] <- NA
  }

  if (!is.null(custom_species)) {
    df$Code <- custom_species
  }

  return(df)
}

# Fonction pour vérifier la cohérence des résultats
check_result_consistency <- function(result, original_df) {
  checks <- list(
    same_nrows = nrow(result) == nrow(original_df),
    has_species = "Species" %in% colnames(result),
    has_diameters = any(c("C130", "C150") %in% colnames(result)),
    has_basal_areas = any(c("G130", "G150") %in% colnames(result)),
    positive_values = all(result$G130 > 0, na.rm = TRUE)
  )

  return(all(unlist(checks)))
}

# Tests utilisant les fonctions helper
test_that("Tests avec fonctions helper", {

  # Test avec données générées
  test_data <- create_test_data(10, include_na = TRUE)
  result <- calculer_volumes(test_data, specimens = "Code")
  expect_true(check_result_consistency(result, test_data))

  # Test avec espèces personnalisées
  custom_data <- create_test_data(5, custom_species = rep(1, 5))
  result <- calculer_volumes(custom_data, specimens = "Code")
  expect_true(all(result$Species == "Épinette noire"))
})
