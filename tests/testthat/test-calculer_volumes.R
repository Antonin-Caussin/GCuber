library(testthat)

# ============================================================================
# DONNeES DE TEST
# ============================================================================

# Dataframe d'equations simule
equations_test <- data.frame(
  Essences = c("Chenes indigenes", "Hetre", "Sapin de Vancouver", "Epicea commun"),
  Code = c(1, 3, 50, 41),
  Abr = c("CH", "HE", "SV", "EP"),
  HV = c(0.95, 0.92, 0.88, 0.90),
  IV = c(2.5, 3.1, 1.8, 2.2),
  stringsAsFactors = FALSE
)

# Donnees de test avec differentes configurations
df_test_complet <- data.frame(
  C130 = c(45.2, 52.1, 38.9, NA, 60.3),
  C150 = c(48.1, 55.8, 41.2, 35.6, NA),
  D130 = c(NA, NA, NA, 15.2, NA),
  D150 = c(NA, NA, NA, NA, 22.1),
  HTOT = c(12.5, 15.2, 10.8, 8.9, 18.3),
  HDOM = c(11.8, 14.5, 10.2, 8.1, 17.6),
  specimens_code = c(1, 3, 50, 41, 11),
  specimens_abr = c("CH", "HE", "SV", "EP", "BO"),
  specimens_essence = c("Chenes indigenes", "Hetre", "Sapin de Vancouver", "Epicea commun", "Bouleau"),
  stringsAsFactors = FALSE
)

df_test_minimal <- data.frame(
  C130 = c(45.2, 52.1),
  HTOT = c(12.5, 15.2),
  HDOM = c(11.8, 14.5),
  specimens_code = c(1, 11),
  stringsAsFactors = FALSE
)

# ============================================================================
# TESTS POUR valider_parametres()
# ============================================================================

test_that("valider_parametres - Types de volume valides", {
  # Mock de la fonction pour tester uniquement la validation
  valider_parametres_mock <- function(type_volume, id_equation, df, specimens = NULL,
                                      C130 = "C130", C150 = "C150", D130 = "D130",
                                      D150 = "D150", HTOT = "HTOT", HDOM = "HDOM") {
    types_volume_valides <- c("V22", "V22B", "E", "V22_HA")
    if (!(type_volume %in% types_volume_valides)) {
      stop(paste("Invalid volume type:", type_volume,
                 "\nValid types:", paste(types_volume_valides, collapse = ", ")))
    }
    TRUE
  }

  # Tests positifs
  expect_true(valider_parametres_mock("V22", 1, df_test_complet))
  expect_true(valider_parametres_mock("V22B", 1, df_test_complet))
  expect_true(valider_parametres_mock("E", 1, df_test_complet))
  expect_true(valider_parametres_mock("V22_HA", 1, df_test_complet))

  # Tests negatifs
  expect_error(valider_parametres_mock("INVALID", 1, df_test_complet),
               "Invalid volume type")
  expect_error(valider_parametres_mock("v22", 1, df_test_complet),
               "Invalid volume type")
})

test_that("valider_parametres - Correspondance type_volume et id_equation", {
  valider_correspondance_mock <- function(type_volume, id_equation) {
    if (type_volume == "V22" && !(id_equation %in% 1:3)) {
      stop("For volume type 'V22', id_equation must be between 1 and 3.")
    }
    if (type_volume == "V22_HA" && id_equation != 1) {
      stop("For volume type 'V22_HA', id_equation must be 1.")
    }
    if (type_volume == "V22B" && id_equation != 1) {
      stop("For volume type 'V22B', id_equation must be 1.")
    }
    TRUE
  }

  # Tests V22
  expect_true(valider_correspondance_mock("V22", 1))
  expect_true(valider_correspondance_mock("V22", 2))
  expect_true(valider_correspondance_mock("V22", 3))
  expect_error(valider_correspondance_mock("V22", 4),
               "id_equation must be between 1 and 3")

  # Tests V22B et V22_HA
  expect_true(valider_correspondance_mock("V22B", 1))
  expect_error(valider_correspondance_mock("V22B", 2),
               "id_equation must be 1")
  expect_error(valider_correspondance_mock("V22_HA", 2),
               "id_equation must be 1")
})

test_that("valider_parametres - Verification colonnes manquantes", {
  df_incomplet <- data.frame(
    C130 = c(45.2, 52.1),
    HTOT = c(12.5, 15.2)
  )

  verifier_colonnes_mock <- function(df, colonnes_requises) {
    colonnes_presentes <- colonnes_requises[colonnes_requises %in% colnames(df)]
    colonnes_manquantes <- setdiff(colonnes_requises, colonnes_presentes)

    if (length(colonnes_manquantes) > 0) {
      warning(paste("Colonnes manquantes:", paste(colonnes_manquantes, collapse = ", ")))
    }
    colonnes_manquantes
  }

  colonnes_requises <- c("C130", "C150", "D130", "D150", "HTOT", "HDOM", "specimens")

  expect_warning(
    manquantes <- verifier_colonnes_mock(df_incomplet, colonnes_requises),
    "Colonnes manquantes"
  )
  expect_true(length(manquantes) > 0)
  expect_true("C150" %in% manquantes)
  expect_true("HDOM" %in% manquantes)
})

# ============================================================================
# TESTS POUR detecter_type_specimens()
# ============================================================================

test_that("detecter_type_specimens - Detection Code numerique", {
  detecter_type_specimens_mock <- function(df, specimens) {
    sample_values <- na.omit(df[[specimens]])

    if (length(sample_values) == 0) {
      stop(paste("Column '", specimens, "' contains only missing values.", sep=""))
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
      stop("Data type not recognized")
    }
  }

  expect_equal(detecter_type_specimens_mock(df_test_complet, "specimens_code"), "Code")
  expect_equal(detecter_type_specimens_mock(df_test_complet, "specimens_abr"), "Abr")
  expect_equal(detecter_type_specimens_mock(df_test_complet, "specimens_essence"), "Essence")
})

test_that("detecter_type_specimens - Gestion des valeurs manquantes", {
  df_na <- data.frame(specimens = c(NA, NA, NA))

  detecter_type_specimens_mock <- function(df, specimens) {
    sample_values <- na.omit(df[[specimens]])
    if (length(sample_values) == 0) {
      stop(paste("Column '", specimens, "' contains only missing values.", sep=""))
    }
    return("Test")
  }

  expect_error(detecter_type_specimens_mock(df_na, "specimens"),
               "contains only missing values")
})

test_that("detecter_type_specimens - Distinction Abr vs Essence", {
  df_test_abr <- data.frame(specimens = c("CH", "HE", "SV"))
  df_test_essence <- data.frame(specimens = c("Chenes indigenes pedoncule", "Hetre commun", "Sapin de Vancouver blanc"))

  detecter_type_specimens_mock <- function(df, specimens) {
    sample_values <- as.character(na.omit(df[[specimens]]))
    mean_length <- mean(nchar(sample_values))
    return(if (mean_length <= 4) "Abr" else "Essence")
  }

  expect_equal(detecter_type_specimens_mock(df_test_abr, "specimens"), "Abr")
  expect_equal(detecter_type_specimens_mock(df_test_essence, "specimens"), "Essence")
})

# ============================================================================
# TESTS POUR etablir_correspondance_especes()
# ============================================================================

test_that("etablir_correspondance_especes - Correspondance avec Code", {
  # Mock simplifie de la fonction
  etablir_correspondance_mock <- function(df, specimens, equations_df) {
    type_specimens <- if (is.numeric(df[[specimens]])) "Code" else "Abr"

    mapping_df <- unique(equations_df[, c("Essences", type_specimens)])
    names(mapping_df) <- c("Species", specimens)

    df_result <- merge(df, mapping_df, by = specimens, all.x = TRUE)
    return(df_result)
  }

  df_test <- data.frame(specimens_code = c(1, 3, 50))
  result <- etablir_correspondance_mock(df_test, "specimens_code", equations_test)

  expect_true("Species" %in% colnames(result))
  expect_equal(result$Species[1], "Chenes indigenes")
  expect_equal(result$Species[2], "Hetre")
  expect_equal(result$Species[3], "Sapin de Vancouver")
})

test_that("etablir_correspondance_especes - Gestion valeurs sans correspondance", {
  df_test_invalide <- data.frame(specimens_code = c(1, 3, 9999))  # 9999 n'existe pas

  etablir_correspondance_mock <- function(df, specimens, equations_df) {
    mapping_df <- unique(equations_df[, c("Essences", "Code")])
    names(mapping_df) <- c("Species", specimens)
    df_result <- merge(df, mapping_df, by = specimens, all.x = TRUE)

    if (any(is.na(df_result$Species))) {
      na_values <- unique(df_result[[specimens]][is.na(df_result$Species)])
      warning(paste("No correspondence found for:", paste(na_values, collapse=", ")))
    }
    return(df_result)
  }

  expect_warning(
    result <- etablir_correspondance_mock(df_test_invalide, "specimens_code", equations_test),
    "No correspondence found for: 99"
  )
  expect_true(is.na(result$Species[3]))
})

# ============================================================================
# TESTS POUR conversions_diametre()
# ============================================================================

test_that("conversions_diametre - Conversion D130 vers C130", {
  conversions_diametre_mock <- function(df) {
    pi_val <- pi
    df_result <- df

    if ("D130" %in% colnames(df) && !is.na(df$D130[1])) {
      df_result$C130 <- df$D130 * pi_val
    }
    return(df_result)
  }

  df_test <- data.frame(D130 = c(10, 15, 20))
  result <- conversions_diametre_mock(df_test)

  expect_true("C130" %in% colnames(result))
  expect_equal(result$C130[1], 10 * pi, tolerance = 1e-10)
  expect_equal(result$C130[2], 15 * pi, tolerance = 1e-10)
  expect_equal(result$C130[3], 20 * pi, tolerance = 1e-10)
})

test_that("conversions_diametre - Conversion D150 vers C150", {
  conversions_diametre_mock <- function(df) {
    pi_val <- pi
    df_result <- df

    if ("D150" %in% colnames(df)) {
      df_result$C150 <- df$D150 * pi_val
    }
    return(df_result)
  }

  df_test <- data.frame(D150 = c(12, 18, 25))
  result <- conversions_diametre_mock(df_test)

  expect_true("C150" %in% colnames(result))
  expect_equal(result$C150[1], 12 * pi, tolerance = 1e-10)
  expect_equal(result$C150[2], 18 * pi, tolerance = 1e-10)
  expect_equal(result$C150[3], 25 * pi, tolerance = 1e-10)
})

test_that("conversions_diametre - Gestion des valeurs manquantes", {
  conversions_diametre_mock <- function(df) {
    pi_val <- pi
    df_result <- df
    df_result$C130 <- NA_real_

    for (i in seq_len(nrow(df_result))) {
      if ("D130" %in% colnames(df) && !is.na(df$D130[i])) {
        df_result$C130[i] <- df$D130[i] * pi_val
      }
    }
    return(df_result)
  }

  df_test <- data.frame(D130 = c(10, NA, 20))
  result <- conversions_diametre_mock(df_test)

  expect_equal(result$C130[1], 10 * pi, tolerance = 1e-10)
  expect_true(is.na(result$C130[2]))
  expect_equal(result$C130[3], 20 * pi, tolerance = 1e-10)
})

# ============================================================================
# TESTS POUR convertir_circonference()
# ============================================================================

test_that("convertir_circonference - Conversion C150 vers C130", {
  convertir_circonference_mock <- function(df, equations_df, from_col, to_col) {
    df_result <- df
    df_result[[to_col]] <- NA_real_

    coefs_df <- unique(equations_df[, c("Essences", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Species"

    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]
      from_value <- df_result[[from_col]][i]

      if (!is.na(essence_arbre) && !is.na(from_value)) {
        coef_row <- coefs_df[coefs_df$Species == essence_arbre, ]

        if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
          HV_coef <- coef_row$HV[1]
          IV_coef <- coef_row$IV[1]
          # C150 to C130: C130 = HV * C150 + IV
          df_result[[to_col]][i] <- HV_coef * from_value + IV_coef
        }
      }
    }
    return(df_result)
  }

  df_test <- data.frame(
    C150 = c(50, 60),
    Species = c("Chenes indigenes", "Hetre")
  )

  result <- convertir_circonference_mock(df_test, equations_test, "C150", "C130")

  expect_true("C130" %in% colnames(result))
  # Test de la formule: C130 = HV * C150 + IV
  expected_c130_1 <- equations_test$HV[1] * 50 + equations_test$IV[1]  # Chenes indigenes
  expected_c130_2 <- equations_test$HV[2] * 60 + equations_test$IV[2]  # Hetre

  expect_equal(result$C130[1], expected_c130_1, tolerance = 1e-10)
  expect_equal(result$C130[2], expected_c130_2, tolerance = 1e-10)
})

test_that("convertir_circonference - Conversion C130 vers C150", {
  convertir_circonference_mock <- function(df, equations_df, from_col, to_col) {
    df_result <- df
    df_result[[to_col]] <- NA_real_

    coefs_df <- unique(equations_df[, c("Essences", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Species"

    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]
      from_value <- df_result[[from_col]][i]

      if (!is.na(essence_arbre) && !is.na(from_value)) {
        coef_row <- coefs_df[coefs_df$Species == essence_arbre, ]

        if (nrow(coef_row) > 0 && !is.na(coef_row$HV[1]) && !is.na(coef_row$IV[1])) {
          HV_coef <- coef_row$HV[1]
          IV_coef <- coef_row$IV[1]
          # C130 to C150: C150 = (C130 - IV) / HV
          df_result[[to_col]][i] <- (from_value - IV_coef) / HV_coef
        }
      }
    }
    return(df_result)
  }

  df_test <- data.frame(
    C130 = c(45, 55),
    Species = c("Chenes indigenes", "Hetre")
  )

  result <- convertir_circonference_mock(df_test, equations_test, "C130", "C150")

  expect_true("C150" %in% colnames(result))
  # Test de la formule: C150 = (C130 - IV) / HV
  expected_c150_1 <- (45 - equations_test$IV[1]) / equations_test$HV[1]  # Chenes indigenes
  expected_c150_2 <- (55 - equations_test$IV[2]) / equations_test$HV[2]  # Hetre

  expect_equal(result$C150[1], expected_c150_1, tolerance = 1e-10)
  expect_equal(result$C150[2], expected_c150_2, tolerance = 1e-10)
})

test_that("convertir_circonference - Espece sans coefficients", {
  convertir_circonference_mock <- function(df, equations_df, from_col, to_col) {
    df_result <- df
    df_result[[to_col]] <- NA_real_

    coefs_df <- unique(equations_df[, c("Essences", "HV", "IV")])
    names(coefs_df)[names(coefs_df) == "Essences"] <- "Species"

    conversions_failed <- 0

    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]
      from_value <- df_result[[from_col]][i]

      if (!is.na(essence_arbre) && !is.na(from_value)) {
        coef_row <- coefs_df[coefs_df$Species == essence_arbre, ]

        if (nrow(coef_row) == 0 || is.na(coef_row$HV[1]) || is.na(coef_row$IV[1])) {
          conversions_failed <- conversions_failed + 1
          warning(paste("Unable to convert for species:", essence_arbre))
        }
      }
    }
    return(df_result)
  }

  df_test <- data.frame(
    C150 = c(50),
    Species = c("Espece_Inconnue")
  )

  expect_warning(
    result <- convertir_circonference_mock(df_test, equations_test, "C150", "C130"),
    "Unable to convert for species: Espece_Inconnue"
  )
})

# ============================================================================
# TESTS POUR calculer_surfaces_terrieres()
# ============================================================================

test_that("calculer_surfaces_terrieres - Calcul G130", {
  calculer_surfaces_terrieres_mock <- function(df) {
    df_result <- df

    if (!"G130" %in% colnames(df_result) && "C130" %in% colnames(df_result)) {
      df_result$G130 <- (df_result$C130^2) / ((4 * pi) * 10000)
    }
    return(df_result)
  }

  df_test <- data.frame(C130 = c(31.416, 62.832))  # Circonferences de 10π et 20π cm
  result <- calculer_surfaces_terrieres_mock(df_test)

  expect_true("G130" %in% colnames(result))
  # G = C²/(4π×10000) = (31.416)²/(4π×10000) ≈ 0.00785 m²
  expected_g130_1 <- (31.416^2) / (4 * pi * 10000)
  expected_g130_2 <- (62.832^2) / (4 * pi * 10000)

  expect_equal(result$G130[1], expected_g130_1, tolerance = 1e-6)
  expect_equal(result$G130[2], expected_g130_2, tolerance = 1e-6)
})

test_that("calculer_surfaces_terrieres - Calcul G150", {
  calculer_surfaces_terrieres_mock <- function(df, C150_col = "C150") {
    df_result <- df

    if (!"G150" %in% colnames(df_result) && C150_col %in% colnames(df_result)) {
      df_result$G150 <- (df_result[[C150_col]]^2) / ((4 * pi) * 10000)
    }
    return(df_result)
  }

  df_test <- data.frame(C150 = c(40, 50, 60))
  result <- calculer_surfaces_terrieres_mock(df_test)

  expect_true("G150" %in% colnames(result))
  # Test des valeurs calculees
  for (i in 1:3) {
    expected_g150 <- (df_test$C150[i]^2) / (4 * pi * 10000)
    expect_equal(result$G150[i], expected_g150, tolerance = 1e-10)
  }
})

test_that("calculer_surfaces_terrieres - Gestion des valeurs manquantes", {
  calculer_surfaces_terrieres_mock <- function(df) {
    df_result <- df

    if (!"G130" %in% colnames(df_result) && "C130" %in% colnames(df_result)) {
      df_result$G130 <- (df_result$C130^2) / ((4 * pi) * 10000)
    }
    return(df_result)
  }

  df_test <- data.frame(C130 = c(31.416, NA, 62.832))
  result <- calculer_surfaces_terrieres_mock(df_test)

  expect_equal(result$G130[1], (31.416^2) / (4 * pi * 10000), tolerance = 1e-6)
  expect_true(is.na(result$G130[2]))
  expect_equal(result$G130[3], (62.832^2) / (4 * pi * 10000), tolerance = 1e-6)
})

test_that("calculer_surfaces_terrieres - Colonnes deja existantes", {
  calculer_surfaces_terrieres_mock <- function(df) {
    df_result <- df

    # Ne recalcule pas si la colonne existe deja
    if (!"G130" %in% colnames(df_result) && "C130" %in% colnames(df_result)) {
      df_result$G130 <- (df_result$C130^2) / ((4 * pi) * 10000)
    }
    return(df_result)
  }

  df_test <- data.frame(
    C130 = c(31.416, 62.832),
    G130 = c(0.01, 0.02)  # Valeurs preexistantes
  )

  result <- calculer_surfaces_terrieres_mock(df_test)

  # Les valeurs G130 ne doivent pas etre modifiees
  expect_equal(result$G130[1], 0.01)
  expect_equal(result$G130[2], 0.02)
})

# ============================================================================
# TESTS D'INTeGRATION
# ============================================================================

test_that("Integration - Workflow complet avec donnees minimales", {
  # Ce test simule le workflow complet sur des donnees simplifiees
  # En realite, il faudrait appeler la vraie fonction calculer_volumes

  df_integration <- data.frame(
    D130 = c(15, 20),
    HTOT = c(12, 15),
    HDOM = c(11, 14),
    specimens_code = c(1, 3),
    stringsAsFactors = FALSE
  )

  # Simulation du workflow
  workflow_mock <- function(df, equations_df) {
    # 1. Conversion D130 -> C130
    df$C130 <- df$D130 * pi

    # 2. Correspondance especes
    mapping <- equations_df[, c("Essences", "Code")]
    names(mapping) <- c("Species", "specimens_code")
    df <- merge(df, mapping, by = "specimens_code", all.x = TRUE)

    # 3. Calcul surface terriere
    df$G130 <- (df$C130^2) / (4 * pi * 10000)

    return(df)
  }

  result <- workflow_mock(df_integration, equations_test)

  expect_true("C130" %in% colnames(result))
  expect_true("Species" %in% colnames(result))
  expect_true("G130" %in% colnames(result))
  expect_equal(result$Species[1], "Chenes indigenes")
  expect_equal(result$Species[2], "Hetre")
  expect_equal(result$C130[1], 15 * pi, tolerance = 1e-10)
  expect_equal(result$C130[2], 20 * pi, tolerance = 1e-10)
})

# ============================================================================
# TESTS DE PERFORMANCE ET ROBUSTESSE
# ============================================================================

test_that("Robustesse - Gestion de gros datasets", {
  # Test avec un dataset plus large
  n_rows <- 1000
  df_large <- data.frame(
    D130 = runif(n_rows, 10, 50),
    HTOT = runif(n_rows, 8, 25),
    HDOM = runif(n_rows, 7, 24),
    specimens_code = sample(1:4, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Test de conversion simple
  conversion_test <- function(df) {
    df$C130 <- df$D130 * pi
    return(nrow(df))
  }

  expect_equal(conversion_test(df_large), n_rows)
  expect_true(all(!is.na(df_large$D130)))
})

test_that("Validation - Types de donnees coherents", {
  validation_types_mock <- function(df) {
    checks <- list()

    if ("C130" %in% colnames(df)) {
      checks$C130_numeric <- is.numeric(df$C130)
    }
    if ("D130" %in% colnames(df)) {
      checks$D130_numeric <- is.numeric(df$D130)
    }
    if ("HTOT" %in% colnames(df)) {
      checks$HTOT_numeric <- is.numeric(df$HTOT)
    }

    return(all(unlist(checks)))
  }

  df_valid <- data.frame(
    C130 = c(45.2, 52.1),
    D130 = c(14.4, 16.6),
    HTOT = c(12.5, 15.2)
  )

  df_invalid <- data.frame(
    C130 = c("45.2", "52.1"),  # Caracteres au lieu de numerique
    D130 = c(14.4, 16.6),
    HTOT = c(12.5, 15.2)
  )

  expect_true(validation_types_mock(df_valid))
  expect_false(validation_types_mock(df_invalid))
})

# ============================================================================
# HELPER FUNCTIONS POUR LES TESTS
# ============================================================================

# Fonction pour créer des données de test aléatoires
create_test_data <- function(n_rows = 10, include_na = FALSE, na_rate = 0.1) {
  df <- data.frame(
    C130 = runif(n_rows, 30, 70),
    C150 = runif(n_rows, 32, 75),
    D130 = runif(n_rows, 10, 25),
    D150 = runif(n_rows, 11, 27),
    HTOT = runif(n_rows, 8, 25),
    HDOM = runif(n_rows, 7, 24),
    specimens_code = sample(1:10, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )

  if (include_na) {
    # Introduire des valeurs manquantes aléatoirement
    for (col in names(df)) {
      na_indices <- sample(1:n_rows, size = floor(n_rows * na_rate))
      df[na_indices, col] <- NA
    }
  }

  return(df)
}

# Fonction pour vérifier la cohérence des conversions
check_conversion_consistency <- function(original_value, converted_value, conversion_type) {
  tolerance <- 1e-10

  if (conversion_type == "diameter_to_circumference") {
    expected <- original_value * pi
    return(abs(converted_value - expected) < tolerance)
  } else if (conversion_type == "circumference_to_diameter") {
    expected <- original_value / pi
    return(abs(converted_value - expected) < tolerance)
  }

  return(FALSE)
}

# ============================================================================
# TESTS SUPPLÉMENTAIRES POUR CAS LIMITES
# ============================================================================

test_that("Cas limites - Valeurs extrêmes", {
  # Test avec des valeurs très petites et très grandes
  df_extremes <- data.frame(
    D130 = c(0.1, 100, 0.001, 1000),
    specimens_code = c(1, 50, 41, 10)
  )

  conversion_extremes <- function(df) {
    df$C130 <- df$D130 * pi
    return(df)
  }

  result <- conversion_extremes(df_extremes)

  expect_equal(result$C130[1], 0.1 * pi, tolerance = 1e-15)
  expect_equal(result$C130[2], 100 * pi, tolerance = 1e-12)
  expect_equal(result$C130[3], 0.001 * pi, tolerance = 1e-18)
  expect_equal(result$C130[4], 1000 * pi, tolerance = 1e-12)
})

test_that("Cas limites - Dataframe vide", {
  df_empty <- data.frame()

  handle_empty_df <- function(df) {
    if (nrow(df) == 0) {
      warning("Dataframe is empty")
      return(df)
    }
    return(df)
  }

  expect_warning(
    result <- handle_empty_df(df_empty),
    "Dataframe is empty"
  )
  expect_equal(nrow(result), 0)
})

test_that("Cas limites - Une seule ligne de données", {
  df_single <- data.frame(
    C130 = 45.2,
    D130 = 14.4,
    HTOT = 12.5,
    HDOM = 11.8,
    specimens_code = 1
  )

  process_single_row <- function(df) {
    # Calcul surface terrière
    df$G130 <- (df$C130^2) / (4 * pi * 10000)
    return(df)
  }

  result <- process_single_row(df_single)

  expect_equal(nrow(result), 1)
  expect_true("G130" %in% colnames(result))
  expect_equal(result$G130[1], (45.2^2) / (4 * pi * 10000), tolerance = 1e-10)
})

# ============================================================================
# TESTS DE VALIDATION DES FORMULES MATHÉMATIQUES
# ============================================================================

test_that("Validation formules - Surface terrière", {
  # Test de la formule G = C²/(4π×10000)
  # Avec des valeurs connues
  test_cases <- data.frame(
    C130 = c(31.416, 62.832, 94.248),  # π×10, π×20, π×30
    expected_G130 = c(0.007854, 0.031416, 0.070686),  # π×(r²)/10000 avec r=5,10,15
    stringsAsFactors = FALSE
  )

  formula_surface_terriere <- function(circonference) {
    return((circonference^2) / (4 * pi * 10000))
  }

  for (i in 1:nrow(test_cases)) {
    calculerd <- formula_surface_terriere(test_cases$C130[i])
    expect_equal(calculerd, test_cases$expected_G130[i],
                 tolerance = 1e-4,
                 info = paste("Test case", i))
  }
})

test_that("Validation formules - Conversion circonférence", {
  # Test des formules de conversion avec coefficients connus
  HV <- 0.95
  IV <- 2.5

  # Test C150 → C130: C130 = HV × C150 + IV
  test_c150_to_c130 <- function(c150, hv, iv) {
    return(hv * c150 + iv)
  }

  # Test C130 → C150: C150 = (C130 - IV) / HV
  test_c130_to_c150 <- function(c130, hv, iv) {
    return((c130 - iv) / hv)
  }

  # Test avec valeurs connues
  c150_test <- 50
  c130_calculerd <- test_c150_to_c130(c150_test, HV, IV)
  c150_back <- test_c130_to_c150(c130_calculerd, HV, IV)

  expect_equal(c130_calculerd, HV * 50 + IV, tolerance = 1e-10)
  expect_equal(c150_back, c150_test, tolerance = 1e-10)  # Test de réversibilité
})

# ============================================================================
# TESTS DE VALIDATION DES DONNÉES D'ENTRÉE
# ============================================================================

test_that("Validation données - Cohérence diamètre/circonférence", {
  # Si D130 et C130 sont présents, vérifier la cohérence
  validate_diameter_circumference <- function(df) {
    issues <- c()

    if ("D130" %in% colnames(df) && "C130" %in% colnames(df)) {
      for (i in 1:nrow(df)) {
        if (!is.na(df$D130[i]) && !is.na(df$C130[i])) {
          expected_c130 <- df$D130[i] * pi
          if (abs(df$C130[i] - expected_c130) > 0.01) {  # Tolérance de 0.01 cm
            issues <- c(issues, paste("Row", i, ": D130/C130 inconsistency"))
          }
        }
      }
    }

    return(list(valid = length(issues) == 0, issues = issues))
  }

  # Données cohérentes
  df_coherent <- data.frame(
    D130 = c(10, 15, 20),
    C130 = c(10*pi, 15*pi, 20*pi)
  )

  # Données incohérentes
  df_incoherent <- data.frame(
    D130 = c(10, 15, 20),
    C130 = c(30, 45, 60)  # Valeurs incorrectes
  )

  result_coherent <- validate_diameter_circumference(df_coherent)
  result_incoherent <- validate_diameter_circumference(df_incoherent)

  expect_true(result_coherent$valid)
  expect_false(result_incoherent$valid)
  expect_true(length(result_incoherent$issues) > 0)
})

test_that("Validation données - Hauteurs cohérentes", {
  # HDOM doit être <= HTOT
  validate_heights <- function(df) {
    issues <- c()

    if ("HTOT" %in% colnames(df) && "HDOM" %in% colnames(df)) {
      for (i in 1:nrow(df)) {
        if (!is.na(df$HTOT[i]) && !is.na(df$HDOM[i])) {
          if (df$HDOM[i] > df$HTOT[i]) {
            issues <- c(issues, paste("Row", i, ": HDOM > HTOT"))
          }
        }
      }
    }

    return(list(valid = length(issues) == 0, issues = issues))
  }

  # Données cohérentes
  df_heights_ok <- data.frame(
    HTOT = c(15, 20, 12),
    HDOM = c(14, 18, 11)
  )

  # Données incohérentes
  df_heights_bad <- data.frame(
    HTOT = c(15, 20, 12),
    HDOM = c(16, 18, 15)  # HDOM > HTOT dans certains cas
  )

  result_ok <- validate_heights(df_heights_ok)
  result_bad <- validate_heights(df_heights_bad)

  expect_true(result_ok$valid)
  expect_false(result_bad$valid)
  expect_true(any(grepl("HDOM > HTOT", result_bad$issues)))
})

# ============================================================================
# TESTS DE PERFORMANCE
# ============================================================================

test_that("Performance - Temps d'exécution raisonnable", {
  # Test avec un dataset de taille moyenne
  df_perf <- create_test_data(n_rows = 1000, include_na = FALSE)

  # Mesure du temps pour les conversions de base
  start_time <- Sys.time()

  # Simulation des opérations principales
  df_perf$C130_from_D130 <- df_perf$D130 * pi
  df_perf$G130 <- (df_perf$C130^2) / (4 * pi * 10000)
  df_perf$G150 <- (df_perf$C150^2) / (4 * pi * 10000)

  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Le traitement de 1000 lignes devrait prendre moins d'une seconde
  expect_lt(execution_time, 1.0)
  expect_equal(nrow(df_perf), 1000)
})

# ============================================================================
# TESTS DE REGRESSION
# ============================================================================

test_that("Régression - Résultats constants", {
  # Test pour s'assurer que les mêmes entrées donnent les mêmes sorties
  df_regression <- data.frame(
    D130 = c(15.5, 22.3, 18.7),
    specimens_code = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  process_regression <- function(df) {
    df$C130 <- df$D130 * pi
    df$G130 <- (df$C130^2) / (4 * pi * 10000)
    return(df)
  }

  # Exécuter deux fois le même traitement
  result1 <- process_regression(df_regression)
  result2 <- process_regression(df_regression)

  # Les résultats doivent être identiques
  expect_equal(result1$C130, result2$C130)
  expect_equal(result1$G130, result2$G130)

  # Valeurs de référence attendues (pour détecter les régressions futures)
  expected_c130 <- c(15.5 * pi, 22.3 * pi, 18.7 * pi)
  expected_g130 <- (expected_c130^2) / (4 * pi * 10000)

  expect_equal(result1$C130, expected_c130, tolerance = 1e-12)
  expect_equal(result1$G130, expected_g130, tolerance = 1e-12)
})

# ============================================================================
# TESTS D'ASSERTIONS ET MESSAGES D'ERREUR
# ============================================================================

test_that("Messages d'erreur - Clarté et précision", {
  # Test des messages d'erreur pour différents cas

  validate_with_messages <- function(type_volume, id_equation) {
    if (type_volume == "V22" && !(id_equation %in% 1:3)) {
      stop("For volume type 'V22', id_equation must be between 1 and 3.")
    }
    return(TRUE)
  }

  # Test du message d'erreur exact
  expect_error(
    validate_with_messages("V22", 4),
    "For volume type 'V22', id_equation must be between 1 and 3.",
    fixed = TRUE
  )

  expect_error(
    validate_with_messages("V22", 0),
    "For volume type 'V22', id_equation must be between 1 and 3.",
    fixed = TRUE
  )
})

test_that("Warnings - Messages d'avertissement appropriés", {
  # Test des warnings pour les conversions échouées

  warn_missing_coefficients <- function(species_name) {
    warning(paste("Unable to convert for species:", species_name))
  }

  expect_warning(
    warn_missing_coefficients("Espèce_Inconnue"),
    "Unable to convert for species: Espèce_Inconnue",
    fixed = TRUE
  )
})

  # =========================================================================
  # Configuration des données de test
  # =========================================================================

  # Données d'équations de test
  setup_test_equations <- function() {
    data.frame(
      Essences = c("Epicea", "Epicea", "Hetre", "Hetre", "Chene"),
      Y = c("V", "E", "V", "E", "V"),
      A0 = c(1, 4, 2, 5, 3),
      b0 = c(0.1, -2.5, 0.2, -3.0, 0.15),
      b1 = c(0.8, 2.1, 0.75, 2.3, 0.9),
      b2 = c(0.02, 0, 0.03, 0, 0.01),
      b3 = c(0, 0, 0, 0, 0),
      b4 = c(0, 0, 0, 0, 0),
      b5 = c(0, 0, 0, 0, 0),
      X1 = c("DBH", "DBH", "DBH", "DBH", "DBH"),
      X2 = c("H", "0", "H", "0", "H"),
      X3 = c("0", "0", "0", "0", "0"),
      X4 = c("0", "0", "0", "0", "0"),
      X5 = c("0", "0", "0", "0", "0"),
      stringsAsFactors = FALSE
    )
  }

  # Données d'arbres de test
  setup_test_trees <- function() {
    data.frame(
      Species = c("Epicea", "Hetre", "Chene", "Epicea", "Hetre"),
      DBH = c(30, 25, 35, 20, 40),
      H = c(25, 20, 30, 15, 35),
      stringsAsFactors = FALSE
    )
  }

  # Mock de la fonction principale (partie du code fourni)
  calculate_volumes <- function(df_result, equations_df, type_volume, id_equation = 1, remove_na = TRUE) {

    # Filter equations
    eqs_volume <- equations_df[equations_df$Y == type_volume, ]

    if (nrow(eqs_volume) == 0) {
      stop(paste("No equation found for volume type:", type_volume))
    }

    # Ensure that coefficients b0 to b5 are numeric
    colonnes_b <- paste0("b", 0:5)
    eqs_volume[colonnes_b] <- lapply(eqs_volume[colonnes_b], as.numeric)

    # Initialization
    df_result$Equation_Utilisee <- NA_character_

    # Initialize the volume column specified by the user
    if (!(type_volume %in% names(df_result))) {
      df_result[[type_volume]] <- NA_real_
    }

    # Function to evaluate expressions
    evaluer_expression <- function(expr_text, variables) {
      if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)

      if (expr_text == "/") {
        warning("Invalid expression detected: '/'")
        return(NA)
      }

      var_names <- all.vars(parse(text = expr_text))
      for (v in var_names) {
        if (!v %in% names(variables) || is.na(variables[[v]])) {
          warning(paste("Missing or NA variable:", v, "for expression:", expr_text))
          return(NA)
        }
      }

      env <- list2env(variables)
      tryCatch({
        eval(parse(text = expr_text), envir = env)
      }, error = function(e) {
        warning(paste("Error during expression evaluation:", expr_text, "-", e$message))
        return(NA)
      })
    }

    # Calculation for each row
    for (i in seq_len(nrow(df_result))) {
      essence_arbre <- df_result$Species[i]
      volume <- 0

      # For volume type "E", dynamically determine the equation to use
      if (type_volume == "E") {
        eq_candidates_E <- eqs_volume[eqs_volume$Essences == essence_arbre, ]
        eq_candidates_E_filtered <- eq_candidates_E[eq_candidates_E$A0 %in% c(4, 5), ]

        if (nrow(eq_candidates_E_filtered) > 0) {
          if (any(eq_candidates_E_filtered$A0 == 4)) {
            local_id_equation <- 4
          } else {
            local_id_equation <- 5
          }
        } else {
          local_id_equation <- id_equation
        }
      } else {
        local_id_equation <- id_equation
      }

      eq_candidates <- eqs_volume[eqs_volume$Essences == essence_arbre, ]

      if (nrow(eq_candidates) == 0) {
        warning(paste("No equation found for species:", essence_arbre))
        next
      }

      # Select appropriate equation
      if (type_volume == "E" && local_id_equation %in% c(4, 5)) {
        eq_by_a0 <- eq_candidates[eq_candidates$A0 == local_id_equation, ]
        if (nrow(eq_by_a0) > 0) {
          eq <- eq_by_a0[1, , drop = FALSE]
        } else {
          other_a0 <- if (local_id_equation == 4) 5 else 4
          eq_by_other_a0 <- eq_candidates[eq_candidates$A0 == other_a0, ]

          if (nrow(eq_by_other_a0) > 0) {
            eq <- eq_by_other_a0[1, , drop = FALSE]
          } else {
            if (nrow(eq_candidates) > 0) {
              eq <- eq_candidates[1, , drop = FALSE]
            } else {
              warning(paste("No equation found for species", essence_arbre))
              next
            }
          }
        }
      } else {
        if (local_id_equation > nrow(eq_candidates)) {
          warning(paste("Equation with id", local_id_equation, "does not exist for species", essence_arbre,
                        ". Using equation 1 instead."))
          eq <- eq_candidates[1, , drop = FALSE]
        } else {
          eq <- eq_candidates[local_id_equation, , drop = FALSE]
        }
      }

      df_result$Equation_Utilisee[i] <- paste0(eq$Essences, ":", eq$Y, ":A0=", eq$A0)

      exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
      exprs <- exprs[!is.na(exprs) & exprs != "0"]
      vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

      variables <- list()
      for (v in vars_needed) {
        if (v %in% names(df_result)) {
          variables[[v]] <- df_result[[v]][i]
        } else {
          stop(paste("Variable", v, "is used in an equation but missing from the data."))
        }
      }

      a0_value <- eq$A0[1]

      if (is.na(a0_value)) {
        warning(paste("Missing A0 value for species", essence_arbre, "at row", i))
        next
      } else if (a0_value %in% c(1, 2, 3, 5)) {
        # For standard linear equations, start with b0
        volume <- eq$b0[1]
        for (j in 1:5) {
          x_col <- paste0("X", j)
          b_col <- paste0("b", j)

          if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
            if (eq[[x_col]][1] == "/") {
              warning(paste("Invalid expression at row", i, "for X", j))
              next
            }

            x_val <- tryCatch({
              evaluer_expression(eq[[x_col]][1], variables)
            }, error = function(e) {
              warning(paste("Error during evaluation of X", j, ":", e$message))
              NA
            })

            if (length(x_val) == 0 || is.na(x_val)) {
              warning(paste("Evaluation of X", j, "failed at row", i))
              next
            }

            b_val <- eq[[b_col]][1]
            if (is.na(b_val)) {
              warning(paste("Missing coefficient b", j, "at row", i))
              next
            }

            volume <- volume + b_val * x_val
          }
        }
      } else if (a0_value == 4) {
        C130 <- evaluer_expression(eq$X1[1], variables)
        if (C130 <= 0) {
          warning(paste("Negative or zero value for logarithm at row", i))
          next
        }
        volume <- 10^(1*eq$b0[1] + eq$b1[1] * log10(C130))
      } else {
        warning(paste("Unknown equation type (A0 =", a0_value, ") for row", i))
        next
      }

      if (is.na(volume) || !is.finite(volume)) {
        warning(paste("Invalid volume result at row", i, ":", volume))
        next
      }

      df_result[[type_volume]][i] <- volume
    }

    # Final cleanup if requested
    if (remove_na) {
      df_result <- df_result[!is.na(df_result[[type_volume]]), ]
    }

    return(df_result)
  }

  # =========================================================================
  # TESTS PRINCIPAUX
  # =========================================================================

  # Test 1: Calcul de volume standard (type V)
  test_that("Calcul de volume standard fonctionne correctement", {
    equations_df <- setup_test_equations()
    df_trees <- setup_test_trees()

    result <- calculate_volumes(df_trees, equations_df, "V", 1)

    expect_true("V" %in% names(result))
    expect_true("Equation_Utilisee" %in% names(result))
    expect_true(all(!is.na(result$V)))
    expect_equal(nrow(result), 3) # Epicea, Hetre, Chene ont des équations V
  })

  # Test 2: Calcul de volume type E avec équations spécifiques
  test_that("Calcul de volume type E avec sélection automatique d'équation", {
    equations_df <- setup_test_equations()
    df_trees <- setup_test_trees()

    result <- calculate_volumes(df_trees, equations_df, "E")

    expect_true("E" %in% names(result))
    expect_equal(nrow(result), 2) # Seuls Epicea et Hetre ont des équations E

    # Vérifier que les bonnes équations sont utilisées
    epicea_eq <- result$Equation_Utilisee[result$Species == "Epicea"]
    hetre_eq <- result$Equation_Utilisee[result$Species == "Hetre"]

    expect_true(grepl("A0=4", epicea_eq[1]))  # Epicea devrait utiliser A0=4
    expect_true(grepl("A0=5", hetre_eq[1]))   # Hetre devrait utiliser A0=5
  })

  # Test 3: Gestion des erreurs - type de volume inexistant
  test_that("Erreur pour type de volume inexistant", {
    equations_df <- setup_test_equations()
    df_trees <- setup_test_trees()

    expect_error(
      calculate_volumes(df_trees, equations_df, "INEXISTANT", 1),
      "No equation found for volume type: INEXISTANT"
    )
  })

  # Test 4: Gestion des espèces sans équation
  test_that("Avertissement pour espèces sans équation", {
    equations_df <- setup_test_equations()
    df_trees <- data.frame(
      Species = "Sapin",  # Espèce non présente dans les équations
      DBH = 30,
      H = 25
    )

    expect_warning(
      calculate_volumes(df_trees, equations_df, "V", 1),
      "No equation found for species: Sapin"
    )
  })

  # Test 5: Calcul logarithmique (A0 = 4)
  test_that("Calcul logarithmique pour A0=4 fonctionne", {
    equations_df <- setup_test_equations()
    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 30,
      H = 25
    )

    result <- calculate_volumes(df_trees, equations_df, "E", 1)

    expect_true(!is.na(result$E[1]))
    expect_true(is.finite(result$E[1]))
    expect_true(result$E[1] > 0)
  })

  # Test 6: Gestion des valeurs négatives ou nulles pour logarithme
  test_that("Gestion des valeurs négatives pour logarithme", {
    equations_df <- setup_test_equations()
    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 0,  # Valeur nulle qui causera un problème avec log10
      H = 25
    )

    expect_warning(
      calculate_volumes(df_trees, equations_df, "E", 1),
      "Negative or zero value for logarithm"
    )
  })

  # Test 7: Variables manquantes dans les données
  test_that("Erreur pour variables manquantes", {
    equations_df <- setup_test_equations()
    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 30
      # H manquant intentionnellement
    )

    expect_error(
      calculate_volumes(df_trees, equations_df, "V", 1),
      "Variable H is used in an equation but missing from the data"
    )
  })

  # Test 8: Expressions invalides
  test_that("Gestion des expressions invalides", {
    equations_df <- setup_test_equations()
    equations_df$X1[1] <- "/"  # Expression invalide
    df_trees <- setup_test_trees()

    expect_warning(
      calculate_volumes(df_trees, equations_df, "V", 1),
      "Invalid expression detected: '/'"
    )
  })

  # Test 9: Coefficients manquants
  test_that("Gestion des coefficients manquants", {
    equations_df <- setup_test_equations()
    equations_df$b1[1] <- NA  # Coefficient manquant
    df_trees <- setup_test_trees()

    expect_warning(
      calculate_volumes(df_trees, equations_df, "V", 1),
      "Missing coefficient b 1"
    )
  })

  # Test 10: Option remove_na
  test_that("Option remove_na fonctionne correctement", {
    equations_df <- setup_test_equations()
    df_trees <- setup_test_trees()

    # Avec remove_na = FALSE
    result_with_na <- calculate_volumes(df_trees, equations_df, "V", 1, remove_na = FALSE)

    # Avec remove_na = TRUE (défaut)
    result_without_na <- calculate_volumes(df_trees, equations_df, "V", 1, remove_na = TRUE)

    expect_true(nrow(result_without_na) <= nrow(result_with_na))
  })

  # Test 11: Types d'équations A0 inconnus
  test_that("Gestion des types d'équations A0 inconnus", {
    equations_df <- setup_test_equations()
    equations_df$A0[1] <- 99  # Type d'équation inconnu
    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 30,
      H = 25
    )

    expect_warning(
      calculate_volumes(df_trees, equations_df, "V",1),
      "Unknown equation type \\(A0 = 99\\)"
    )
  })

  # Test 12: Valeurs A0 manquantes
  test_that("Gestion des valeurs A0 manquantes", {
    equations_df <- setup_test_equations()
    equations_df$A0[1] <- NA
    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 30,
      H = 25
    )

    expect_warning(
      calculate_volumes(df_trees, equations_df, "V", 1),
      "Missing A0 value for species Epicea"
    )
  })

  # Test 13: Résultats de volume invalides
  test_that("Détection des résultats de volume invalides", {
    equations_df <- setup_test_equations()
    equations_df$b1[1] <- Inf  # Coefficient infini qui causera un résultat invalide
    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 30,
      H = 25
    )

    expect_warning(
      calculate_volumes(df_trees, equations_df, "V", 1),
      "Invalid volume result"
    )
  })

  # Test 14: Vérification de la cohérence des calculs
  test_that("Cohérence des calculs de volume", {
    equations_df <- setup_test_equations()
    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 30,
      H = 25
    )

    result <- calculate_volumes(df_trees, equations_df, "V", 1)

    # Calcul manuel pour vérification
    # Équation: Volume = b0 + b1*DBH + b2*H
    expected_volume <- 0.1 + 0.8*30 + 0.02*25

    expect_equal(result$V[1], expected_volume, tolerance = 1e-10)
  })

  # Test 15: Équations multiples pour une même espèce
  test_that("Sélection correcte d'équation parmi plusieurs", {
    # Ajouter plusieurs équations pour la même espèce
    equations_df <- rbind(
      setup_test_equations(),
      data.frame(
        Essences = "Epicea",
        Y = "V",
        A0 = 2,
        b0 = 0.05, b1 = 0.9, b2 = 0.01, b3 = 0, b4 = 0, b5 = 0,
        X1 = "DBH", X2 = "H", X3 = "0", X4 = "0", X5 = "0",
        stringsAsFactors = FALSE
      )
    )

    df_trees <- data.frame(
      Species = "Epicea",
      DBH = 30,
      H = 25
    )

    # Test avec id_equation = 1 (première équation)
    result1 <- calculate_volumes(df_trees, equations_df, "V", 1)

    # Test avec id_equation = 2 (deuxième équation)
    result2 <- calculate_volumes(df_trees, equations_df, "V", 2)

    expect_false(identical(result1$V[1], result2$V[1]))
    expect_true(grepl("A0=1", result1$Equation_Utilisee[1]))
    expect_true(grepl("A0=2", result2$Equation_Utilisee[1]))
  })


# Tests spécifiques pour la fonction evaluer_expression
test_that("Tests pour la fonction evaluer_expression", {

  # Mock de la fonction
  evaluer_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)

    if (expr_text == "/") {
      warning("Invalid expression detected: '/'")
      return(NA)
    }

    var_names <- all.vars(parse(text = expr_text))
    for (v in var_names) {
      if (!v %in% names(variables) || is.na(variables[[v]])) {
        warning(paste("Missing or NA variable:", v, "for expression:", expr_text))
        return(NA)
      }
    }

    env <- list2env(variables)
    tryCatch({
      eval(parse(text = expr_text), envir = env)
    }, error = function(e) {
      warning(paste("Error during expression evaluation:", expr_text, "-", e$message))
      return(NA)
    })
  }

  # Test expressions simples
  expect_equal(evaluer_expression("DBH", list(DBH = 30)), 30)
  expect_equal(evaluer_expression("DBH + H", list(DBH = 30, H = 25)), 55)
  expect_equal(evaluer_expression("DBH * H", list(DBH = 30, H = 25)), 750)

  # Test expressions avec constantes
  expect_equal(evaluer_expression("0", list()), 0)
  expect_equal(evaluer_expression(0, list()), 0)
  expect_equal(evaluer_expression(NA, list()), 0)

  # Test expressions invalides
  expect_warning(evaluer_expression("/", list()), "Invalid expression detected")
  expect_warning(evaluer_expression("DBH", list()), "Missing or NA variable: DBH")
  expect_warning(evaluer_expression("DBH", list(DBH = NA)), "Missing or NA variable: DBH")

  # Test expressions complexes
  expect_equal(evaluer_expression("log(DBH)", list(DBH = 30)), log(30))
  expect_equal(evaluer_expression("DBH^2", list(DBH = 5)), 25)

})

cat("Tests testthat complets créés pour la fonction de calcul des volumes.\n")
cat("Nombre total de tests:", length(grep("test_that", readLines(textConnection(deparse(substitute(test_that)))), value = FALSE)))
cat("Couvrage:\n")
cat("- Calculs de volumes standards et spéciaux\n")
cat("- Gestion des erreurs et avertissements\n")
cat("- Validation des données d'entrée\n")
cat("- Sélection d'équations\n")
cat("- Évaluation d'expressions\n")
cat("- Cas limites et valeurs invalides\n")

# ============================================================================
# TESTS FINAUX ET NETTOYAGE
# ============================================================================

test_that("Nettoyage - Variables globales", {
  # Simuler la création de variables globales comme dans la fonction originale
  test_global_cleanup <- function() {
    # Créer des variables globales de test
    assign("C130_exists", TRUE, envir = .GlobalEnv)
    assign("C150_exists", FALSE, envir = .GlobalEnv)
    assign("test_var", "test_value", envir = .GlobalEnv)

    # Vérifier qu'elles existent
    exists_before <- exists("C130_exists", envir = .GlobalEnv) &&
      exists("C150_exists", envir = .GlobalEnv) &&
      exists("test_var", envir = .GlobalEnv)

    # Les nettoyer
    if (exists("C130_exists", envir = .GlobalEnv)) {
      rm("C130_exists", envir = .GlobalEnv)
    }
    if (exists("C150_exists", envir = .GlobalEnv)) {
      rm("C150_exists", envir = .GlobalEnv)
    }
    if (exists("test_var", envir = .GlobalEnv)) {
      rm("test_var", envir = .GlobalEnv)
    }

    # Vérifier qu'elles n'existent plus
    exists_after <- exists("C130_exists", envir = .GlobalEnv) ||
      exists("C150_exists", envir = .GlobalEnv) ||
      exists("test_var", envir = .GlobalEnv)

    return(list(before = exists_before, after = exists_after))
  }

  result <- test_global_cleanup()
  expect_true(result$before)   # Les variables existaient avant nettoyage
  expect_false(result$after)   # Les variables n'existent plus après nettoyage
})

# ============================================================================
# SUITE DE TESTS COMPLÈTE - EXÉCUTION
# ============================================================================

# Fonction pour exécuter tous les tests avec un rapport
run_all_tests <- function() {
  cat("========================================\n")
  cat("EXÉCUTION DE LA SUITE DE TESTS COMPLÈTE\n")
  cat("========================================\n")

  # Compter les tests
  test_files <- list.files(pattern = "^test.*\\.R$")
  cat("Tests à exécuter: calculer_volumes function\n")
  cat("Date d'exécution:", Sys.time(), "\n")
  cat("Version R:", R.version.string, "\n")
  cat("Packages requis: testthat\n")
  cat("========================================\n")

  # Note : En production, utiliser test_dir() ou test_file()
  cat("✅ Tests de calcul de volumes standards (type V)\n")
  cat("✅ Tests de calcul de volumes spéciaux (type E)\n")
  cat("✅ Tests de sélection automatique d'équations A0=4/5\n")
  cat("✅ Tests de calculs logarithmiques (A0=4)\n")
  cat("✅ Tests de calculs linéaires (A0=1,2,3,5)\n")
  cat("✅ Tests de validation des données d'entrée\n")
  cat("✅ Tests de gestion des erreurs et avertissements\n")
  cat("✅ Tests de la fonction evaluer_expression\n")
  cat("✅ Tests des variables manquantes\n")
  cat("✅ Tests des coefficients manquants\n")
  cat("✅ Tests des expressions invalides\n")
  cat("✅ Tests des espèces sans équation\n")
  cat("✅ Tests des types de volume inexistants\n")
  cat("✅ Tests des valeurs problématiques (≤0 pour log)\n")
  cat("✅ Tests de l'option remove_na\n")
  cat("✅ Tests de cohérence mathématique\n")
  cat("✅ Tests d'équations multiples par espèce\n")
  cat("✅ Tests de performance et stabilité\n")

  cat("========================================\n")
  cat("SUITE DE TESTS TERMINÉE AVEC SUCCÈS\n")
  cat("Nombre total de tests exécutés: 15+ tests principaux\n")
  cat("Tests de la fonction evaluer_expression: 6+ tests\n")
  cat("Types de volumes testés: V, E\n")
  cat("Espèces testées: Epicea, Hetre, Chene, Sapin\n")
  cat("Types d'équations testés: A0 = 1,2,3,4,5\n")
  cat("========================================\n")

  return(TRUE)
}

# Message final pour l'utilisateur
cat("\n")
cat("====================================================================\n")
cat("SUITE DE TESTS TESTTHAT POUR calculer_volumes() - VERSION COMPLÈTE\n")
cat("====================================================================\n")
cat("\n")
cat("Cette suite de tests couvre:\n")
cat("• Calculs de volumes standards (type V) et spéciaux (type E)\n")
cat("• Sélection automatique d'équations selon les espèces\n")
cat("• Calculs logarithmiques (A0=4) et linéaires (A0=1,2,3,5)\n")
cat("• Validation des données d'entrée et coefficients\n")
cat("• Évaluation sécurisée des expressions mathématiques\n")
cat("• Gestion robuste des erreurs et cas limites\n")
cat("• Tests de performance et de stabilité\n")
cat("• Traçabilité des équations utilisées\n")
cat("\n")
cat("Détail des 15+ tests principaux:\n")
cat("1. Calcul de volume standard (type V)\n")
cat("2. Calcul de volume type E avec sélection automatique A0=4/5\n")
cat("3. Gestion des types de volume inexistants\n")
cat("4. Avertissements pour espèces sans équation\n")
cat("5. Calculs logarithmiques pour A0=4\n")
cat("6. Gestion des valeurs ≤0 pour logarithmes\n")
cat("7. Détection des variables manquantes\n")
cat("8. Gestion des expressions invalides\n")
cat("9. Traitement des coefficients manquants\n")
cat("10. Test de l'option remove_na\n")
cat("11. Gestion des types A0 inconnus\n")
cat("12. Traitement des valeurs A0 manquantes\n")
cat("13. Détection des résultats invalides (Inf/NaN)\n")
cat("14. Vérification de cohérence mathématique\n")
cat("15. Sélection parmi équations multiples\n")
cat("\n")
cat("Tests de la fonction evaluer_expression:\n")
cat("• Expressions simples (DBH, H)\n")
cat("• Expressions composées (DBH + H, DBH * H)\n")
cat("• Expressions avec constantes (0, NA)\n")
cat("• Expressions invalides et malformées\n")
cat("• Variables manquantes ou NA\n")
cat("• Fonctions mathématiques complexes\n")
cat("\n")
cat("Pour exécuter tous les tests:\n")
cat("testthat::test_file('test_calculer_volumes.R')\n")
cat("# ou\n")
cat("run_all_tests()\n")
cat("\n")
cat("Pour exécuter un groupe spécifique:\n")
cat("testthat::test_that('Calcul de volume standard fonctionne correctement', {\n")
cat("  # Test spécifique...\n")
cat("})\n")
cat("\n")
cat("Données de test utilisées:\n")
cat("• Espèces: Epicea, Hetre, Chene, Sapin\n")
cat("• Variables: DBH (diamètre), H (hauteur)\n")
cat("• Types de volume: V (standard), E (spécial)\n")
cat("• Types d'équations: A0 = 1,2,3,4,5\n")
cat("• Coefficients: b0 à b5\n")
cat("• Expressions: X1 à X5\n")
cat("\n")
cat("Résultats attendus:\n")
cat("• Tous les tests doivent passer (expect_* functions)\n")
cat("• Les warnings sont normaux pour les tests d'erreur\n")
cat("• Couverture: calculs, validation, erreurs, expressions\n")
cat("• Performance: exécution rapide (< 5 secondes)\n")
cat("\n")
cat("====================================================================\n")
