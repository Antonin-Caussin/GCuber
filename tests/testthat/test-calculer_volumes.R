#' @title Tests pour la fonction calculer_volumes
#' @description Tests unitaires pour vérifier le comportement de la fonction calculer_volumes

# Charger les bibliothèques nécessaires
library(testthat)

# Test de la fonction calculer_volumes
test_that("La fonction calculer_volumes calcule correctement les volumes", {

  # Création de données de test
  donnees_test <- data.frame(
    Essence = c("Chene", "Hetre", "Pin", "Inconnu"),
    C130 = c(120, 150, 100, 80),
    HDOM = c(15, 18, 14, 12),
    Volume_autre = c(125, 155, 105, 85)
  )

  equations_test <- data.frame(
    Y = rep("VC22", 3),
    Essences = c("Chene", "Hetre", "General"),
    A0 = c(1, 1, 1),  # A0 pour équation polynomiale
    b0 = c(0.5, 0.6, 0.7),
    b1 = c(0.1, 0.2, 0.15),
    b2 = c(0.1, 0.2, 0.3),
    b3 = c(0.01, 0.02, 0.03),
    b4 = c(0.001, 0.002, 0.003),
    b5 = c(0, 0, 0),
    X1 = c("C130", "C130", "C130"),
    X2 = c("HDOM", "HDOM", "HDOM"),
    X3 = c(NA, NA, NA),
    X4 = c(NA, NA, NA),
    X5 = c(NA, NA, NA)
  )

  # Test avec paramètres par défaut
  resultat <- calculer_volumes(donnees_test, equations_df = equations_test)

  # Vérification que la colonne Volume a été ajoutée
  expect_true("Volume" %in% names(resultat))

  # Vérification des calculs pour le Chêne
  # Volume = b0 + b1 * C130 + b2 * HDOM = 0.5 + 0.1 * 120 + 0.1 * 15
  # Volume = 0.5 + 12 + 1.5 = 14
  expect_equal(resultat$Volume[1], 0.5 + 0.1 * 120 + 0.1 * 15)

  # Vérification des calculs pour le Hêtre
  expect_equal(resultat$Volume[2], 0.6 + 0.2 * 150 + 0.2 * 18)

  # Vérification que le Pin utilise l'équation General
  expect_equal(resultat$Volume[3], 0.7 + 0.15 * 100 + 0.3 * 14)

  # Vérification que "Inconnu" utilise aussi l'équation General
  expect_equal(resultat$Volume[4], 0.7 + 0.15 * 80 + 0.3 * 12)
})

test_that("La fonction gère correctement les données manquantes et invalides", {

  donnees_test <- data.frame(
    Essence = c("Chene", "Hetre", "Chene", "Hetre"),
    C130 = c(120, NA, -10, 150)
  )

  equations_test <- data.frame(
    Y = rep("VC22", 2),
    Essences = c("Chene", "Hetre"),
    A0 = c(1, 1),
    b0 = c(0.5, 0.6),
    b1 = c(0.1, 0.2),
    b2 = c(0.1, 0.2),
    b3 = c(0.01, 0.02),
    b4 = c(0.001, 0.002),
    b5 = c(0, 0),
    X1 = c("C130", "C130"),
    X2 = c("HDOM", "HDOM"),
    X3 = c(NA, NA),
    X4 = c(NA, NA),
    X5 = c(NA, NA)
  )

  # Test avec afficher_warnings = FALSE pour capturer les messages
  expect_warning(
    resultat <- calculer_volumes(donnees_test, equations_df = equations_test),
    "Valeurs manquantes|Valeurs invalides"
  )

  # Vérification que les lignes avec des problèmes ont des NA
  expect_equal(resultat$Volume[1], 0.5 + 0.1 * 120 + 0.1 * 15)
  expect_true(is.na(resultat$Volume[2]))  # NA volume
  expect_true(is.na(resultat$Volume[3]))  # Volume négatif
  expect_equal(resultat$Volume[4], 0.7 + 0.15 * 150 + 0.3 * 18)
})

test_that("La fonction détecte correctement les colonnes manquantes", {
  # Test sans colonne Essence
  donnees_sans_essence <- data.frame(
    NonEssence = c("Chene", "Hetre"),
    C130 = c(120, 150)
  )

  equations_test <- data.frame(
    Y = rep("VC22", 2),
    Essences = c("Chene", "Hetre"),
    A0 = c(1, 1),
    b0 = c(0.5, 0.6),
    b1 = c(0.1, 0.2),
    b2 = c(0.1, 0.2),
    b3 = c(0.01, 0.02),
    b4 = c(0.001, 0.002),
    b5 = c(0, 0),
    X1 = c("C130", "C130"),
    X2 = c("HDOM", "HDOM"),
    X3 = c(NA, NA),
    X4 = c(NA, NA),
    X5 = c(NA, NA)
  )

  # Vérifie que l'erreur est bien levée avec le bon message
  expect_error(
    calculer_volumes(donnees_sans_essence, equations_df = equations_test),
    "La colonne 'Essence' est requise dans les données"
  )
})

test_that("La fonction détecte les équations manquantes ou invalides", {
  # Test avec equations_df incomplet
  equations_incomplet <- data.frame(
    Y = rep("VC22", 2),
    Essences = c("Chene", "Hetre")
    # Manque A0, b0, b1, etc.
  )

  expect_error(
    calculer_volumes(donnees_test, equations_df = equations_incomplet),
    "Colonnes manquantes dans equations_df"
  )
})
