#' @title Tests pour la fonction calculer_biomasse
#' @description Tests unitaires pour vérifier le comportement de la fonction calculer_biomasse
# Test de la fonction calculer_biomasse
test_that("La fonction calculer_biomasse calcule correctement la biomasse et le carbone", {

  # Création de données de test
  donnees_test <- data.frame(
    Essence = c("Chene", "Hetre", "Pin", "Inconnu"),
    VC22 = c(120, 150, 100, 80),
    Volume_autre = c(125, 155, 105, 85)
  )

  equations_test <- data.frame(
    Y = rep("Biomasse", 3),
    Essences = c("Chene", "Hetre", "General"),
    BEF = c(0.7, 0.8, 0.75),
    EF = c(0.9, 0.85, 0.88),
    Carbone = c(0.5, 0.48, 0.49),
    VC_source = c("VC22", "VC22", "VC22")
  )

  # Test avec paramètres par défaut
  resultat <- calculer_biomasse(donnees_test, equations_df = equations_test)

  # Vérification que les colonnes attendues sont présentes
  expect_true(all(c("Biomasse", "Carbone") %in% names(resultat)))

  # Vérification des calculs pour le Chêne
  # Biomasse = volume * BEF * EF = 120 * 0.7 * 0.9 = 75.6
  # Carbone = Biomasse * facteur_carbone = 75.6 * 0.5 = 37.8
  expect_equal(resultat$Biomasse[1], 120 * 0.7 * 0.9)
  expect_equal(resultat$Carbone[1], 120 * 0.7 * 0.9 * 0.5)

  # Vérification des calculs pour le Hêtre
  expect_equal(resultat$Biomasse[2], 150 * 0.8 * 0.85)
  expect_equal(resultat$Carbone[2], 150 * 0.8 * 0.85 * 0.48)

  # Vérification que le Pin utilise l'équation General
  expect_equal(resultat$Biomasse[3], 100 * 0.75 * 0.88)
  expect_equal(resultat$Carbone[3], 100 * 0.75 * 0.88 * 0.49)

  # Vérification que "Inconnu" utilise aussi l'équation General
  expect_equal(resultat$Biomasse[4], 80 * 0.75 * 0.88)
  expect_equal(resultat$Carbone[4], 80 * 0.75 * 0.88 * 0.49)
})

test_that("Le paramètre colonne_volume fonctionne correctement", {

  donnees_test <- data.frame(
    Essence = c("Chene", "Hetre"),
    VC22 = c(120, 150),
    Volume_autre = c(125, 155)
  )

  equations_test <- data.frame(
    Y = rep("Biomasse", 2),
    Essences = c("Chene", "Hetre"),
    BEF = c(0.7, 0.8),
    EF = c(0.9, 0.85),
    Carbone = c(0.5, 0.48),
    VC_source = c("VC22", "VC22")
  )

  # Test avec colonne_volume spécifiée
  resultat <- calculer_biomasse(donnees_test, equations_df = equations_test, colonne_volume = "Volume_autre")

  # Vérification que les calculs utilisent bien la colonne Volume_autre
  expect_equal(resultat$Biomasse[1], 125 * 0.7 * 0.9)
  expect_equal(resultat$Biomasse[2], 155 * 0.8 * 0.85)
})

test_that("La fonction gère correctement les données manquantes et invalides", {

  donnees_test <- data.frame(
    Essence = c("Chene", "Hetre", "Chene", "Hetre"),
    VC22 = c(120, NA, -10, 150)
  )

  equations_test <- data.frame(
    Y = rep("Biomasse", 2),
    Essences = c("Chene", "Hetre"),
    BEF = c(0.7, 0.8),
    EF = c(0.9, 0.85),
    Carbone = c(0.5, 0.48),
    VC_source = c("VC22", "VC22")
  )

  # Test avec afficher_warnings = FALSE pour capturer les messages
  expect_warning(
    resultat <- calculer_biomasse(donnees_test, equations_df = equations_test),
    "Valeurs manquantes|Valeurs invalides"
  )

  # Vérification que les lignes avec des problèmes ont des NA
  expect_equal(resultat$Biomasse[1], 120 * 0.7 * 0.9)
  expect_true(is.na(resultat$Biomasse[2]))  # NA volume
  expect_true(is.na(resultat$Biomasse[3]))  # Volume négatif
  expect_equal(resultat$Biomasse[4], 150 * 0.8 * 0.85)

  # Test avec stockage des messages
  resultat2 <- calculer_biomasse(donnees_test, equations_df = equations_test, afficher_warnings = FALSE)
  expect_true("Messages" %in% names(resultat2))
  expect_true(grepl("Valeurs manquantes", resultat2$Messages[2]))
  expect_true(grepl("Valeurs invalides", resultat2$Messages[3]))
})

test_that("La fonction détecte correctement les colonnes manquantes", {
  # Test sans colonne Essence
  donnees_sans_essence <- data.frame(
    NonEssence = c("Chene", "Hetre"),
    VC22 = c(120, 150)
  )

  equations_test <- data.frame(
    Y = rep("Biomasse", 2),
    Essences = c("Chene", "Hetre"),
    BEF = c(0.7, 0.8),
    EF = c(0.9, 0.85),
    Carbone = c(0.5, 0.48),
    VC_source = c("VC22", "VC22")
  )

  # Vérifie que l'erreur est bien levée avec le bon message
  expect_error(
    calculer_biomasse(donnees_sans_essence, equations_df = equations_test),
    "La colonne 'Essence' est requise dans les données"
  )

  # Test avec colonne de volume manquante
  donnees_test <- data.frame(
    Essence = c("Chene", "Hetre"),
    VolumeFaux = c(120, 150)
  )

  # Vérifie que l'avertissement est bien levé
  expect_warning(
    calculer_biomasse(donnees_test, equations_df = equations_test),
    "Colonne de volume VC22 absente"
  )

  # Test avec equations_df incomplet
  equations_incomplet <- data.frame(
    Y = rep("Biomasse", 2),
    Essences = c("Chene", "Hetre")
    # Manque BEF, EF, Carbone
  )

  expect_error(
    calculer_biomasse(donnees_test, equations_df = equations_incomplet),
    "Colonnes manquantes dans equations_df"
  )
})
