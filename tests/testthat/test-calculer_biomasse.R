# tests/testthat/test-calculer_biomasse.R

test_that("calculer_biomasse fonctionne avec le data frame equations interne", {
  # Les tests dans testthat ont automatiquement accès aux données internes du package

  # Créer des données de test avec des essences qui existent réellement dans votre equations
  essences_dispo <- unique(equations$Essences[equations$Y == "BIOMASSE"])
  expect_true(length(essences_dispo) > 0, "Aucune essence trouvée dans le data frame equations")

  # Sélectionner une essence pour les tests
  essence_test <- essences_dispo[1]

  # Créer un jeu de données simple pour le test
  donnees_test <- data.frame(
    Essence = essence_test,
    VC22 = 100,
    VC22B = 90
  )

  # Exécuter la fonction
  resultat <- calculer_biomasse(donnees_test)

  # Vérifications
  expect_false(is.na(resultat$Biomasse[1]), "La biomasse calculée est NA")
  expect_false(is.na(resultat$Carbone[1]), "Le carbone calculé est NA")

  # Vérifier que le calcul est correct
  eq <- equations[equations$Y == "BIOMASSE" & equations$Essences == essence_test, ][1, ]
  biomasse_attendue <- 100 * eq$BF * eq$EF
  carbone_attendu <- biomasse_attendue * eq$Carbone

  expect_equal(resultat$Biomasse[1], biomasse_attendue,
               tolerance = 1e-6,
               label = "La biomasse calculée ne correspond pas à la valeur attendue")
  expect_equal(resultat$Carbone[1], carbone_attendu,
               tolerance = 1e-6,
               label = "Le carbone calculé ne correspond pas à la valeur attendue")

  # Tester le comportement avec une essence inconnue
  donnees_test_inconnu <- data.frame(
    Essence = "EssenceInexistante",
    VC22 = 100
  )

  # Si vous avez une essence "General" dans votre dataframe
  if ("General" %in% essences_dispo) {
    resultat_inconnu <- calculer_biomasse(donnees_test_inconnu)
    expect_false(is.na(resultat_inconnu$Biomasse[1]),
                 "L'essence inconnue devrait utiliser l'équation 'General'")
  }
})
