
# Exemple d'utilisation de la fonction Biomasse
data <- data.frame(essence = 'Bouleau', volume = 100)
resultat <- Biomasse(data, type = 'VC22')
print(resultat)
# Exemple d'utilisation de la fonction liste_eq
equations_allometriques <- data.frame(
  essence = c("Bouleau", "Chêne"),
  type_volume = c("VC22", "VC23"),
  equation = c("eq1", "eq2")
)

liste_eq(equations_allometriques, essence = "Bouleau", type_volume = "VC22")

