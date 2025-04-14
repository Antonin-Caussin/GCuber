#' Calcule les volumes forestiers selon différentes méthodes
#'
#' @param donnees_arbres Un data frame contenant les données des arbres
#' @param type_volume Type de volume à calculer (ex: "VC22", "VC22B", "Ecorce")
#' @param essence Essence d'arbre spécifique (ou NULL pour utiliser celle des données)
#' @param equations_df Le data frame contenant les équations allométriques
#' @param id_equation Indice de l'équation à utiliser si plusieurs sont disponibles (par défaut = 1)
#' @return Un data frame avec les données d'entrée plus une colonne de volume calculé
#' @export

calculer_volumes <- function(donnees_arbres, type_volume = "VC22", essence = NULL,
                             equations_df = equations_allometriques, id_equation = 1) {

  # Vérification de la colonne C130 ou conversion depuis C150
  if (!"C130" %in% colnames(donnees_arbres) && "C150" %in% colnames(donnees_arbres)) {
    if (!"Essence" %in% colnames(donnees_arbres)) {
      stop("Impossible de convertir C150 en C130 sans la colonne 'Essence'.")
    }
    if (!exists("coefs_conversion")) {
      stop("Le tableau 'coefs_conversion' est requis pour convertir C150 en C130.")
    }

    donnees_arbres <- merge(donnees_arbres, coefs_conversion, by.x = "Essence", by.y = "Essence", all.x = TRUE)

    if (any(is.na(donnees_arbres$Coef_C150_C130))) {
      stop("Coefficient de conversion manquant pour certaines essences.")
    }

    donnees_arbres$C130 <- donnees_arbres$C150 * donnees_arbres$Coef_C150_C130
  }

  # Vérifier les colonnes requises
  if(!all(c("Essence", "C130") %in% colnames(donnees_arbres))) {
    stop("Le fichier doit contenir au moins les colonnes 'Essence' et 'C130'")
  }

  # Filtrer les équations pour le type de volume demandé
  eqs_volume <- equations_df[equations_df$Y == type_volume, ]

  if(nrow(eqs_volume) == 0) {
    stop(paste("Aucune équation trouvée pour le type de volume:", type_volume))
  }

  # Vérifier si l'équation nécessite HDOM
  necessite_hdom <- any(grepl("HDOM", c(eqs_volume$X1, eqs_volume$X2,
                                        eqs_volume$X3, eqs_volume$X4, eqs_volume$X5)))

  if(necessite_hdom && !("HDOM" %in% colnames(donnees_arbres))) {
    stop("Certaines équations nécessitent la colonne 'HDOM' qui est absente de vos données")
  }

  # Fonction pour évaluer dynamiquement une expression en remplaçant les variables
  evaluer_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
    env <- list2env(variables)
    return(eval(parse(text = expr_text), envir = env))
  }

  # Créer colonne pour le résultat
  donnees_arbres$Volume <- NA

  # Pour chaque arbre
  for(i in 1:nrow(donnees_arbres)) {
    essence_arbre <- donnees_arbres$Essence[i]
    c130_arbre <- donnees_arbres$C130[i]

    # Variables disponibles pour les calculs
    variables <- list("C130" = c130_arbre)
    if("HDOM" %in% colnames(donnees_arbres)) {
      variables[["HDOM"]] <- donnees_arbres$HDOM[i]
    }

    # Chercher l'équation appropriée
    if(!is.null(essence)) {
      eq <- eqs_volume[eqs_volume$Essences == essence, ]
    } else {
      eq <- eqs_volume[eqs_volume$Essences == essence_arbre, ]
      if(nrow(eq) == 0) {
        eq <- eqs_volume[eqs_volume$Essences == "General", ]
      }
    }

    if(nrow(eq) == 0) {
      warning(paste("Pas d'équation trouvée pour l'essence:", essence_arbre))
      next
    }

    # S'il y a plusieurs équations, utiliser celle choisie par l'utilisateur
    if(nrow(eq) > 1) {
      if(id_equation > nrow(eq)) {
        stop(paste("L'index id_equation =", id_equation,
                   "dépasse le nombre d'équations disponibles pour", essence_arbre))
      }
      eq <- eq[id_equation, ]
      message(paste("Plusieurs équations trouvées pour", essence_arbre,
                    "- utilisation de l'équation à ", id_equation, "entré"))
    }

    # Calculer le volume selon le type d'équation (valeur de A0)
    if(eq$A0 == 1) {
      # Équation polynomiale standard
      volume <- eq$b0
      for(j in 1:5) {
        terme_x <- paste0("X", j)
        terme_b <- paste0("b", j)

        if(!is.na(eq[[terme_x]]) && eq[[terme_x]] != "0") {
          x_val <- evaluer_expression(eq[[terme_x]], variables)
          volume <- volume + eq[[terme_b]] * x_val
        }
      }

    } else if(eq$A0 == 2) {
      # Équation logarithmique : 10^(b0 + b1 * log(variable))
      log_input <- evaluer_expression(eq$X1, variables)
      volume <- 10^(eq$b0 + eq$b1 * log_input)
    }

    # Ajouter le volume calculé
    donnees_arbres$Volume[i] <- volume
  }

  return(donnees_arbres)
}
