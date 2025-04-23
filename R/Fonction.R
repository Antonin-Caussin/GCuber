#' Calcule les volumes forestiers selon differentes methodes
#'
#' Cette fonction applique des equations allometriques pour estimer le volume
#' des arbres a partir de differentes variables dendrometriques.
#'
#' @param df Un data frame contenant les donnees des arbres avec au moins une colonne d'identification
#'        d'essence ("Essence", "Code", ou "Abr")
#' @param type_volume Type de volume a calculer (ex: "VC22", "VC22B", "E", "VC22_HA")
#' @param essence Essence d'arbre specifique. Si NULL (par defaut), l'essence est lue ligne par ligne dans `df`.
#' @param equations_df Le data frame contenant les equations allometriques a appliquer
#' @param id_equation Indice de l'equation a utiliser si plusieurs sont disponibles (par defaut = 1)
#' @param coefs_conversion (Optionnel) Data frame pour convertir C150 en C130 (doit contenir "Essence" et "Coef_C150_C130")
#' @param remove_na (Optionnel) Si TRUE, supprime les lignes où aucun volume n'a pu etre calcule (par defaut = FALSE)
#'
#' @details
#' Le champ `A0` dans les equations allometriques indique le type de formule utilisee :
#' \itemize{
#'   \item{1, 2, 3, 5 : Formule lineaire du type }{ V = b0 + b1*X1 + b2*X2 + ... }
#'   \item{4 : Formule logarithmique du type }{ V = 10^(b0 + b1 * log10(X1)) }
#' }
#'
#' @return Un data frame avec les colonnes d'origine augmentees de :
#' \itemize{
#'   \item `Volume` : volume calcule selon l'equation choisie
#'   \item `Equation_Utilisee` : description de l'equation utilisee
#' }
#' @export

calculer_volumes <- function(df, type_volume = "VC22", essence = NULL,
                             equations_df = equations, id_equation = 1,
                             coefs_conversion = NULL, remove_na = FALSE) {

  # Liste des types de volume valides
  types_volume_valides <- c("VC22", "VC22B", "E", "VC22_HA")

  # Verification du type de volume
  if (!(type_volume %in% types_volume_valides)) {
    stop(paste("Type de volume invalide:", type_volume,
               "\nTypes valides:", paste(types_volume_valides, collapse = ", ")))
  }

  # Vérification et préparation des identifiants d'essence
  colonnes_id_essence <- c("Essence", "Code", "Abr")
  colonne_essence_trouvee <- NULL

  # Vérifie quelles colonnes d'identification sont présentes dans df
  colonnes_presentes <- intersect(colonnes_id_essence, colnames(df))

  if (length(colonnes_presentes) == 0) {
    stop("Le fichier doit contenir au moins une des colonnes suivantes: 'Essence', 'Code', ou 'Abr'")
  }

  # Utilise la première colonne présente comme identifiant d'essence
  colonne_essence_trouvee <- colonnes_presentes[1]

  # Si nécessaire, ajoute une colonne Essence standardisée pour correspondre aux équations
  if (colonne_essence_trouvee != "Essence") {
    # Extrait les correspondances uniques des essences depuis equations_df
    if (!all(c("Essences", colonne_essence_trouvee) %in% colnames(equations_df))) {
      stop(paste("Le dataframe 'equations_df' doit contenir les colonnes 'Essences' et '",
                 colonne_essence_trouvee, "'", sep=""))
    }

    # Crée un dataframe de correspondance à partir de equations_df
    mapping_df <- unique(equations_df[, c("Essences", colonne_essence_trouvee)])
    names(mapping_df)[names(mapping_df) == "Essences"] <- "Essence"  # Standardise le nom

    # Crée une colonne temporaire Essence pour les calculs
    df <- merge(df, mapping_df, by = colonne_essence_trouvee, all.x = TRUE)

    # Vérifier si des correspondances n'ont pas été trouvées
    if (any(is.na(df$Essence))) {
      na_values <- unique(df[[colonne_essence_trouvee]][is.na(df$Essence)])
      warning(paste("Aucune correspondance trouvée pour les valeurs suivantes de",
                    colonne_essence_trouvee, ":", paste(na_values, collapse=", ")))

    }
  }

  # Verification des colonnes requises pour le diamètre
  if (!any(c("C130", "C150") %in% colnames(df))) {
    stop("Le fichier doit contenir au moins une colonne 'C130' ou 'C150'")
  }

  # Conversion si necessaire
  if (!"C130" %in% colnames(df) && "C150" %in% colnames(df)) {
    if (is.null(coefs_conversion)) {
      stop("Le tableau 'coefs_conversion' est requis pour convertir C150 en C130.")
    }

    df <- merge(df, coefs_conversion, by = "Essence", all.x = TRUE)

    if (any(is.na(df$Coef_C150_C130))) {
      stop("Coefficient de conversion manquant pour certaines essences.")
    }

    df$C130 <- df$C150 * df$Coef_C150_C130
  }

  # Calcul des surfaces terrieres si necessaires
  if (!"G130" %in% colnames(df) && "C130" %in% colnames(df)) {
    df$G130 <- (df$C130^2) / (4 * pi)
  }

  if (!"G150" %in% colnames(df) && "C150" %in% colnames(df)) {
    df$G150 <- (df$C150^2) / (4 * pi)
  }

  # Filtrer les equations
  eqs_volume <- equations_df[equations_df$Y == type_volume, ]
  if (nrow(eqs_volume) == 0) {
    stop(paste("Aucune equation trouvee pour le type de volume:", type_volume))
  }

  # S'assurer que les coefficients b0 à b5 sont numeriques
  colonnes_b <- paste0("b", 0:5)
  eqs_volume[colonnes_b] <- lapply(eqs_volume[colonnes_b], as.numeric)


  # Initialisation
  df$Equation_Utilisee <- NA_character_
  df$Volume <- NA_real_

  # Verifier les variables requises pour l'equation choisie uniquement
  if (!is.null(essence)) {
    eq_candidates_check <- eqs_volume[eqs_volume$Essences == essence, ]
  } else {
    # On cherche les équations correspondant aux essences dans le dataframe
    # ou l'équation générale si aucune ne correspond
    unique_essences <- unique(df$Essence)
    eq_candidates_check <- eqs_volume[eqs_volume$Essences %in% unique_essences, ]
    if (nrow(eq_candidates_check) == 0)
      eq_candidates_check <- eqs_volume[eqs_volume$Essences == "General", ]
  }

  if (nrow(eq_candidates_check) < id_equation) {
    stop(paste("id_equation =", id_equation,
               "dépasse le nombre d'équations disponibles pour l'essence spécifiée"))
  }

  # Vérifier uniquement pour l'équation sélectionnée
  eq_select <- eq_candidates_check[id_equation, , drop = FALSE]

  # Extraire les variables requises par cette équation spécifique
  expressions_utilisees <- as.character(unlist(eq_select[1, paste0("X", 1:5)]))
  expressions_utilisees <- expressions_utilisees[!is.na(expressions_utilisees) & expressions_utilisees != "0"]
  variables_requises <- unique(unlist(regmatches(expressions_utilisees,
                                                 gregexpr("[A-Za-z_][A-Za-z0-9_]*", expressions_utilisees))))

  # Vérifier si ces variables sont présentes
  for (var in variables_requises) {
    if (!(var %in% colnames(df))) {
      stop(paste("La variable", var,
                 "est requise par l'équation sélectionnée (id_equation =",
                 id_equation, ") mais absente des données."))
    }
  }


  # Fonction d'evaluation securisee
  evaluer_expression <- function(expr_text, variables) {
    if (is.na(expr_text) || expr_text == "0" || expr_text == 0) return(0)
    env <- list2env(variables)
    tryCatch({
      eval(parse(text = expr_text), envir = env)
    }, error = function(e) {
      stop(paste("Erreur lors de l'evaluation de l'expression:", expr_text, "-", e$message))
    })
  }

  # Calcul pour chaque ligne
  for (i in seq_len(nrow(df))) {
    essence_arbre <- df$Essence[i]

    eq_candidates <- if (!is.null(essence)) {
      eqs_volume[eqs_volume$Essences == essence, ]
    } else {
      eq_found <- eqs_volume[eqs_volume$Essences == essence_arbre, ]
      if (nrow(eq_found) == 0) eq_found <- eqs_volume[eqs_volume$Essences == "General", ]
      eq_found
    }

    if (nrow(eq_candidates) == 0) {
      warning(paste("Pas d'equation trouvee pour l'essence:", essence_arbre))
      next
    }

    if (nrow(eq_candidates) < id_equation) {
      stop(paste("id_equation =", id_equation, "depasse le nombre d'equations disponibles (",
                 nrow(eq_candidates), ") pour", essence_arbre))
    }

    eq <- eq_candidates[id_equation, , drop = FALSE]
    df$Equation_Utilisee[i] <- paste0(eq$Essences, ":", eq$Y, ":A0=", eq$A0)

    exprs <- as.character(unlist(eq[1, paste0("X", 1:5)]))
    exprs <- exprs[!is.na(exprs) & exprs != "0"]
    vars_needed <- unique(unlist(regmatches(exprs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", exprs))))

    variables <- list()
    for (v in vars_needed) {
      if (v %in% names(df)) {
        variables[[v]] <- df[[v]][i]
      } else {
        stop(paste("La variable", v, "est utilisee dans une equation mais absente des donnees."))
      }
    }

    a0_value <- eq$A0[1]

    if (a0_value %in% c(1, 2, 3, 5)) {
      volume <- eq$b0[1]
      for (j in 1:5) {
        x_col <- paste0("X", j)
        b_col <- paste0("b", j)
        if (!is.na(eq[[x_col]][1]) && eq[[x_col]][1] != "0") {
          x_val <- evaluer_expression(eq[[x_col]][1], variables)
          volume <- volume + eq[[b_col]][1] * x_val
        }
      }
    } else if (a0_value == 4) {
      C130 <- evaluer_expression(eq$X1[1], variables)  # Recupere directement la valeur de C130
      if (C130 <= 0) {
        warning(paste("Valeur negative ou nulle pour logarithme a la ligne", i))
        next
      }
      volume <- 10^(eq$b0[1] + eq$b1[1] * log10(C130))
    } else {
      warning(paste("Type d'equation inconnu (A0 =", a0_value, ") pour la ligne", i))
      next
    }

    if (is.na(volume) || !is.finite(volume)) {
      warning(paste("Resultat de volume non valide a la ligne", i, ":", volume))
      next
    }

    df$Volume[i] <- volume
  }

  # La colonne Essence est conservée même si elle a été créée pendant l'exécution
  # Aucun code de nettoyage n'est nécessaire ici

  # Nettoyage final si demande
  if (remove_na) {
    df <- df[!is.na(df$Volume), ]
  }

  return(df)
}
