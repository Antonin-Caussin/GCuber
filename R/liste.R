#' Explore and Display Forest Allometric Equations
#'
#' This function provides an interface to explore forest allometric equations
#' from the carbofor database. It allows filtering by species and/or equation type,
#' and presents results in formatted tables with complete equation formulas.
#'
#' @param species Character string. Name of the tree species to filter for.
#'   Must match exactly the species names in the database. Use \code{list_species()}
#'   to see available species. Default is \code{NULL} (no species filter).
#'
#' @param equation_type Character string. Type of equation to filter for
#'   (e.g., "V22", "biomass", "height"). Must match exactly the equation types
#'   in the Y column of the database. Use \code{list_equation_types()} to see
#'   available types. Default is \code{NULL} (no equation type filter).
#'
#' @param plot Logical. Whether to display formatted tables. If \code{TRUE}
#'   (default), attempts to create formatted HTML tables using knitr/kableExtra
#'   or gt packages. If these packages are not available, falls back to console
#'   output. If \code{FALSE}, returns data without visual formatting.
#'
#' @return Invisibly returns a data frame containing the filtered results:
#'   \itemize{
#'     \item If no filters are applied: Summary table with species names,
#'           available equation types, and sources
#'     \item If filters are applied: Detailed table with equation IDs,
#'           species, volume types, sources, and complete equation formulas
#'   }
#'
#' @details
#' The function operates in two modes:
#' \itemize{
#'   \item \strong{Summary mode} (no filters): Shows overview of all available
#'         species with their equation types and sources
#'   \item \strong{Detailed mode} (with filters): Shows complete equations with
#'         coefficients formatted as mathematical expressions
#' }
#'
#' Equations are built using the \code{build_equation()} helper function, which
#' formats coefficients and variables into readable mathematical expressions.
#' Only non-zero, non-NA coefficients are included in the final equations.
#'
#' The function supports visual output through multiple table formatting packages:
#' \itemize{
#'   \item Primary: knitr + kableExtra (HTML tables with Bootstrap styling)
#'   \item Alternative: gt package (Grammar of Tables)
#'   \item Fallback: Console output with formatted text
#' }
#'
#' @section Database Structure:
#' The function expects a data frame named \code{equations} with the following columns:
#' \itemize{
#'   \item \code{Species}: Species names
#'   \item \code{A0}: Equation IDs
#'   \item \code{Y}: Equation types/response variables
#'   \item \code{Source_Eq}: Source references
#'   \item \code{X0, X1, X2, X3, X4, X5}: Predictor variable names
#'   \item \code{b0, b1, b2, b3, b4, b5}: Equation coefficients
#' }
#'
#' @examples
#' \dontrun{
#' # Display all available species and equation types
#' carbofor_species()
#'
#' # Get equations for a specific species
#' carbofor_species("Fagus sylvatica")
#' carbofor_species("Picea abies")
#'
#' # Get equations of a specific type
#' carbofor_species(equation_type = "V22")
#' carbofor_species(equation_type = "biomass")
#'
#' # Combine filters: specific species and equation type
#' carbofor_species("Fagus sylvatica", equation_type = "V22")
#'
#' # Get data without visual formatting for further analysis
#' data <- carbofor_species(plot = FALSE)
#' equations_v22 <- carbofor_species(equation_type = "V22", plot = FALSE)
#'
#' # Using the shorter alias
#' species("Quercus robur")
#' species()
#' }
#' @seealso
#' \code{\link{list_species}} for listing available species names
#' \code{\link{list_equation_types}} for listing available equation types
#' \code{\link{species}} for the shorter function alias
#' \code{\link{equation}} for filtering by equation type only
#'
#' @author [Your Name]
#' @export
#' @importFrom dplyr filter rowwise mutate ungroup select rename arrange group_by summarise
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec
#' @importFrom gt gt tab_header tab_style cell_fill cell_text cells_column_labels cells_body


# Helper function to build equation string
build_equation <- function(x_cols, b_cols, y_var) {
  # Get non-zero coefficients and their corresponding variables
  valid_terms <- !is.na(b_cols) & b_cols != 0 & !is.na(x_cols) & x_cols != "0" & x_cols != ""

  if (!any(valid_terms)) {
    return("No valid equation")
  }

  # Build equation terms
  terms <- character(0)

  for (i in seq_along(b_cols)) {
    if (valid_terms[i]) {
      coef <- b_cols[i]
      var <- x_cols[i]

      # Format coefficient
      if (i == 1) {
        # First term (intercept or first variable)
        if (var == "1" || var == "") {
          terms <- c(terms, sprintf("%.4f", coef))
        } else {
          terms <- c(terms, sprintf("%.4f*%s", coef, var))
        }
      } else {
        # Subsequent terms
        sign <- ifelse(coef >= 0, " + ", " - ")
        abs_coef <- abs(coef)

        if (var == "1" || var == "") {
          terms <- c(terms, sprintf("%s%.4f", sign, abs_coef))
        } else {
          terms <- c(terms, sprintf("%s%.4f*%s", sign, abs_coef, var))
        }
      }
    }
  }

  equation <- paste0(y_var, " = ", paste(terms, collapse = ""))
  return(equation)
}

# Main function with short, evocative name
carbofor_species <- function(species = NULL, equation_type = NULL, plot = TRUE) {

  # Start with full dataset
  filtered_data <- equations

  # Filter by species if specified
  if (!is.null(species)) {
    if (!"Species" %in% names(equations)) {
      stop("Erreur : la colonne 'Species' est manquante dans le jeu de donnees `equations`.")
    }

    filtered_data <- filtered_data %>%
      filter(Species == species)

    if (nrow(filtered_data) == 0) {
      cat(sprintf("No equations found for species: %s\n", species))
      cat("Available species:\n")
      available <- unique(equations$Species)
      cat(paste(available, collapse = ", "))
      return(invisible(NULL))
    }
  }

  # Filter by equation type if specified
  if (!is.null(equation_type)) {
    if (!"Y" %in% names(equations)) {
      stop("Erreur : la colonne 'Y' (type d'equation) est manquante dans le jeu de donnees `equations`.")
    }

    filtered_data <- filtered_data %>%
      filter(Y == equation_type)

    if (nrow(filtered_data) == 0) {
      cat(sprintf("No equations found for equation type: %s\n", equation_type))
      cat("Available equation types:\n")
      available <- unique(equations$Y)
      cat(paste(available, collapse = ", "))
      return(invisible(NULL))
    }
  }

  # If we have specific filters, build detailed equations
  if (!is.null(species) || !is.null(equation_type)) {
    # Build equations for each row
    result <- filtered_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        Equation = build_equation(
          x_cols = c(X0, X1, X2, X3, X4, X5),
          b_cols = c(b0, b1, b2, b3, b4, b5),
          y_var = Y
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(Species, A0, Y, Source_Eq, Equation) %>%
      dplyr::rename(
        Equation_ID = A0,
        Volume_Type = Y,
        Source = Source_Eq
      ) %>%
      dplyr::arrange(Equation_ID)

  } else {
    # Summary by species with sample equations
    result <- equations %>%
      group_by(Species = Species) %>%
      summarise(
        `Equations disponibles` = paste(unique(Y), collapse = ", "),
        Sources = paste(unique(Source_Eq), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(Species)
  }

  # Create visual table if requested
  if (plot && requireNamespace("knitr", quietly = TRUE) &&
      requireNamespace("kableExtra", quietly = TRUE)) {

    # Create formatted table
    table_output <- result %>%
      knitr::kable(format = "html",
                   caption = ifelse(is.null(species) && is.null(equation_type),
                                    "Available Species and Equations in carbofor",
                                    paste("Equations for",
                                          ifelse(!is.null(species), species, ""),
                                          ifelse(!is.null(equation_type), paste("- Type:", equation_type), "")))) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                full_width = FALSE,
                                font_size = 12) %>%
      kableExtra::column_spec(1, bold = TRUE, color = "darkblue") %>%
      kableExtra::row_spec(0, bold = TRUE, color = "white", background = "darkgreen")

    # Display the table
    print(table_output)

  } else if (plot && requireNamespace("gt", quietly = TRUE)) {

    # Alternative with gt package
    table_output <- result %>%
      gt::gt() %>%
      gt::tab_header(
        title = ifelse(is.null(species) && is.null(equation_type),
                       "Available Species and Equations in carbofor",
                       paste("Equations for",
                             ifelse(!is.null(species), species, ""),
                             ifelse(!is.null(equation_type), paste("- Type:", equation_type), "")))
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = "lightblue"),
        locations = gt::cells_column_labels()
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = 1)
      )

    print(table_output)

  } else {
    # Fallback: print nice console table
    cat("\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    if (is.null(species) && is.null(equation_type)) {
      cat("AVAILABLE SPECIES AND EQUATIONS IN CARBOFOR\n")
    } else {
      cat(sprintf("EQUATIONS FOR %s %s\n",
                  ifelse(!is.null(species), toupper(species), ""),
                  ifelse(!is.null(equation_type), paste("- TYPE:", toupper(equation_type)), "")))
    }
    cat(paste(rep("=", 60), collapse = ""), "\n")
    print(result)
    cat(paste(rep("=", 60), collapse = ""), "\n")

    if (plot) {
      message("For better visual tables, install: install.packages(c('knitr', 'kableExtra')) or install.packages('gt')")
    }
  }

  # Return data invisibly for further use
  return(invisible(result))
}

# Shorter alias
species <- function(...) carbofor_species(...)

# Function to explore specific equation type
equation <- function(equation_type, ...) carbofor_species(equation_type = equation_type, ...)

# Quick list function
list_species <- function() {
  unique_species <- sort(unique(equations$Species))
  cat("Available species in carbofor:\n")
  cat(paste(seq_along(unique_species), unique_species, sep = ". ", collapse = "\n"))
  cat(sprintf("\n\nTotal: %d species\n", length(unique_species)))
  return(invisible(unique_species))
}

# Function to list available equation types
list_equation_types <- function() {
  unique_types <- sort(unique(equations$Y))
  cat("Available equation types in carbofor:\n")
  cat(paste(seq_along(unique_types), unique_types, sep = ". ", collapse = "\n"))
  cat(sprintf("\n\nTotal: %d equation types\n", length(unique_types)))
  return(invisible(unique_types))
}

# Usage examples:
#
# # List all species and their equations (with visual table)
# carbofor_species()
#
# # Same but shorter
# species()
#
# # Get equations for specific species
# carbofor_species("Fagus sylvatica")
# species("Picea abies")
#
# # Get equations for specific equation type (e.g., "V22", "biomass")
# carbofor_species(equation_type = "V22")
# equation("V22")
#
# # Combine filters: specific species AND equation type
# carbofor_species("Fagus sylvatica", equation_type = "V22")
#
# # Just list species names
# list_species()
#
# # Just list equation types
# list_equation_types()
#
# # Get data without visual formatting
# data <- carbofor_species(plot = FALSE)
#
# # Store result for further analysis
# my_species_data <- carbofor_species()
