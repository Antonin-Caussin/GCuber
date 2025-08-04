#' Build Equation String from Coefficients and Variables
#'
#' This helper function constructs a readable mathematical equation string
#' from coefficient and variable vectors. It formats coefficients, handles
#' signs properly, and excludes zero or missing terms.
#'
#' @param x_cols Character vector of variable names (X0, X1, X2, etc.)
#' @param b_cols Numeric vector of coefficients (b0, b1, b2, etc.)
#' @param y_var Character string for the response variable name
#'
#' @return Character string representing the complete equation
#'
#' @details
#' The function processes coefficient and variable pairs to create a properly
#' formatted equation string. Only non-zero, non-NA coefficients with valid
#' variable names are included. The first term is handled differently from
#' subsequent terms to manage signs correctly.
#'
#' @examples
#' \dontrun{
#' # Internal function - not exported
#' build_equation(c("1", "DBH", "DBH^2"), c(1.5, 0.02, -0.001), "Volume")
#' # Returns: "Volume = 1.5000 + 0.0200*DBH - 0.0010*DBH^2"
#' }
#'
#' @keywords internal


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
#'   \describe{
#'     \item{Summary mode (no filters)}{Data frame with columns: Species,
#'           Available_equations, Sources}
#'     \item{Detailed mode (with filters)}{Data frame with columns: Species,
#'           Equation_ID, Volume_Type, Source, Equation}
#'   }
#'
#' @details
#' The function operates in two modes:
#' \describe{
#'   \item{Summary mode}{When no filters are applied, shows overview of all
#'         available species with their equation types and sources}
#'   \item{Detailed mode}{When filters are applied, shows complete equations
#'         with coefficients formatted as mathematical expressions}
#' }
#'
#' Equations are built using the \code{build_equation()} helper function, which
#' formats coefficients and variables into readable mathematical expressions.
#' Only non-zero, non-NA coefficients are included in the final equations.
#'
#' The function supports visual output through multiple table formatting packages:
#' \describe{
#'   \item{Primary}{knitr + kableExtra (HTML tables with Bootstrap styling)}
#'   \item{Alternative}{gt package (Grammar of Tables)}
#'   \item{Fallback}{Console output with formatted text}
#' }
#'
#' @section Database Structure:
#' The function expects a data frame named \code{equations} with the following columns:
#' \describe{
#'   \item{Species}{Species names}
#'   \item{A0}{Equation IDs}
#'   \item{Y}{Equation types/response variables}
#'   \item{Source_Eq}{Source references}
#'   \item{X0, X1, X2, X3, X4, X5}{Predictor variable names}
#'   \item{b0, b1, b2, b3, b4, b5}{Equation coefficients}
#' }
#'
#' @examples
#' \dontrun{
#' # Load the package and ensure equations data is available
#' library(carbofor)
#' data(equations)  # Assuming the data is included in the package
#'
#' # Display all available species and equation types
#' carbofor_species()
#'
#' # Get equations for a specific species
#' carbofor_species("Hetre")
#' carbofor_species("Epicea commun")
#'
#' # Get equations of a specific type
#' carbofor_species(equation_type = "V22")
#' carbofor_species(equation_type = "biomass")
#'
#' # Combine filters: specific species and equation type
#' carbofor_species("Hetre", equation_type = "V22")
#'
#' # Get data without visual formatting for further analysis
#' data <- carbofor_species(plot = FALSE)
#' equations_v22 <- carbofor_species(equation_type = "V22", plot = FALSE)
#'
#' # Store results for further analysis
#' hetre_data <- carbofor_species("Hetre", plot = FALSE)
#' str(hetre_data)
#' }
#'
#' @seealso
#' \code{\link{list_species}} for listing available species names,
#' \code{\link{list_equation_types}} for listing available equation types,
#' \code{\link{species}} for the shorter function alias,
#' \code{\link{equation}} for filtering by equation type only
#'
#' @author [Your Name]
#' @export
#' @importFrom dplyr filter rowwise mutate ungroup select rename arrange group_by summarise %>%
#' @importFrom stats aggregate median qt quantile sd
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec
#' @importFrom gt gt tab_header tab_style cell_fill cell_text cells_column_labels cells_body

# Main function with short, evocative name
carbofor_species <- function(species = NULL, equation_type = NULL, plot = TRUE) {

  # Suppress R CMD check warnings about global variables
  Species <- Y <- X0 <- X1 <- X2 <- X3 <- X4 <- X5 <- NULL
  b0 <- b1 <- b2 <- b3 <- b4 <- b5 <- A0 <- Source_Eq <- NULL
  Equation <- Equation_ID <- Volume_Type <- Source <- NULL

  # Start with full dataset
  filtered_data <- equations

  if (!is.null(species)) {
    if (!"Species" %in% names(equations)) {
      stop("Error : the column 'Species' est missing in the dataset `equations`.")
    }

    filtered_data <- filtered_data %>%
      filter(Species == species)

    if (nrow(filtered_data) == 0) {
      cat(sprintf("No equations found for species: %s\n", species))
      cat("Available species:\n")
      available <- unique(equations$Species)
      cat(paste(available, collapse = ", "))

      return(invisible(
        data.frame(
          Species = character(),
          Equation_ID = character(),
          Volume_Type = character(),
          Source = character(),
          Equation = character()
        )
      ))
    }
  }

  # Filter by equation type if specified
  if (!is.null(equation_type)) {
    if (!"Y" %in% names(equations)) {
      stop("Error : the column 'Y' (type d'equation) est missing in the dataset `equations`.")
    }

    filtered_data <- filtered_data %>%
      filter(Y == equation_type)

    if (nrow(filtered_data) == 0) {
      cat(sprintf("No equations found for equation type: %s\n", equation_type))
      cat("Available equation types:\n")
      available <- unique(equations$Y)
      cat(paste(available, collapse = ", "))

      return(invisible(
        data.frame(
          Species = character(),
          Equation_ID = character(),
          Volume_Type = character(),
          Source = character(),
          Equation = character()
        )
      ))
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
        `Available equations` = paste(unique(Y), collapse = ", "),
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

#' Species - Short Alias for carbofor_species
#'
#' This is a convenient short alias for the \code{carbofor_species()} function.
#' It provides the same functionality with a more concise function name for
#' interactive use.
#'
#' @param ... Arguments passed to \code{carbofor_species()}
#'
#' @return Same as \code{carbofor_species()}
#'
#' @details
#' This function is identical to \code{carbofor_species()} but with a shorter,
#' more convenient name for interactive analysis. All parameters and functionality
#' are the same.
#'
#' @examples
#' \dontrun{
#' # Using the shorter alias - same functionality
#' species("Quercus robur")
#' species(equation_type = "V22")
#' species()  # Show all species and equations
#'
#' # Equivalent to:
#' carbofor_species("Quercus robur")
#' carbofor_species(equation_type = "V22")
#' carbofor_species()
#' }
#'
#' @seealso \code{\link{carbofor_species}} for the full version of this function.
#' @export

species <- function(...) {
  carbofor_species(...)
}


# Shorter alias
species <- function(...) carbofor_species(...)

#' Filter Equations by Type
#'
#' This function provides a convenient way to filter equations by type only.
#' It's a wrapper around \code{carbofor_species()} with the equation_type
#' parameter pre-specified.
#'
#' @param equation_type Character string. Type of equation to filter for
#'   (e.g., "V22", "biomass", "height"). Must match exactly the equation types
#'   in the Y column of the database.
#' @param ... Additional arguments passed to \code{carbofor_species()}
#'
#' @return Same as \code{carbofor_species()}
#'
#' @details
#' This is a convenience function for users who want to explore all equations
#' of a specific type across all species. It's equivalent to calling
#' \code{carbofor_species(equation_type = equation_type, ...)}.
#'
#' @examples
#' \dontrun{
#' # Get all V22 equations across all species
#' equation("V22")
#'
#' # Get biomass equations without visual formatting
#' biomass_data <- equation("biomass", plot = FALSE)
#'
#' # Equivalent to:
#' carbofor_species(equation_type = "V22")
#' carbofor_species(equation_type = "biomass", plot = FALSE)
#' }
#'
#' @seealso \code{\link{carbofor_species}} for the main function documentation
#'
#' @export

# Function to explore specific equation type
equation <- function(equation_type, ...) carbofor_species(equation_type = equation_type, ...)



#' List Available Species
#'
#' This function displays all available tree species in the carbofor database
#' in a numbered, formatted list.
#'
#' @return Invisibly returns a character vector of unique species names,
#'   sorted alphabetically.
#'
#' @details
#' The function extracts unique species names from the equations dataset,
#' sorts them alphabetically, and displays them in a numbered list format
#' with a total count. This is useful for exploring available species before
#' filtering with \code{carbofor_species()}.
#'
#' @examples
#' \dontrun{
#' # Display all available species
#' list_species()
#'
#' # Store species list for programmatic use
#' available_species <- list_species()
#' head(available_species)
#' length(available_species)
#'
#' # Check if a specific species is available
#' "Hetre" %in% list_species()
#' }
#'
#' @seealso \code{\link{list_equation_types}} for listing equation types,
#'   \code{\link{carbofor_species}} for filtering by species
#'
#' @export

# Quick list function
list_species <- function() {
  unique_species <- sort(unique(equations$Species))
  cat("Available species in carbofor:\n")
  cat(paste(seq_along(unique_species), unique_species, sep = ". ", collapse = "\n"))
  cat(sprintf("\n\nTotal: %d species\n", length(unique_species)))
  return(invisible(unique_species))
}

#' List Available Equation Types
#'
#' This function displays all available equation types in the carbofor database
#' in a numbered, formatted list.
#'
#' @return Invisibly returns a character vector of unique equation types,
#'   sorted alphabetically.
#'
#' @details
#' The function extracts unique equation types (Y column values) from the
#' equations dataset, sorts them alphabetically, and displays them in a
#' numbered list format with a total count. This is useful for exploring
#' available equation types before filtering with \code{carbofor_species()}.
#'
#' Common equation types include:
#' \describe{
#'   \item{V22}{Volume calculations}
#'   \item{biomass}{Biomass estimations}
#'   \item{height}{Height predictions}
#'   \item{Other types}{Depending on the database content}
#' }
#'
#' @examples
#' \dontrun{
#' # Display all available equation types
#' list_equation_types()
#'
#' # Store equation types for programmatic use
#' available_types <- list_equation_types()
#' head(available_types)
#'
#' # Check if a specific equation type is available
#' "V22" %in% list_equation_types()
#' "biomass" %in% list_equation_types()
#' }
#'
#' @seealso \code{\link{list_species}} for listing species,
#'   \code{\link{carbofor_species}} for filtering by equation type
#'
#' @export

# Function to list available equation types
list_equation_types <- function() {
  unique_types <- sort(unique(equations$Y))
  cat("Available equation types in carbofor:\n")
  cat(paste(seq_along(unique_types), unique_types, sep = ". ", collapse = "\n"))
  cat(sprintf("\n\nTotal: %d equation types\n", length(unique_types)))
  return(invisible(unique_types))
}

# Suppress R CMD check warnings about global variables used in NSE (Non-Standard Evaluation)
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # Column names from equations dataset
    "Species", "Y", "A0", "Source_Eq",
    "X0", "X1", "X2", "X3", "X4", "X5",
    "b0", "b1", "b2", "b3", "b4", "b5",
    # Computed variables in dplyr pipelines
    "Equation", "Equation_ID", "Volume_Type", "Source",
    "Available_equations", "Sources",
    # Pipe operator
    "%>%"
  ))
}
