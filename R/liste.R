# =====================================================================
#                         Build_equations()
# =====================================================================
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
#' build_equation(
#'   x_cols = c("1", "DBH", "DBH^2"),
#'   b_cols = c(1.5, 0.02, -0.001),
#'   y_var  = "Volume"
#' )
#' # Returns: "Volume = 1.5000 + 0.0200*DBH - 0.0010*DBH^2"
#' }
#' @keywords internal


# Helper function to build equation string
# R/build_equation.R
build_equation <- function(x_cols, b_cols, y_var) {
  # a term is valid if coef is finite & non-zero, var is not NA, and var != "0"
  valid_terms <- !is.na(b_cols) & b_cols != 0 & !is.na(x_cols) & x_cols != "0"

  if (!any(valid_terms)) {
    return("No valid equation")
  }

  terms <- character(0)
  first <- TRUE

  for (i in seq_along(b_cols)) {
    if (!valid_terms[i]) next

    coef <- b_cols[i]
    var  <- x_cols[i]

    if (first) {
      # first valid term: no leading sign
      if (var == "1" || var == "") {
        terms <- c(terms, sprintf("%.4f", coef))
      } else {
        terms <- c(terms, sprintf("%.4f*%s", coef, var))
      }
      first <- FALSE
    } else {
      sign <- ifelse(coef >= 0, " + ", " - ")
      abs_coef <- abs(coef)
      if (var == "1" || var == "") {
        terms <- c(terms, sprintf("%s%.4f", sign, abs_coef))
      } else {
        terms <- c(terms, sprintf("%s%.4f*%s", sign, abs_coef, var))
      }
    }
  }

  paste0(y_var, " = ", paste(terms, collapse = ""))
}

# =====================================================================
#                         helpers (internal)
# =====================================================================

#' Ensure columns exist (helper)
#' @keywords internal
.ensure_cols <- function(df, cols) {
  for (nm in cols) {
    if (!nm %in% names(df)) df[[nm]] <- NA_character_
  }
  df
}

# =====================================================================
#                         carbofor_species()
# =====================================================================

#' Explore and display allometric equations by species and source
#'
#' @description
#' `carbofor_species()` explores the package allometric database (`equations`)
#' and returns a structured table of equations for each tree species.
#' It supports two display modes:
#' \itemize{
#'   \item \strong{Summary mode} (default; when both `species` and `equation_type` are `NULL`): one row per \emph{Latin} species name \eqn{\times} source, with the column \emph{Available equations} listing all equation types for that source. Cells for repeated \emph{Species}, \emph{Code}, and \emph{Abreviation} are visually merged when HTML tables are enabled.
#'   \item \strong{Detailed mode} (when `species` and/or `equation_type` is provided): one row per equation with the fully formatted mathematical expression (built via `build_equation()`).
#' }
#'
#' The output column \code{Species} always displays the \emph{Latin} name. Practical
#' helpers (HTML styling, borders, merged cells) are applied when
#' \pkg{knitr} + \pkg{kableExtra} or \pkg{gt} are available; otherwise a clean
#' console fallback is used.
#'
#' @param species Character or `NULL`. Human-readable species name used in the
#'   \code{equations$Species} column (e.g., `"Hetre"`, `"Chene sessile"`). If `NULL`,
#'   no species filter is applied (summary mode).
#' @param equation_type Character or `NULL`. Equation type to filter on
#'   (e.g., `"V22"`, `"biomass"`, `"height"`), matched against \code{equations$Y}.
#'   If `NULL`, no equation-type filter is applied.
#' @param plot Logical. If `TRUE` (default), attempts to render a formatted
#'   table (HTML with \pkg{kableExtra}, or \pkg{gt}); if those packages are not
#'   available, prints a console table. If `FALSE`, simply returns the data.
#'
#' @return
#' Invisibly returns a \code{data.frame}. The schema depends on the mode:
#' \describe{
#'   \item{Summary mode}{Columns: \code{Species} (Latin), \code{Code}, \code{Abreviation},
#'   \code{Source}, \code{Available equations}. There is one row per Latin species
#'   \eqn{\times} source.}
#'   \item{Detailed mode}{Columns: \code{Species} (Latin), \code{Code},
#'   \code{Abreviation}, \code{Source}, \code{Equation_ID}, \code{Volume_Type},
#'   \code{Equation}. There is one row per equation.}
#' }
#'
#' @details
#' This function expects a data frame named \code{equations} available in scope
#' with columns at least: \code{Species}, \code{Latin}, \code{Code}, \code{Abr},
#' \code{Y}, \code{A0}, \code{Source_Eq}, \code{X0..X5}, \code{b0..b5}.
#' Detailed equations are built with \code{\link{build_equation}}.
#'
#' If \pkg{knitr} and \pkg{kableExtra} are available, header styling and cell merging
#' (\code{kableExtra::collapse_rows}) are applied. With \pkg{gt}, a styled table
#' is produced; otherwise a console print is shown.
#'
#' @section Notes:
#' \itemize{
#'   \item Species without \code{Code} or \code{Abr} are kept with \code{NA} values.
#'   \item Filtering by \code{species} matches \code{equations$Species} (human label),
#'         while the output \code{Species} column always shows the Latin name.
#' }
#'
#' @seealso \code{\link{build_equation}}, \code{\link{list_species}}, \code{\link{list_equation_types}}
#'
#' @examples
#' \dontrun{
#' equations <- data.frame(
#'   Species   = c("Hetre", "Hetre", "Epicea commun"),
#'   Latin     = c("Fagus sylvatica", "Fagus sylvatica", "Picea abies"),
#'   Code      = c("FASY", "FASY", "PIAB"),
#'   Abr       = c("HETR", "HETR", "EPIC"),
#'   Y         = c("V22", "biomass", "V22"),
#'   A0        = c(1, 2, 3),
#'   Source_Eq = c("Dagnelie", "Vallet", "Dagnelie"),
#'   X0 = c("1", "1", "1"), X1 = c("DBH", "DBH", "DBH"),
#'   b0 = c(1.5, 2.0, 0.8), b1 = c(0.02, 0.03, 0.05),
#'   stringsAsFactors = FALSE
#' )
#' carbofor_species(plot = FALSE)                         # summary mode
#' carbofor_species(species = "Hetre", plot = FALSE)      # detailed by species
#' carbofor_species(equation_type = "V22", plot = FALSE)  # detailed by type
#' }
#'
#' @export
#' @importFrom dplyr filter rowwise mutate ungroup select rename arrange group_by summarise %>%
#' @importFrom stats aggregate median qt quantile sd
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec collapse_rows
#' @importFrom gt gt tab_header tab_style cell_fill cell_text cells_column_labels cells_body tab_options
#'
carbofor_species <- function(species = NULL, equation_type = NULL, plot = TRUE) {
  Species <- Latin <- Code <- Abr <- Y <- X0 <- X1 <- X2 <- X3 <- X4 <- X5 <- NULL
  b0 <- b1 <- b2 <- b3 <- b4 <- b5 <- A0 <- Source_Eq <- NULL
  Equation <- Equation_ID <- Volume_Type <- Source <- NULL

  if (!exists("equations", inherits = TRUE)) {
    stop("`equations` data frame not found. Load it before calling carbofor_species().")
  }

  if (!"Latin" %in% names(equations)) equations$Latin <- NA_character_
  if (!"Code"  %in% names(equations)) equations$Code  <- NA_character_
  if (!"Abr"   %in% names(equations)) equations$Abr   <- NA_character_
  if (!"Source_Eq" %in% names(equations)) equations$Source_Eq <- NA_character_

  filtered_data <- equations

  if (!is.null(species)) {
    if (!"Species" %in% names(filtered_data)) {
      stop("Error: the column 'Species' is missing in `equations`.")
    }
    filtered_data <- dplyr::filter(filtered_data, Species == species)

    if (nrow(filtered_data) == 0) {
      cat(sprintf("No equations found for species: %s\n", species))
      cat("Available species:\n")
      available <- unique(equations$Species)
      cat(paste(available, collapse = ", "))
      return(invisible(
        data.frame(
          Species = character(),
          Code = character(),
          Abreviation = character(),
          Source = character(),
          `Available equations` = character()
        )
      ))
    }
  }

  if (!is.null(equation_type)) {
    if (!"Y" %in% names(equations)) {
      stop("Error: the column 'Y' (equation type) is missing in `equations`.")
    }
    filtered_data <- dplyr::filter(filtered_data, Y == equation_type)

    if (nrow(filtered_data) == 0) {
      cat(sprintf("No equations found for equation type: %s\n", equation_type))
      cat("Available equation types:\n")
      available <- unique(equations$Y)
      cat(paste(available, collapse = ", "))
      return(invisible(
        data.frame(
          Species = character(),
          Code = character(),
          Abreviation = character(),
          Source = character(),
          `Available equations` = character()
        )
      ))
    }
  }

  if (!is.null(species) || !is.null(equation_type)) {
    result <- filtered_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        Equation = build_equation(
          x_cols = c(X0, X1, X2, X3, X4, X5),
          b_cols = c(b0, b1, b2, b3, b4, b5),
          y_var  = Y
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        Species = Latin,
        Code,
        Abreviation = Abr,
        Source = Source_Eq,
        Equation_ID = A0,
        Volume_Type = Y,
        Equation
      ) %>%
      dplyr::arrange(Species, Source, Equation_ID)
  } else {
    result <- equations %>%
      dplyr::group_by(Species = Latin, Code, Abreviation = Abr, Source = Source_Eq) %>%
      dplyr::summarise(
        `Available equations` = paste(sort(unique(Y)), collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Species, Source)
  }

  if (!plot) return(invisible(result))

  if (requireNamespace("knitr", quietly = TRUE) &&
      requireNamespace("kableExtra", quietly = TRUE)) {

    tb <- result %>%
      knitr::kable(
        format = "html",
        caption = ifelse(is.null(species) && is.null(equation_type),
                         "Available Species and Equations in carbofor (by source)",
                         paste("Equations for",
                               ifelse(!is.null(species), species, ""),
                               ifelse(!is.null(equation_type),
                                      paste("- Type:", equation_type), ""))),
        row.names = FALSE
      ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE, font_size = 12
      ) %>%
      kableExtra::row_spec(0, background = "#d9edf7", bold = TRUE, color = "black") %>%
      kableExtra::column_spec(1:ncol(result), border_left = TRUE, border_right = TRUE)

    tb <- tb %>% kableExtra::collapse_rows(columns = 1:3, valign = "top")

    print(tb)

  } else if (requireNamespace("gt", quietly = TRUE)) {

    tbl <- result %>%
      gt::gt(groupname_col = "Species") %>%
      gt::tab_header(
        title = ifelse(is.null(species) && is.null(equation_type),
                       "Available Species and Equations in carbofor (by source)",
                       "Equations (grouped by species)")
      ) %>%
      gt::tab_style(
        style = list(gt::cell_fill(color = "#d9edf7"), gt::cell_text(weight = "bold")),
        locations = gt::cells_column_labels(gt::everything())
      ) %>%
      gt::tab_options(table.border.top.color = "black",
                      table.border.bottom.color = "black",
                      table_body.border.color = "grey")

    print(tbl)

  } else {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
    if (is.null(species) && is.null(equation_type)) {
      cat("AVAILABLE SPECIES AND EQUATIONS IN CARBOFOR (BY SOURCE)\n")
    } else {
      cat(sprintf("EQUATIONS FOR %s %s\n",
                  ifelse(!is.null(species), toupper(species), ""),
                  ifelse(!is.null(equation_type),
                         paste("- TYPE:", toupper(equation_type)), "")))
    }
    cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
    print(result)
    cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
  }

  invisible(result)
}



# =====================================================================
#                            species() alias
# =====================================================================

#' Species — short alias for `carbofor_species()`
#'
#' @description
#' `species()` is a convenience alias to `carbofor_species()` for interactive use.
#' It accepts the same arguments and returns the same output; the only difference
#' is the shorter function name.
#'
#' @param ... Arguments passed through to [carbofor_species()].
#'
#' @return
#' The same data frame (invisibly) as returned by [carbofor_species()].
#'
#' @details
#' This function is strictly a wrapper. See [carbofor_species()] for details on
#' input columns expected in the `equations` data frame, output schema (summary
#' vs detailed modes), and display behavior.
#'
#' @examples
#' \dontrun{
#' # Minimal demo data
#' equations <- data.frame(
#'   Species   = c("Hetre", "Hetre", "Epicea commun"),
#'   Latin     = c("Fagus sylvatica", "Fagus sylvatica", "Picea abies"),
#'   Code      = c("3", "3", "41"),
#'   Abr       = c("HE", "HE", "EP"),
#'   Y         = c("V22", "biomass", "V22"),
#'   A0        = c(1, 2, 3),
#'   Source_Eq = c("Dagnelie", "Vallet", "Dagnelie"),
#'   X0 = "1", X1 = "DBH",
#'   b0 = c(1.5, 2.0, 0.8), b1 = c(0.02, 0.03, 0.05),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Summary (one row per Latin species x source)
#' species(plot = FALSE)
#'
#' # Detailed by species (match on `equations$Species`)
#' species("Hetre", plot = FALSE)
#'
#' # Detailed by equation type
#' species(equation_type = "V22", plot = FALSE)
#' }
#'
#' @seealso [carbofor_species()]
#' @export
species <- function(...) carbofor_species(...)

# =====================================================================
#                          equation() helper
# =====================================================================

#' Filter equations by type (wrapper)
#'
#' @description
#' `equation()` is a thin wrapper around [carbofor_species()] that presets the
#' `equation_type` argument. Use it to quickly list or display all equations of
#' a given type across species.
#'
#' @param equation_type Character scalar. Equation type to filter on
#'   (e.g., `"V22"`, `"biomass"`, `"height"`). Must match values in `equations$Y`.
#' @param ... Additional arguments passed to [carbofor_species()] (e.g., `plot = FALSE`).
#'
#' @return
#' The same data frame (invisibly) as returned by [carbofor_species()].
#'
#' @examples
#' \dontrun{
#' # Minimal demo data
#' equations <- data.frame(
#'   Species   = c("Hetre", "Chene sessile"),
#'   Latin     = c("Fagus sylvatica", "Quercus rubra"),
#'   Code      = c("3", "41"),
#'   Abr       = c("HE", "CHS"),
#'   Y         = c("V22", "V22"),
#'   A0        = c(1, 2),
#'   Source_Eq = c("Dagnelie", "Vallet"),
#'   X0 = "1", X1 = "DBH",
#'   b0 = c(1.2, 0.9), b1 = c(0.03, 0.04),
#'   stringsAsFactors = FALSE
#' )
#'
#' # All V22 equations across species (summary by source)
#' equation("V22", plot = FALSE)
#'
#' # Same as:
#' carbofor_species(equation_type = "V22", plot = FALSE)
#' }
#'
#' @seealso [carbofor_species()]
#' @export
equation <- function(equation_type, ...) {
  carbofor_species(equation_type = equation_type, ...)
}

# =====================================================================
#                          list_species()
# =====================================================================

#' List available species (human-readable labels)
#'
#' @description
#' Prints and returns the set of unique species labels found in `equations$Species`
#' (the human-readable/common label). Useful to discover valid values for the
#' `species` filter of [carbofor_species()].
#'
#' @return
#' Invisibly returns a character vector of unique species labels, sorted
#' alphabetically.
#'
#' @examples
#' \dontrun{
#' equations <- data.frame(
#'   Species = c("Hetre", "Chene sessile", "Hetre"),
#'   Y = "V22"
#' )
#'
#' # Print a numbered list and get the vector back
#' sp <- list_species()
#' sp
#'
#' # Quick existence check before filtering
#' "Hetre" %in% list_species()
#' }
#'
#' @seealso [list_equation_types()], [carbofor_species()]
#' @export
list_species <- function() {
  if (!exists("equations", inherits = TRUE)) {
    stop("`equations` data frame not found.")
  }
  unique_species <- sort(unique(equations$Species))
  cat("Available species in carbofor:\n")
  cat(paste(seq_along(unique_species), unique_species, sep = ". ", collapse = "\n"))
  cat(sprintf("\n\nTotal: %d species\n", length(unique_species)))
  invisible(unique_species)
}

# =====================================================================
#                      list_equation_types()
# =====================================================================

#' List available equation types
#'
#' @description
#' Prints and returns the set of unique equation types (values of `equations$Y`).
#' Useful to discover valid values for the `equation_type` filter of
#' [carbofor_species()].
#'
#' @return
#' Invisibly returns a character vector of unique equation types, sorted
#' alphabetically.
#'
#' @examples
#' \dontrun{
#' equations <- data.frame(
#'   Y = c("V22", "biomass", "V22", "height")
#' )
#'
#' # Print a numbered list and get the types vector
#' types <- list_equation_types()
#' types
#'
#' # Sanity checks
#' "V22" %in% list_equation_types()
#' "biomass" %in% list_equation_types()
#' }
#'
#' @seealso [list_species()], [carbofor_species()]
#' @export
list_equation_types <- function() {
  if (!exists("equations", inherits = TRUE)) {
    stop("`equations` data frame not found.")
  }
  unique_types <- sort(unique(equations$Y))
  cat("Available equation types in carbofor:\n")
  cat(paste(seq_along(unique_types), unique_types, sep = ". ", collapse = "\n"))
  cat(sprintf("\n\nTotal: %d equation types\n", length(unique_types)))
  invisible(unique_types)
}

# =====================================================================
#                      list_sources()
# =====================================================================

#' List available equation sources (with optional bibliography)
#'
#' @description
#' Prints and returns the set of unique sources found in `equations$Source_Eq`.
#' When `ref = TRUE` (default) **and** a `Reference` column is present, the
#' function also prints and returns the bibliographic reference(s) associated to
#' each source (collapsed if multiple).
#'
#' @param ref Logical, default `TRUE`. If `TRUE` and a `Reference` column exists,
#'   include bibliography in the printed output and in the returned object.
#'
#' @return
#' Invisibly returns either:
#' - a **data frame** with columns `Source` and `Reference` (when `ref = TRUE`
#'   and a `Reference` column exists), or
#' - a **character vector** of unique sources (otherwise).
#'
#' @details
#' - If `Reference` exists and there are multiple distinct references per
#'   `Source_Eq`, they are collapsed with `" | "`.
#' - If `Reference` is missing for some sources, `NA` is shown/returned for
#'   those entries.
#' - The printed output is numbered and ends with a total count line.
#'
#' @examples
#' \dontrun{
#' # List sources only
#' list_sources(ref = FALSE)
#'
#' # List sources with bibliography (if available)
#' list_sources()
#' }
#'
#' @seealso [carbofor_species()], [list_species()], [list_equation_types()]
#' @export
list_sources <- function(ref = TRUE) {
  if (!exists("equations", inherits = TRUE)) {
    stop("`equations` data frame not found.")
  }
  if (!"Source_Eq" %in% names(equations)) {
    stop("`equations` has no 'Source_Eq' column.")
  }

  # Extract sources
  src <- equations[["Source_Eq"]]
  src <- src[!is.na(src)]
  if (length(src) == 0L) {
    cat("No sources found in 'equations'.\n")
    if (ref && "Reference" %in% names(equations)) {
      return(invisible(data.frame(Source = character(), Reference = character(),
                                  stringsAsFactors = FALSE)))
    } else {
      return(invisible(character()))
    }
  }

  has_ref <- ref && ("Reference" %in% names(equations))

  if (has_ref) {
    # Build Source -> collapsed Reference mapping
    df <- equations[, intersect(c("Source_Eq", "Reference"), names(equations)), drop = FALSE]
    df$Source_Eq[is.na(df$Source_Eq)] <- NA_character_
    # split and collapse unique non-NA references per source
    sp <- split(seq_len(nrow(df)), df$Source_Eq)
    collapse_one <- function(idx) {
      r <- df$Reference[idx]
      r <- unique(r[!is.na(r) & nzchar(as.character(r))])
      if (length(r) == 0L) NA_character_ else paste(r, collapse = " | ")
    }
    refs <- vapply(sp, collapse_one, FUN.VALUE = character(1), USE.NAMES = TRUE)

    res <- data.frame(
      Source    = names(refs),
      Reference = unname(refs),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    # sort by Source
    ord <- order(tolower(res$Source))
    res <- res[ord, , drop = FALSE]

    # Printed output
    cat("Available sources in carbofor:\n")
    if (nrow(res) > 0) {
      lines <- ifelse(is.na(res$Reference) | !nzchar(res$Reference),
                      paste(seq_len(nrow(res)), res$Source, sep = ". "),
                      paste0(seq_len(nrow(res)), ". ", res$Source, " — ", res$Reference))
      cat(paste(lines, collapse = "\n"))
      cat(sprintf("\n\nTotal: %d sources\n", nrow(res)))
    } else {
      cat("(none)\n\nTotal: 0 sources\n")
    }
    return(invisible(res))

  } else {
    # Sources only (character vector)
    vec <- sort(unique(src))
    cat("Available sources in carbofor:\n")
    if (length(vec) > 0) {
      cat(paste(seq_along(vec), vec, sep = ". ", collapse = "\n"))
      cat(sprintf("\n\nTotal: %d sources\n", length(vec)))
    } else {
      cat("(none)\n\nTotal: 0 sources\n")
    }
    return(invisible(vec))
  }
}
