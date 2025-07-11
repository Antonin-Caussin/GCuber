% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/liste.R
\name{carbofor_species}
\alias{carbofor_species}
\title{Explore and Display Forest Allometric Equations}
\usage{
carbofor_species(species = NULL, equation_type = NULL, plot = TRUE)
}
\arguments{
\item{species}{Character string. Name of the tree species to filter for.
Must match exactly the species names in the database. Use \code{list_species()}
to see available species. Default is \code{NULL} (no species filter).}

\item{equation_type}{Character string. Type of equation to filter for
(e.g., "V22", "biomass", "height"). Must match exactly the equation types
in the Y column of the database. Use \code{list_equation_types()} to see
available types. Default is \code{NULL} (no equation type filter).}

\item{plot}{Logical. Whether to display formatted tables. If \code{TRUE}
(default), attempts to create formatted HTML tables using knitr/kableExtra
or gt packages. If these packages are not available, falls back to console
output. If \code{FALSE}, returns data without visual formatting.}
}
\value{
Invisibly returns a data frame containing the filtered results:
  \describe{
    \item{Summary mode (no filters)}{Data frame with columns: Species,
          Available_equations, Sources}
    \item{Detailed mode (with filters)}{Data frame with columns: Species,
          Equation_ID, Volume_Type, Source, Equation}
  }
}
\description{
This function provides an interface to explore forest allometric equations
from the carbofor database. It allows filtering by species and/or equation type,
and presents results in formatted tables with complete equation formulas.
}
\details{
The function operates in two modes:
\describe{
  \item{Summary mode}{When no filters are applied, shows overview of all
        available species with their equation types and sources}
  \item{Detailed mode}{When filters are applied, shows complete equations
        with coefficients formatted as mathematical expressions}
}

Equations are built using the \code{build_equation()} helper function, which
formats coefficients and variables into readable mathematical expressions.
Only non-zero, non-NA coefficients are included in the final equations.

The function supports visual output through multiple table formatting packages:
\describe{
  \item{Primary}{knitr + kableExtra (HTML tables with Bootstrap styling)}
  \item{Alternative}{gt package (Grammar of Tables)}
  \item{Fallback}{Console output with formatted text}
}
}
\section{Database Structure}{

The function expects a data frame named \code{equations} with the following columns:
\describe{
  \item{Species}{Species names}
  \item{A0}{Equation IDs}
  \item{Y}{Equation types/response variables}
  \item{Source_Eq}{Source references}
  \item{X0, X1, X2, X3, X4, X5}{Predictor variable names}
  \item{b0, b1, b2, b3, b4, b5}{Equation coefficients}
}
}

\examples{
\dontrun{
# Load the package and ensure equations data is available
library(carbofor)
data(equations)  # Assuming the data is included in the package

# Display all available species and equation types
carbofor_species()

# Get equations for a specific species
carbofor_species("Hetre")
carbofor_species("Epicea commun")

# Get equations of a specific type
carbofor_species(equation_type = "V22")
carbofor_species(equation_type = "biomass")

# Combine filters: specific species and equation type
carbofor_species("Hetre", equation_type = "V22")

# Get data without visual formatting for further analysis
data <- carbofor_species(plot = FALSE)
equations_v22 <- carbofor_species(equation_type = "V22", plot = FALSE)

# Store results for further analysis
hetre_data <- carbofor_species("Hetre", plot = FALSE)
str(hetre_data)
}

}
\seealso{
\code{\link{list_species}} for listing available species names,
\code{\link{list_equation_types}} for listing available equation types,
\code{\link{species}} for the shorter function alias,
\code{\link{equation}} for filtering by equation type only
}
\author{
[Your Name]
}
