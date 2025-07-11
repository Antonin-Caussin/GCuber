% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{validate_parameters}
\alias{validate_parameters}
\title{Validate Input Parameters for Allometric Volume Calculation}
\usage{
validate_parameters(
  x,
  volume_type = "V22",
  equation_id = 1,
  source = "Dagnellie",
  specimens = NULL,
  C130 = "C130",
  C150 = "C150",
  D130 = "D130",
  D150 = "D150",
  HTOT = "HTOT",
  HDOM = "HDOM"
)
}
\arguments{
\item{x}{A `data.frame` containing the tree-level inventory data. Must include
at least one column related to diameter or circumference at a defined height.}

\item{volume_type}{Character. Type of volume to be computed.
Accepted values include:
\itemize{
  \item \code{"V22"}: Commercial volume over bark from 0 to 22 cm diameter.
  \item \code{"V22B"}: Commercial volume under bark from 0 to 22 cm diameter.
  \item \code{"V22_HA"}: Commercial volume over bark extrapolated to hectare scale.
  \item \code{"Sawlog"}: Sawlog volume.
  \item \code{"Aboveground"}: Total aboveground volume.
  \item \code{"Merchantable"}: Merchantable volume.
}}

\item{equation_id}{Integer. Identifier of the equation to use (typically from an internal database).
Some volume types only accept specific values for this argument.}

\item{source}{Character. Source of the allometric equations.
Must be one of: \code{"Dagnellie"}, \code{"Algan"}, \code{"Vallet"}, \code("Bouvard"), \code("Rondeu") or \code("Courbet")}

\item{specimens}{Optional character. Name of the column containing species identifiers
(codes, abbreviations, or full names), used to select the appropriate model.}

\item{C130, C150}{Character. Names of the columns containing circumference at 130 cm
and 150 cm above ground, respectively (in cm).}

\item{D130, D150}{Character. Names of the columns containing diameter at 130 cm
and 150 cm above ground, respectively (in cm).}

\item{HTOT}{Character. Name of the column with total tree height (in meters).}

\item{HDOM}{Character. Name of the column with dominant height, used for plot-level scaling (in meters).}
}
\value{
The function returns the input `data.frame` invisibly, after validating the parameters.
It throws an error if a parameter is invalid (e.g., unknown volume type or source),
and issues a warning if some required columns are missing from the dataset.
}
\description{
This function checks the validity of the input arguments provided to functions
using allometric equations (e.g., for volume, biomass, or carbon estimations).
It ensures that:
- The specified `source` and `volume_type` are among the accepted values.
- The selected `equation_id` is allowed for the given `volume_type`.
- All required columns (diameter, height, species) are present in the input data.

The function is primarily used internally before launching model predictions,
to catch user errors and provide informative messages.
}
\details{
This function does not modify the dataset. It is intended to be used as a safeguard
before applying volume, biomass, or carbon prediction models. At least one of the
following columns must be present in `x`: `C130`, `C150`, `D130`, or `D150`.
}
\examples{
\dontrun{
# Minimal working example
df <- data.frame(
  C130 = c(100, 120),
  D130 = c(31.8, 38.2),
  HTOT = c(20, 25),
  HDOM = c(22, 26),
  Species = c("FASY", "PCSY")
)

validate_parameters(
  x = df,
  volume_type = "V22",
  equation_id = 1,
  source = "Dagnellie",
  specimens = "Species"
)
}

}
\seealso{
\code{\link{calculate_volumes}}, \code{\link{carbofor_species}},
\code{\link{calculate_prediction_interval}} for downstream use.
}
