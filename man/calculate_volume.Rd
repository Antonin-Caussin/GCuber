% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calculate_volume}
\alias{calculate_volume}
\title{Volume calculation from allometric equations}
\usage{
calculate_volume(
  x,
  volume_type = "V22",
  equations,
  equation_id = 1,
  D130 = "D130",
  specimens = NULL,
  source = "Dagnellie"
)
}
\arguments{
\item{x}{A data.frame containing individual tree data (must include at least Species and diameter column)}

\item{volume_type}{Type of volume to calculate (e.g., "V22", "V22B", "V22_HA", "E")}

\item{equations}{A data.frame of allometric equations including: Species, A0, b0 to b5, X1 to X5, D_Min, D_Max, Y}

\item{equation_id}{Index of the equation to use for each species (default is 1)}

\item{D130}{Name of the diameter variable to use (default = "D130")}

\item{specimens}{A vector of specimen codes or identifiers, used to map equations to observations}

\item{source}{Source of the equations (e.g., "Dagnelie", "Myers")}
}
\value{
The data.frame enriched with columns: [volume_type], Validity_Status, and Equation_Used
}
\description{
Applies species-specific allometric equations to calculate volume.
}
\examples{
trees <- data.frame(Species = "Fagus_sylvatica", D130 = 32)
specimens <- c("FASY001")
result <- calculate_volume(trees, equations = equations, specimens = specimens)
}
