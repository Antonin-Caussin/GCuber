% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/liste.R
\name{build_equation}
\alias{build_equation}
\title{Build Equation String from Coefficients and Variables}
\usage{
build_equation(x_cols, b_cols, y_var)
}
\arguments{
\item{x_cols}{Character vector of variable names (X0, X1, X2, etc.)}

\item{b_cols}{Numeric vector of coefficients (b0, b1, b2, etc.)}

\item{y_var}{Character string for the response variable name}
}
\value{
Character string representing the complete equation
}
\description{
This helper function constructs a readable mathematical equation string
from coefficient and variable vectors. It formats coefficients, handles
signs properly, and excludes zero or missing terms.
}
\details{
The function processes coefficient and variable pairs to create a properly
formatted equation string. Only non-zero, non-NA coefficients with valid
variable names are included. The first term is handled differently from
subsequent terms to manage signs correctly.
}
\examples{
\dontrun{
# Internal function - not exported
build_equation(c("1", "DBH", "DBH^2"), c(1.5, 0.02, -0.001), "Volume")
# Returns: "Volume = 1.5000 + 0.0200*DBH - 0.0010*DBH^2"
}

}
\keyword{internal}
