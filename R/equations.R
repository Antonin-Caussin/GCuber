#' Internal Allometric Equation Database
#'
#' @description
#' A dataset containing species-specific allometric models and associated metadata used
#' internally by the GCuber package for computing volume, biomass, and carbon in forest inventories.
#'
#' This dataset is used internally by GCuber functions such as \code{\link{calculate_volume}},
#' \code{\link{calculate_biomass}}, and \code{\link{convert_circumference}}.
#'
#'
#' @format A \code{data.frame} with \code{n} rows and the following variables:
#' \describe{
#'   \item{A0}{Equation indentification}
#'   \item{Abr}{Species abbreviation}
#'   \item{Anglais}{English name of the species}
#'   \item{C130_Max}{Maximum value for C130}
#'   \item{C130_Min}{Minimum value for C130}
#'   \item{Code}{Species code}
#'   \item{D130_Max}{Maximum value for D130}
#'   \item{D130_Min}{Minimum value for D130}
#'   \item{Equation}{Equation formula}
#'   \item{G130_Max}{Maximum value for G130}
#'   \item{G130_Min}{Minimum value for G130}
#'   \item{HDOM_Max}{Maximum value for HDOM}
#'   \item{HDOM_Min}{Minimum value for HDOM}
#'   \item{HTOT_Max}{Maximum value for HTOT}
#'   \item{HTOT_Min}{Minimum value for HTOT}
#'   \item{HV}{Wood volume}
#'   \item{H_Max}{Maximum observed height}
#'   \item{H_Min}{Minimum observed height}
#'   \item{ID}{Unique identifier}
#'   \item{IV}{Variation index}
#'   \item{Latin}{Latin name of the species}
#'   \item{NumEquation}{Equation number}
#'   \item{Reference}{Bibliographic reference}
#'   \item{Region}{Geographic region}
#'   \item{SCE_C130}{Standard error for C130}
#'   \item{SCE_D130}{Standard error for D130}
#'   \item{SCE_G130}{Standard error for G130}
#'   \item{SCE_H}{Standard error for H}
#'   \item{SCE_HDOM}{Standard error for HDOM}
#'   \item{SCE_HTOT}{Standard error for HTOT}
#'   \item{Source_Eq}{Equation source}
#'   \item{Source_ID}{Infra density source}
#'   \item{Species}{Species}
#'   \item{Type_Eq}{Equation type}
#'   \item{X0}{Coefficient X0}
#'   \item{X1}{Coefficient X1}
#'   \item{X2}{Coefficient X2}
#'   \item{X3}{Coefficient X3}
#'   \item{X4}{Coefficient X4}
#'   \item{X5}{Coefficient X5}
#'   \item{Y}{Dependent variable}
#'   \item{b0}{Standard error of b0}
#'   \item{b1}{Standard error of b1}
#'   \item{b2}{Standard error of b2}
#'   \item{b3}{Standard error of b3}
#'   \item{b4}{Standard error of b4}
#'   \item{b5}{Standard error of b5}
#'   \item{n}{Sample size}
#'   \item{sigma}{Residual standard deviation}
#'   \item{x_mean_C130}{Mean of C130}
#'   \item{x_mean_D130}{Mean of D130}
#'   \item{x_mean_G130}{Mean of G130}
#'   \item{x_mean_H}{Mean of H}
#'   \item{x_mean_HDOM}{Mean of HDOM}
#'   \item{x_mean_HTOT}{Mean of HTOT}
#' }
#' @source Internal package data
#' @keywords internal
#' @name equations
#' @docType data
NULL
