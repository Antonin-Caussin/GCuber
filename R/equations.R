#' Internal Allometric Equation Database
#'
#' @description
#' A dataset containing species-specific allometric models and associated metadata used
#' internally by the GCuber package for computing volume, biomass, and carbon in forest inventories.
#'
#' This dataset is used internally by GCuber functions such as \code{\link{calculate_volume}},
#' \code{\link{calculate_biomass}}, and \code{\link{convert_circumference}}.
#'
#' @format A data frame with 53 columns. Main fields include:
#' \describe{
#'   \item{Y}{Response variable modeled (e.g., "V22", "BIOMASS").}
#'   \item{A0}{Equation form code (1–5): determines if equation is additive or log-linear.}
#'   \item{X0–X5}{Predictor variable names used in the equation (e.g., D130, HTOT).}
#'   \item{b0–b5}{Numeric coefficients associated with predictors.}
#'   \item{Species}{Canonical species name used internally.}
#'   \item{Anglais}{Common name in English.}
#'   \item{Latin}{Latin binomial name.}
#'   \item{Code}{Numeric code identifying the species.}
#'   \item{Abr}{Abbreviation (e.g., "HE" for Hêtre).}
#'   \item{Source_Eq}{Citation or source of the model.}
#'   \item{Region}{Region where the model was developed.}
#'   \item{Equation}{Name or label of the original equation.}
#'   \item{n}{Sample size used to fit the model.}
#'   \item{sigma}{Standard error of the model.}
#'   \item{D130_Min, D130_Max}{Min and max D130 in the fitting dataset.}
#'   \item{x_mean_D130, SCE_D130}{Mean and standard error for D130.}
#'   \item{C130_Min, C130_Max}{Min and max C130 in the fitting dataset.}
#'   \item{x_mean_C130, SCE_C130}{Mean and standard error for C130.}
#'   \item{HTOT_Min, HTOT_Max}{Min and max HTOT in the fitting dataset.}
#'   \item{x_mean_HTOT, SCE_HTOT}{Mean and standard error for HTOT.}
#'   \item{HDOM_Min, HDOM_Max}{Min and max HDOM.}
#'   \item{x_mean_HDOM, SCE_HDOM}{Mean and standard error for HDOM.}
#'   \item{G130_Min, G130_Max}{Min and max basal area G130.}
#'   \item{x_mean_G130, SCE_G130}{Mean and standard error for G130.}
#'   \item{H_Min, H_Max}{Min and max height H.}
#'   \item{x_mean_H, SCE_H}{Mean and standard error for H.}
#'   \item{Reference}{Literature or source reference.}
#'   \item{ID}{Infra-density of the wood (in g/cm³), used for biomass estimation.}
#'   \item{Source_ID}{Reference source for infra-density.}
#'   \item{NumEquation}{Unique index of the equation in the dataset.}
#'   \item{HV, IV}{Coefficients for converting C130 to C150 (and vice versa).}
#' }
#'
#' @keywords internal
#' @name equations
#' @docType data
NULL
