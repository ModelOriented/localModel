#' localModel: LIME-like explanations with interpretable features based on Ceteris Paribus profiles
#'
#' This package implements LIME-like explanation method
#' (see Tulio Ribeiro, Singh, Guestrin (2016) <doi:10.1145/2939672.2939778>) in which interpretable
#' inputs are created based on local rather than global behaviour of each original feature.#'
#'
#' @section Important functions:
#' \code{\link{individual_surrogate_model}} generates an explanation for a single prediction with
#' interpretable features based on Ceteris Paribus profiles.
#' \code{\link{plot.local_surrogate_explainer}} plots the explanation.
#'
#' @docType package
#' @name localModel
NULL
