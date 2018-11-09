encode_data_frame <- function(explainer, data, prediction_quantiles) {
  whatif_curves <- lapply(
    colnames(data),
    function(x) {
      ceterisParibus::ceteris_paribus(explainer,
                                      observation,
                                      variables = x,
                                      grid_points = nrow(data))[, c(x, "_yhat_")]
    })

  encoded_data <- data
  for(x in 1:ncol(data)) {
    encoded_data[, x][order(data[, x])] <- ifelse(
      whatif_curves[[x]]$`_yhat_` <= prediction_quantiles[[x]][1],
      "lower",
      ifelse(whatif_curves[[x]]$`_yhat_` > prediction_quantiles[[x]][1] &
               whatif_curves[[x]]$`_yhat_` <= prediction_quantiles[[x]][2],
             "baseline",
             "higher"))
  }

  encoded_data
}


#' Local surrogate model
#'
#' @param explainer DALEX explainer object.
#' @param observation Instance to be explained.
#' @param size Number of instance to simulate.
#' @param seed Argument to set.seed, if not specified, results will not be reproducible.
#' @param response_family Family argument to glmnet function.
#' @param n_rows Number of points, at which ceteris paribus curves will be calculated.
#' @param smoothness Span argument to loess function.
#'
#' @return glmnet model object
#'
#' @importFrom stats predict loess quantile
#'
#' @export
#'

local_approximation <- function(explainer, observation,
                                size, seed = NULL,
                                response_family =  "gaussian",
                                n_points = nrow(explainer$data),
                                smoothness = 0.3, ...) {
  check_conditions(explainer$data, observation, size)
  numerical_data <- dplyr::select_if(explainer$data, is.numeric)
  categorical_data <- dplyr::select_if(explainer$data,
                                       function(x) is.factor(x) | is.character(x))

  whatif_curves <- lapply(
    colnames(numerical_data),
    function(x) {
      ceteris_curve <- ceterisParibus::ceteris_paribus(explainer,
                                                       observation,
                                                       variables = x,
                                                       grid_points = n_points)[, c(x, "_yhat_")]
      ceteris_curve[, 2] <- suppressWarnings(predict(loess(ceteris_curve[, 2] ~ ceteris_curve[, 1],
                                          span = smoothness)))
      ceteris_curve
    })

  prediction_quantiles <- lapply(whatif_curves,
                                 function(x) {
                                   quantile(x$`_yhat_`, probs = c(1/3, 2/3))
                                 })

  encoded_observation <- encode_data_frame(explainer, dplyr::select_if(observation, is.numeric),
                                           prediction_quantiles)
  encoded_observation <- dplyr::bind_cols(encoded_observation,
                                          dplyr::select_if(observation,
                                                           function(x) !is.numeric(x)))

  encoded_data <- encode_data_frame(explainer, numerical_data,
                                    prediction_quantiles)

  means_by_group <- vector("list", ncol(encoded_data))
  for(x in 1:ncol(numerical_data)) {
    means_by_group[[x]] <- tapply(numerical_data[, x], encoded_data[, x], mean)
  }

  encoded_data <- dplyr::bind_cols(encoded_data, categorical_data)

  similar <- dplyr::bind_rows(
    lapply(
      1:size,
      function(x) encoded_observation
    )
  )

  for(i in 1:size) {
    j <- sample(1:ncol(similar), size = 1)
    if(dplyr::n_distinct(encoded_data[, j]) > 1) {
      similar[i, j] <- sample(setdiff(unique(encoded_data[, j]), similar[i, j]),
                              size = 1, prob = rep(1/dplyr::n_distinct(encoded_data[, j]),
                                                   times = dplyr::n_distinct(encoded_data[, j]) - 1))
    }
  }

  to_predict <- similar
  for(i in 1:ncol(similar)) {
    if(colnames(similar[, i, drop = FALSE]) %in% colnames(numerical_data)) {
      to_predict[, i] <- means_by_group[[i]][to_predict[, i]]
    }
  }

  model_response <- explainer$predict_function(explainer$model, to_predict)
  explorer <- list(data = similar,
                   target = model_response,
                   explained_instance = observation)

  model <- fit_explanation(explorer, kernel, response_family = response_family)
  class(model) <- c("live_explainer", class(model))
  model
}

