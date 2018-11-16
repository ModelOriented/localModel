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
#' @param x an explainer created with function `DALEX2::explain()`
#' @param new_observation an observation/observations to be explained.
#'        Columns in should correspond to columns in the data element of explainer.
#' @param size Number of instance to simulate.
#' @param seed Argument to set.seed, if not specified, results will not be reproducible.
#' @param response_family Family argument to glmnet function.
#' @param n_points Number of points, at which ceteris paribus curves will be calculated.
#' @param smoothness Span argument to loess function.
#'
#' @return glmnet model object
#'
#' @importFrom stats predict loess quantile
#'
#' @export
#'

individual_surrogate_model <- function(x, new_observation,
                                       size, seed = NULL,
                                       response_family =  "gaussian",
                                       kernel = gaussian_kernel,
                                       n_points = nrow(x$data),
                                       smoothness = 0.3, ...) {
  check_conditions(x$data, new_observation, size)
  numerical_data <- dplyr::select_if(x$data, is.numeric)
  categorical_data <- dplyr::select_if(x$data,
                                       function(y) is.factor(y) | is.character(y))

  whatif_curves <- lapply(
    colnames(numerical_data),
    function(y) {
      ceteris_curve <- ceterisParibus::ceteris_paribus(x,
                                                       new_observation,
                                                       variables = y,
                                                       grid_points = n_points)[, c(y, "_yhat_")]
      ceteris_curve[, 2] <- suppressWarnings(predict(loess(ceteris_curve[, 2] ~ ceteris_curve[, 1],
                                                           span = smoothness)))
      ceteris_curve
    })

  prediction_quantiles <- lapply(whatif_curves,
                                 function(y) {
                                   quantile(y$`_yhat_`, probs = c(1/3, 2/3))
                                 })

  encoded_observation <- encode_data_frame(x,
                                           dplyr::select_if(new_observation,
                                                            is.numeric),
                                           prediction_quantiles)
  encoded_observation <- dplyr::bind_cols(encoded_observation,
                                          dplyr::select_if(new_observation,
                                                           function(y) !is.numeric(y)))

  encoded_data <- encode_data_frame(x,
                                    numerical_data,
                                    prediction_quantiles)

  means_by_group <- vector("list", ncol(encoded_data))
  for(y in 1:ncol(numerical_data)) {
    means_by_group[[y]] <- tapply(numerical_data[, y], encoded_data[, y], mean)
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

  model_response <- x$predict_function(x$model, to_predict)
  explorer <- list(data = similar,
                   target = model_response,
                   explained_instance = new_observation)

  model <- fit_explanation(explorer, kernel, response_family = response_family)
  class(model) <- c("live_explainer", class(model))
  model
}

