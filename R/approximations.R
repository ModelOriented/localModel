encode_data_frame_two_levels <- function(explainer, observation, data,
                                         prediction_quantiles) {
  whatif_curves <- lapply(
    colnames(data),
    function(y) {
      ceterisParibus::ceteris_paribus(explainer,
                                      observation,
                                      variables = y,
                                      grid_points = nrow(data))[, c(y, "_yhat_")]
    })

  encoded_data <- data
  for(column in 1:ncol(data)) {
    if(explainer$predict_function(explainer$model, observation) <= prediction_quantiles[[column]]) {
      encoded_data[, column][order(data[, column])] <- ifelse(
        whatif_curves[[column]]$`_yhat_` <= prediction_quantiles[[column]],
        "baseline", "higher")
    } else {
      encoded_data[, column][order(data[, column])] <- ifelse(
        whatif_curves[[column]]$`_yhat_` <= prediction_quantiles[[column]],
        "lower", "baseline")
    }
  }
  encoded_data
}


encode_data_frame <- function(explainer, observation, data, prediction_quantiles) {
  whatif_curves <- lapply(
    colnames(data),
    function(y) {
      ceterisParibus::ceteris_paribus(explainer,
                                      observation,
                                      variables = y,
                                      grid_points = nrow(data))[, c(y, "_yhat_")]
    })

  encoded_data <- data
  for(column in 1:ncol(data)) {
    encoded_data[, column][order(data[, column])] <- ifelse(
      whatif_curves[[column]]$`_yhat_` <= prediction_quantiles[[column]][1],
      "lower",
      ifelse(whatif_curves[[column]]$`_yhat_` > prediction_quantiles[[column]][1] &
               whatif_curves[[column]]$`_yhat_` <= prediction_quantiles[[column]][2],
             "baseline",
             "higher"))
  }

  encoded_data
}


single_column_local_surrogate <- function(explainer,
                                          new_observation,
                                          size,
                                          levels = "two",
                                          seed = NULL,
                                          kernel = gaussian_kernel,
                                          grid_points = nrow(explainer$data),
                                          smoothness = NULL, ...) {
  numerical_data <- dplyr::select_if(explainer$data, is.numeric)
  categorical_data <- dplyr::select_if(explainer$data, is.factor)

  # Calculate what if curves for numerical features.
  whatif_curves <- lapply(
    colnames(numerical_data),
    function(y) {
      ceteris_curve <- ceterisParibus::ceteris_paribus(explainer,
                                                       new_observation,
                                                       variables = y,
                                                       grid_points = grid_points)[, c(y, "_yhat_")]
      if(!is.null(smoothness)) {
        ceteris_curve[, 2] <- suppressWarnings(predict(loess(ceteris_curve[, 2] ~ ceteris_curve[, 1],
                                                             span = smoothness)))
      }
      ceteris_curve
    })

  if(levels == "two") {
    # Calculate median of predicted values for each curve.
    prediction_quantiles <- lapply(whatif_curves,
                                   function(y) median(y$`_yhat_`))

    # Encode observation and data into "baseline"/"lower"/"higher" based on the quantiles.
    encoded_observation <- encode_data_frame_two_levels(explainer,
                                             new_observation,
                                             dplyr::select_if(new_observation,
                                                              is.numeric),
                                             prediction_quantiles)
    encoded_observation <- dplyr::bind_cols(encoded_observation,
                                            dplyr::select_if(new_observation,
                                                             function(y) !is.numeric(y)))
    encoded_data <- encode_data_frame_two_levels(explainer,
                                      new_observation,
                                      numerical_data,
                                      prediction_quantiles)

  } else {
    # Calculate median of predicted values for each curve.
    prediction_quantiles <- lapply(whatif_curves,
                                   function(y) {
                                     quantile(y$`_yhat_`, probs = c(1/3, 2/3))
                                   })

    # Encode observation and data into "baseline"/"lower"/"higher" based on the quantiles.
    encoded_observation <- encode_data_frame(explainer,
                                             new_observation,
                                             dplyr::select_if(new_observation,
                                                              is.numeric),
                                             prediction_quantiles)
    encoded_observation <- dplyr::bind_cols(encoded_observation,
                                            dplyr::select_if(new_observation,
                                                             function(y) !is.numeric(y)))

    encoded_data <- encode_data_frame(explainer,
                                      new_observation,
                                      numerical_data,
                                      prediction_quantiles)
  }

  # Find mean prediction for each level of a factor ("baseline" etc).
  means_by_group <- vector("list", ncol(encoded_data))
  for(y in 1:ncol(numerical_data)) {
    means_by_group[[y]] <- tapply(numerical_data[, y], encoded_data[, y], mean)
  }

  encoded_data <- dplyr::bind_cols(encoded_data, categorical_data)

  # Sample new observations.
  similar <- encoded_observation[rep(1, times = size), ]
  if(!is.null(seed)) set.seed(seed)
  for(i in 1:size) {
    n_features <- sample(1:ncol(similar), size = 1)
    to_change <- sample(1:ncol(similar), size = n_features)
    for(j in to_change) {
      if(dplyr::n_distinct(encoded_data[, j]) > 1) {
        props <- prop.table(table(encoded_data[, j]))
        probs <- props[-which(names(props) == similar[i, j])]
        similar[i, j] <- sample(setdiff(sort(unique(encoded_data[, j])), similar[i, j]),
                                size = 1,
                                prob = probs)
      }
    }
  }

  # Calculate model predictions on new observations.
  to_predict <- similar
  for(i in 1:ncol(similar)) {
    if(colnames(similar[, i, drop = FALSE]) %in% colnames(numerical_data)) {
      to_predict[, i] <- means_by_group[[i]][to_predict[, i]]
    }
  }
  model_response <- explainer$predict_function(explainer$model, to_predict)

  # Merge factor levels.
  similar[, colnames(similar) %in% colnames(categorical_data)] <- dplyr::mutate_all(
    similar[, colnames(similar) %in% colnames(categorical_data), drop = F],
    function(y) {
      if(dplyr::n_distinct(y) > 3) {
        merge_factor_levels(
          model_response,
          y
        )
      } else {
        y
      }
    })

  explorer <- list(data = similar,
                   target = model_response,
                   explained_instance = new_observation)

  fit_explanation(explorer, kernel)
}

#' Local surrogate model
#'
#' @param x an explainer created with function `DALEX2::explain()`
#' @param new_observation an observation/observations to be explained.
#'        Columns in should correspond to columns in the data element of explainer.
#' @param size Number of instance to simulate.
#' @param levels Should two levels of each variable be created ("baseline", "lower"
#' or "higher") or three levels ("baseline", "lower" and "higher").
#' @param seed Argument to set.seed, if not specified, results will not be reproducible.
#' @param kernel kernel function which will be used to weight distances between observations.
#' @param grid_points Number of points, at which ceteris paribus curves will be calculated.
#' @param smoothness Span argument to loess function. If NULL, smoothing won't be applied
#' to results of ceteris_paribus function.
#'
#' @return glmnet model object
#'
#' @importFrom stats predict loess quantile
#'
#' @export
#'

individual_surrogate_model <- function(x, new_observation,
                                       size,
                                       levels = "two",
                                       seed = NULL,
                                       kernel = gaussian_kernel,
                                       grid_points = nrow(x$data),
                                       smoothness = NULL) {
  try_predict <- x$predict_function(x$model, x$data[1:5, ])
  if(is.factor(x$y) | (!is.null(ncol(try_predict)))) {
    explainer <- lapply(unique(colnames(try_predict)), function(unique_level) {
      internal_explainer <- x
      internal_explainer$predict_function <- function(model, newdata) {
        x$predict_function(model, newdata)[, unique_level]

      }
      result <- single_column_local_surrogate(internal_explainer, new_observation,
                                    size, levels, seed, kernel,
                                    grid_points, smoothness)
      result$model_coefs[, "response"] <- unique_level
      result
    })
  } else {
    if(is.numeric(x$y)) {
      local_surrogate_result <- single_column_local_surrogate(
        x, new_observation,
        size, levels, seed, kernel,
        grid_points, smoothness
      )
      local_surrogate_result$model_coefs[, "response"] <- ""
      explainer <- list(
        local_surrogate = local_surrogate_result
      )

    } else {
      stop("Response must be numeric or factor vector")
    }
  }
  class(explainer) <- c("local_surrogate_explainer", class(explainer))
  explainer
}

