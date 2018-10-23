#' @export

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

#' @export

local_approximation <- function(explainer, observation,
                                size, fixed_variables = NULL,
                                seed = NULL, feature_selection = FALSE,
                                response_family =  "gaussian",
                                predict_function = predict, ...) {
  # check_conditions(data, explained_instance, size)

  whatif_curves <- lapply(
    colnames(dplyr::select_if(explainer$data, is.numeric)),
    function(x) {
      ceterisParibus::ceteris_paribus(explainer,
                                      observation,
                                      variables = x,
                                      grid_points = nrow(explainer$data))[, c(x, "_yhat_")]
    })

  prediction_quantiles <- lapply(whatif_curves,
                                 function(x) {
                                   quantile(x$`_yhat_`, probs = c(1/3, 2/3))
                                 })

  encoded_observation <- encode_data_frame(explainer, dplyr::select_if(observation, is.numeric),
                                           prediction_quantiles)

  encoded_data <- encode_data_frame(explainer, dplyr::select_if(explainer$data, is.numeric),
                                    prediction_quantiles)

  encode_data_frame(explainer, dplyr::select_if(HR, is.numeric),
                    prediction_quantiles)

  means_by_group <- vector("list", ncol(encoded_data))
  for(x in 1:ncol(numerical_data)) {
    means_by_group[[x]] <- tapply(numerical_data[, x], encoded_data[, x], mean)
  }

  similar <- dplyr::bind_rows(
    lapply(
      1:size,
      function(x) encoded_observation
    )
  )

  explorer <- list(data = similar,
                   target = model_response,
                   explained_instance = observation,
                   fixed_variables = fixed_variables)

  fit_explanation(explorer, kernel, feature_selection, response_family)
}
