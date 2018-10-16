#' @export

local_approximation <- function(explainer, observation,
                                size, fixed_variables = NULL,
                                seed = NULL, feature_selection = FALSE,
                                response_family =  "gaussian",
                                predict_function = predict, ...) {
  # check_conditions(data, explained_instance, size)

  similar_list <- generate_neighbourhood(explainer$data, observation,
                                    size, fixed_variables, seed)
  similar <- similar_list$data
  indices <- similar_list$indices

  # for(i in 1:nrow(similar)) {
  #   similar[i, ] <- encode_ceteris(similar, ceterisParibus)
  # }

  model_response <- predict_function(explainer$model, similar)[, 1]
  median_predictions <- tapply(model_response, as.factor(as.character(indices)), median)

  # for(index in indices) {
  #
  # }


  explorer <- list(data = similar,
                   target = model_response,
                   explained_instance = observation,
                   fixed_variables = fixed_variables)

  fit_explanation(explorer, kernel, feature_selection, response_family)
}
