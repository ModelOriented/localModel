calculate_weights <- function(simulated_dataset, new_observation, kernel) {
  for_weights_x <- rbind(simulated_dataset, new_observation)
  for_weights <- cbind(y = 1, for_weights_x)

  model_matrix <- stats::model.matrix(stats::lm(y ~., data = for_weights))
  new_observation_coords <- model_matrix[nrow(model_matrix), ]
  sapply(as.data.frame(t(model_matrix[1:(nrow(model_matrix) - 1), ])),
         function(x) kernel(new_observation_coords, x))
}


#' @importFrom utils head
assign_target_names <- function(x) {
  try_predict <- x$predict_function(x$model, head(x$data))
  predicted_names <- colnames(try_predict)
  if(is.null(predicted_names))
    predicted_names <- "yhat"
  predicted_names
}

remove_redundant_columns <- function(simulated_data) {
  simulated_data[, vapply(simulated_data,
                          function(col) length(unique(col)) > 1,
                          logical(1)),
                 drop = FALSE]
}

set_explainer_attributes <- function(explainer, x, new_observation, interpretable_features) {
  attr(explainer, "new_observation") <- new_observation
  attr(explainer, "interpretable_features") <- interpretable_features
  attr(explainer, "prediction") <- predict(x, new_observation)
  explainer$model <- x$label
  class(explainer) <- c("local_surrogate_explainer", class(explainer))
  explainer
}


prepare_model_matrix <- function(x, simulated_data) {
  simulated_data[["y"]] <- 1
  model_mean <- mean(x$predict_function(x$model, x$data))

  model_matrix <- model.matrix(y ~ .,
                               data =  simulated_data)[, -1, drop = FALSE]
  if(ncol(model_matrix) == 1) {
    model_matrix <- cbind(zero = 0, model_matrix)
  }
  model_matrix
}
