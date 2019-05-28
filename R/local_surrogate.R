#' @importFrom stats as.formula coef model.matrix
single_column_surrogate <- function(x, new_observation, simulated_data,
                                    to_predict, size, seed, weights,
                                    sampling = "uniform") {

  predicted_scores <- x$predict_function(x$model, to_predict)

  model_matrix <- prepare_model_matrix(x, simulated_data)

  if(!is.null(seed)) set.seed(seed)
  fitted_model <- glmnet::cv.glmnet(model_matrix,
                                    predicted_scores,
                                    alpha = 1, weights = weights)
  result <- as.data.frame(as.matrix(coef(fitted_model, lambda = "lambda.min")))
  result$variable <- rownames(result)
  rownames(result) <- NULL
  result <- result[result$variable != "zero", ]
  colnames(result)[1] <- "estimated"

  for(row_number in 2:nrow(result)) {
    result[row_number, "variable"] <- substr(result[row_number, "variable"],
                                             nchar(colnames(simulated_data)[row_number - 1]) + 1,
                                             nchar(result[row_number, "variable"]))
  }

  result <- rbind(
    data.frame(estimated = mean(x$predict_function(x$model, x$data)),
               variable = "(Model mean)"),
    result
  )
  result$original_variable <- ""
  for(i in 3:nrow(result)) {
    result[i, "original_variable"] <- colnames(new_observation)[
      sapply(colnames(new_observation), function(c) grepl(c, result[i, "variable"]))]
  }

  correct_lambda <- which(fitted_model$glmnet.fit$lambda == fitted_model$lambda.min)
  result$dev_ratio <- fitted_model$glmnet.fit$dev.ratio[correct_lambda]
  result
}


#' LIME-like explanations based on Ceteris Paribus curves
#'
#' This function fits a LIME-type explanation of a single prediction.
#' Interpretable binary features that describe the local impact of features on
#' the prediction are created based on Ceteris Paribus Profiles.
#' Thend, a new dataset of similar observations is created and black box model
#' predictions (scores in case of classification) are calculated for this dataset
#' and LASSO regression model is fitted to them.
#' This way, explanations are simplified and include only the most important features.
#' More details about the methodology can be found in the vignettes.
#'
#' @param x an explainer created with the function DALEX::explain().
#' @param new_observation an observation to be explained. Columns in should correspond to columns in the data argument to x.
#' @param size number of similar observation to be sampled.
#' @param seed If not NULL, seed will be set to this value for reproducibility.
#' @param kernel Kernel function which will be used to weight simulated observations.
#' @param sampling Parameter that controls sampling while creating new observations.
#' @param ... Additional arguments that will be passed to ingredients::ceteris_paribus.
#'
#' @return data.frame of class local_surrogate_explainer
#'
#' @export
#'
#' @examples
#' # Example based on apartments data from DALEX package.
#' library(DALEX)
#' library(randomForest)
#' library(localModel)
#' data('apartments')
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- explain(model = mrf,
#'                      data = apartments[, -1])
#' model_lok <- individual_surrogate_model(explainer, apartments[5, -1],
#'                                         size = 500, seed = 17)
#' model_lok
#' plot(model_lok)
#'

individual_surrogate_model <- function(x, new_observation, size, seed = NULL,
                                       kernel = identity_kernel,
                                       sampling = "uniform", ...) {

  # Prepare the data
  x$data <- x$data[, intersect(colnames(x$data), colnames(new_observation)), drop = F]
  predicted_names <- assign_target_names(x)

  # Create interpretable features
  feature_representations_full <- get_feature_representations(x, new_observation,
                                                              predicted_names, seed, ...)
  discretizations <- lapply(feature_representations_full, function(x) x[[2]])
  names(discretizations) <- colnames(x$data)
  encoded_data <- transform_to_interpretable(x, new_observation,
                                             feature_representations_full)

  # Generate similar observations
  simulated_data <- create_neighbourhood(encoded_data, size, sampling, seed)

  # Transform back to feature space so the predictions can be obtained
  to_predict <- transform_from_interpretable(x, new_observation,
                                             simulated_data, encoded_data,
                                             size, seed)

  # Prepare to fit linear model
  simulated_data <- remove_redundant_columns(simulated_data)
  if(ncol(simulated_data) == 0) {
    explainer <- data.frame(
      estimated = NA,
      variable = NA,
      original_variable = NA,
      dev_ratio = NA,
      response = NA,
      predicted_value = NA,
      model = NA
    )
  } else {
    # Fit linear model to each target dimension, combine the results
    instance <- data.frame(lapply(simulated_data, function(c) levels(c)[2]))
    weights <- calculate_weights(simulated_data, instance, kernel)
    explainer <- combine_explanations(x, new_observation, simulated_data,
                                      to_predict, size, seed, weights, sampling)
  }

  set_explainer_attributes(explainer, x, new_observation, discretizations)
}


