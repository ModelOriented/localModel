calculate_weights <- function(simulated_dataset, new_observation, kernel) {
  for_weights_x <- rbind(simulated_dataset, new_observation)
  for_weights <- cbind(y = 1, for_weights_x)

  model_matrix <- stats::model.matrix(stats::lm(y ~., data = for_weights))
  new_observation_coords <- model_matrix[nrow(model_matrix), ]
  sapply(as.data.frame(t(model_matrix[1:(nrow(model_matrix) - 1), ])),
         function(x) kernel(new_observation_coords, x))
}


#' @importFrom stats as.formula coef model.matrix

single_column_surrogate <- function(x, new_observation,
                                    simulated_data, to_predict,
                                    size, seed = NULL,
                                    weights, sampling = "uniform") {
  predicted_scores <- x$predict_function(x$model, to_predict)

  simulated_data[["y"]] <- 1
  model_mean <- mean(x$predict_function(x$model, x$data))

  model_matrix <- model.matrix(y ~ .,
                               data =  simulated_data)[, -1, drop = FALSE]
  if(ncol(model_matrix) == 1) {
    model_matrix <- cbind(zero = 0, model_matrix)
  }

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
    data.frame(estimated = model_mean,
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
#' @param grid_points Number of points to use while calculating Ceteris Paribus profiles.
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
                                       sampling = "uniform", grid_points = 101) {
  x$data <- x$data[, intersect(colnames(x$data), colnames(new_observation))]
  try_predict <- x$predict_function(x$model, x$data)
  predicted_names <- colnames(try_predict)
  if(is.null(predicted_names))
    predicted_names <- "yhat"

  if(!is.null(seed)) set.seed(seed)
  feature_representations <- lapply(
    colnames(x$data),
    function(column) {
      feature_representation(x,
                             new_observation,
                             column,
                             predicted_names,
                             grid_points)
    }
  )
  encoded_data <- as.data.frame(feature_representations)
  colnames(encoded_data) <- intersect(colnames(new_observation),
                                      colnames(x$data))

  p <- ncol(encoded_data)
  simulated_data <- as.data.frame(
    lapply(encoded_data,
           function(column) {
             as.character(rep(levels(column)[max(1, length(levels(column)))], size))
           }), stringsAsFactors = FALSE)

  probs <- lapply(encoded_data,
                  function(column) {
                    as.data.frame(prop.table(table(column)))$Freq
                  })

  if(!is.null(seed)) set.seed(seed)
  for(row_number in 1:size) {
    n_changes <- sample(1:p, 1)
    change_indices <- sample(1:p, n_changes)
    if(sampling == "uniform") {
      simulated_data[row_number, change_indices] <- "baseline"
    } else {
      for(index in change_indices) {
        simulated_data[row_number, index] <- sample(
          levels(encoded_data[, index]),
          1,
          prob = probs[[index]])
      }
    }
  }

  for(col_number in 1:p) {
    simulated_data[, col_number] <- factor(
      simulated_data[, col_number],
      levels = levels(encoded_data[, col_number])
    )
  }

  n_rows <- nrow(encoded_data)
  if(!is.null(seed)) set.seed(seed)
  to_predict <- data.frame(
    lapply(colnames(simulated_data),
           function(column) {
             how_many_baselines <- sum(simulated_data[, column] == "baseline")
             baseline_indices <- which(encoded_data[, column] == "baseline")
             if(is.numeric(x$data[, column])) {
               ifelse(simulated_data[, column] == "baseline",
                      sample(x$data[baseline_indices, column],
                             how_many_baselines,
                             replace = TRUE),
                      rep(new_observation[, column], size - how_many_baselines)
               )
             } else {
               ifelse(simulated_data[, column] == "baseline",
                      as.character(sample(x$data[baseline_indices, column],
                                          how_many_baselines,
                                          replace = TRUE)),
                      as.character(rep(new_observation[, column],
                                       size - how_many_baselines))
               )
             }
           }))
  colnames(to_predict) <- colnames(simulated_data)
  for(colname in colnames(simulated_data)) {
    if(is.numeric(x$data[, colname])) {
      to_predict[, colname] <- as.numeric(to_predict[, colname])
    } else {
      to_predict[, colname] <- factor(to_predict[, colname],
                                      levels = levels(x$data[, colname]))
    }
  }

  simulated_data <- simulated_data[, sapply(simulated_data,
                                            function(col) length(unique(col)) > 1),
                                   drop = FALSE]
  instance <- data.frame(lapply(simulated_data, function(c) levels(c)[2]))
  weights <- calculate_weights(simulated_data, instance, kernel)

  if(!is.null(ncol(try_predict))) {
    explainer <- lapply(unique(colnames(try_predict)), function(unique_level) {
      internal_explainer <- x
      internal_explainer$predict_function <- function(model, newdata) {
        x$predict_function(model, newdata)[, unique_level]

      }
      result <- single_column_surrogate(internal_explainer,
                                        new_observation,
                                        simulated_data, to_predict,
                                        size, seed,
                                        weights, sampling)
      result[, "response"] <- unique_level
      result[, "predicted_value"] <- internal_explainer$predict_function(
        internal_explainer$model,
        new_observation
      )
      result
    })
  } else {
      explainer <- single_column_surrogate(
        x, new_observation,
        simulated_data, to_predict,
        size, seed, weights, sampling
      )
      explainer[["response"]] <- ""
      explainer[["predicted_value"]] <- x$predict_function(
        x$model,
        new_observation
      )
  }
  if(!is.null(ncol(try_predict))) {
    explainer <- do.call("rbind", explainer)
  }
  attr(explainer, "new_observation") <- new_observation
  explainer$model <- x$label
  class(explainer) <- c("local_surrogate_explainer", class(explainer))
  explainer
}


#' Generic plot function for local surrogate explainers
#'
#' @param x object of class local_surrogate_explainer
#' @param ... other objects of class local_surrogate_explainer.
#' If provided, models will be plotted in rows, response levels in columns.
#' @param geom If "point", lines with points at the end will be plotted,
#' if "bar", bars will be plotted and if "arrow", arrows.
#'
#' @import ggplot2
#'
#' @importFrom stats reorder
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

plot.local_surrogate_explainer <- function(x, ..., geom = "point") {
  variable <- estimated <- intercept <- NULL

  models <- do.call("rbind", c(list(x), list(...)))

  if(all(models$estimated[models$original_variable != ""] == 0)) {
    message("All estimated feature effects are equal to 0.")
    return(ggplot())
  }

  models$labeller <- paste(
    models$model,
    "prediction: ",
    round(models$predicted_value, 2)
  )

  models <- do.call("rbind", by(
    models,
    models$response,
    function(y) {
      y$labeller <- paste(
        unique(y$response),
        paste(unique(y$labeller),
              sep = "\n ", collapse = "\n "),
        sep = "\n ")
      y
    }
  ))

  models$sign <- as.factor(as.character(sign(models$estimated)))
  models <- models[models$variable != "(Intercept)", ]

  models <- do.call("rbind", by(
    models,
    list(models$model, models$response),
    function(y) {
      y$intercept <- y$estimated[y$variable == "(Model mean)"]
      y
    }
  ))

  models <- do.call("rbind", by(
    models,
    list(models$model, models$response),
    function(y) {
      y$estimated = ifelse(
        y$variable == "(Model mean)",
        y$estimated,
        y$estimated + y$intercept
      )
      y
    }
  ))

  models <- do.call("rbind", by(
    models,
    list(models$original_variable),
    function(y) {
      y$all_zero <- all(y$sign == 0)
      y
    }
  ))

  models <- models[!models$all_zero, ]
  models <- models[models$variable != "(Model mean)", ]

  if(geom == "point") {
    final_geom <- geom_pointrange(aes(ymin = intercept, ymax = estimated),
                                  size = 2)
  } else if(geom == "bar") {
    final_geom <- geom_segment(aes(xend = variable, yend = intercept, y = estimated),
                               size = 10, lineend = "butt")
  } else {
    final_geom <- geom_segment(aes(xend = variable, yend = intercept, y = estimated),
                               size = 2,
                               arrow = arrow(length=unit(0.20,"cm"),
                                             ends="first", type = "closed"))
  }

  ggplot(models, aes(x = reorder(variable, -abs(estimated)),
                     y = estimated,
                     color = sign)) +
    theme_bw() +
    geom_hline(aes(yintercept = intercept),
               size = 1)  +
    facet_grid(model~labeller, scales = "free_y") +
    final_geom +
    coord_flip() +
    ylab("Feature influence") +
    xlab("") +
    scale_color_manual(values =  c(`-1` = "#d8b365",
                                   `0` = "#f5f5f5",
                                   `1` = "#5ab4ac")) +
    guides(color = "none")
}


#' Generic print function for local surrogate explainers
#'
#' @param x object of class local_surrogate_explainer
#' @param ... currently ignored
#'
#' @export
#'
#' @importFrom utils head
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
#' plot(model_lok)
#' model_lok
#'

print.local_surrogate_explainer <- function(x, ...) {
  print(head(as.data.frame(x)))
}
