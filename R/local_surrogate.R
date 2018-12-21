#' @importFrom stats as.formula coef model.matrix

single_column_surrogate <- function(x, new_observation,
                                    encoded_data,
                                    size, seed = NULL,
                                    sampling = "uniform") {
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
  predicted_scores <- x$predict_function(x$model, to_predict)
  simulated_data <- simulated_data[, sapply(simulated_data,
                                            function(col) length(unique(col)) > 1)]
  simulated_data[["y"]] <- 1
  model_mean <- mean(x$predict_function(x$model, x$data))
  fitted_model <- glmnet::cv.glmnet(model.matrix(y ~ .,
                                                 data =  simulated_data)[, -1],
                                    predicted_scores - model_mean,
                                    alpha = 1)
  result <- as.data.frame(as.matrix(coef(fitted_model, lambda = "lambda.min")))
  result$variable <- rownames(result)
  rownames(result) <- NULL
  colnames(result)[1] <- "estimated"
  for(row_number in 2:nrow(result)) {
    result[row_number, "variable"] <- stringr::str_sub(result[row_number, "variable"],
                                                       stringr::str_length(colnames(simulated_data)[row_number - 1]) + 1)
  }
  rbind(
    data.frame(estimated = model_mean,
               variable = "(Model mean)"),
    result
  )
}


#' LIME-like explanations based on Ceteris Paribus curves
#'
#' @param x an explainer created with the function DALEX2::explain().
#' @param new_observation an observation to be explained. Columns in should correspond to columns in the data argument to x.
#' @param size number of similar observation to be sampled.
#' @param seed If not NULL, seed will be set to this value for reproducibility.
#' @param sampling Parameter that controls sampling while creating new observations.
#' @param grid_points Number of points to use while calculating Ceteris Paribus profiles.
#'
#' @return data.frame of class local_surrogate_explainer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #' @examples
#' \dontrun{
#' # Example based on apartments data from DALEX package.
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- DALEX::explain(model = mrf,
#'                             data = apartmentsTest[, -1])
#' model_lok <- individual_surrogate_model(explainer, new_observation,
#'                                         size = 500, seed = 17)
#' }

#' }
#'

individual_surrogate_model <- function(x, new_observation, size, seed = NULL,
                                       sampling = "uniform", grid_points = 101) {
  x$data <- x$data[, intersect(colnames(x$data), colnames(new_observation))]
  try_predict <- x$predict_function(x$model, x$data[1:5, ])
  predicted_names <- colnames(try_predict)
  if(is.null(predicted_names))
    predicted_names <- "yhat"

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

  if(!is.null(ncol(try_predict))) {
    explainer <- lapply(unique(colnames(try_predict)), function(unique_level) {
      internal_explainer <- x
      internal_explainer$predict_function <- function(model, newdata) {
        x$predict_function(model, newdata)[, unique_level]

      }
      result <- single_column_surrogate(internal_explainer,
                                        new_observation,
                                        encoded_data,
                                        size, seed, sampling)
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
        encoded_data,
        size, seed, sampling
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
#' @param ... currently ignored
#'
#' @import ggplot2
#'
#' @importFrom stats reorder
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example based on apartments data from DALEX package.
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- DALEX::explain(model = mrf,
#'                             data = apartmentsTest[, -1])
#' model_lok <- individual_surrogate_model(explainer, new_observation,
#'                                         size = 500, seed = 17)
#' plot(model_lok)
#' }
#'

plot.local_surrogate_explainer <- function(x, ...) {
  variable <- estimated <- intercept <- NULL

  x$response <- paste(
    x$response,
    "predicted value: ",
    x$predicted_value
  )

  x <- x[x$estimated != 0, ]
  x <- x[x$variable != "(Intercept)", ]
  x$estimated <- unlist(tapply(x$estimated,
                               x$response,
                               function(y) {
                                 c(y[1], y[-1] + y[1])
                               }), use.names = FALSE)
  x$intercept = unlist(
    tapply(
      x$estimated,
      x$response,
      function(y) rep(y[1], length(y))
    )
  )

  x <- x[x$variable != "(Model mean)", ]

  ggplot(x, aes(x = reorder(variable, abs(estimated)),
                y = estimated)) +
    theme_bw() +
    geom_hline(aes(yintercept = intercept),
               size = 1)  +
    geom_pointrange(aes(ymin = intercept, ymax = estimated),
                    size = 2) +
    facet_wrap(~response, ncol = 1, scales = "free_y") +
    coord_flip() +
    ylab("Estimated effect") +
    xlab("")
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
#' \dontrun{
#' # Example based on apartments data from DALEX package.
#' mrf <- randomForest(m2.price ~., data = apartments, ntree = 50)
#' explainer <- DALEX::explain(model = mrf,
#'                             data = apartmentsTest[, -1])
#' model_lok <- individual_surrogate_model(explainer, new_observation,
#'                                         size = 500, seed = 17)
#' model_lok
#' }
#'

print.local_surrogate_explainer <- function(x, ...) {
  print(head(as.data.frame(x)))
}
