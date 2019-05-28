create_neighbourhood <- function(encoded_data, size, sampling, seed) {
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
    if(sampling == "uniform") {
      change <- which(sample(c(TRUE, FALSE), size = p, replace = TRUE, prob = rep(0.5, 2)))
      if(length(change) == 0) {
        change <- sample(1:p, 1)
      }
      simulated_data[row_number, change] <- "baseline"
    } else {
      change <- which(sample(c(TRUE, FALSE), size = p, replace = TRUE, prob = rep(0.5, 2)))
      for(index in change) {
        simulated_data[row_number, index] <- sample(
          levels(encoded_data[, index]),
          size = 1,
          prob = probs[[index]])
      }
    }
  }

  # TODO: wydzielić z tego funkcję, żeby przetestować te poziomy
  for(col_number in 1:p) {
    new_levels <- levels(encoded_data[, col_number])
    new_levels <- new_levels[c(which(new_levels == "baseline"),
                               which(new_levels != "baseline"))]
    simulated_data[, col_number] <- factor(
      simulated_data[, col_number],
      levels = new_levels
    )
  }

  simulated_data
}

combine_explanations <- function(x, new_observation, simulated_data,
                                 to_predict, size, seed, weights, sampling) {
  try_predict <- x$predict_function(x$model, head(x$data))

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
    explainer <- do.call("rbind", explainer)
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
  explainer
}

get_feature_representations <- function(x, new_observation,
                                        predicted_names, seed, ...) {
  if(!is.null(seed)) set.seed(seed)
  lapply(colnames(x$data),
         function(column) {
           feature_representation(x,
                                  new_observation,
                                  column,
                                  predicted_names,
                                  ...)
         }
  )
}

transform_to_interpretable <- function(x, new_observation,
                                       feature_representations) {
  encoded_data <- as.data.frame(lapply(feature_representations,
                                       function(x) x[[1]]))
  colnames(encoded_data) <- intersect(colnames(new_observation),
                                      colnames(x$data))
  encoded_data
}

transform_from_interpretable <- function(x, new_observation,
                                         simulated_data,
                                         encoded_data, size, seed) {
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
  to_predict
}
