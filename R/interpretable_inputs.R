extract_numerical_feature <- function(rule, observation) {
  feature_name <- unique(rule[, 3])
  actual_value <- observation[, feature_name]
  rule <- rule[, c(5, 7)]
  colnames(rule) <- c("endpoint_1", "endpoint_2")
  rule$is_correct_interval <- FALSE
  for(row_number in 1:nrow(rule)) {
    if(rule[row_number, 1] != "" & rule[row_number, 2] != "") {
      rule[row_number, 3] <- rule[row_number, 1] <= actual_value & actual_value < rule[row_number, 2]
    } else {
      if(rule[row_number, 1] == "") {
        rule[row_number, 3] <- actual_value >= rule[row_number, 2]
      } else {
        rule[row_number, 3] <- actual_value < rule[row_number, 1]
      }
    }
  }
  interval_as_data_frame <- rule[rule$is_correct_interval, ]
  if(rule[1, 1] != "" & rule[1, 2] != "") {
    label <- paste(rule[1, 1], "<=", feature_name, "< ", rule[1, 2])
    interval_ends <- c(rule[1, 1], rule[1, 2])
  } else {
    if(rule[1, 1] == "") {
      label <- paste(feature_name, ">=", rule[1, 2])
      interval_ends <- c(rule[1, 2], Inf)
    } else {
      label <- paste(feature_name, "<", rule[1, 1])
      interval_ends <- c(-Inf, rule[1, 1])
    }
  }
  list(
    label = label,
    interval_ends = interval_ends
  )
}


extract_categorical_feature <- function(rule, observation) {
 feature_name <- unique(rule[, 3])
 actual_value <- as.character(observation[, feature_name])
 rule <- rule[, 5]
 rule <- strsplit(rule, " or ")
 if(actual_value %in% rule[[1]]) {
   label <- paste(feature_name, "=",
                  paste(rule[[1]], sep = ", ", collapse = ", "))
   values <- rule[[1]]
 } else {
   label <- paste(feature_name, "=",
                  paste(rule[[2]], sep = ", ", collapse = ", "))
   values <- rule[[2]]
 }
 list(
   "label" = label,
   "values" = values
 )
}

feature_representation <- function(explainer, new_observation, feature_name) {
  if(is.numeric(explainer$data[, feature_name])) {
    ceteris_curve <- ceterisParibus::ceteris_paribus(
      explainer,
      new_observation,
      variables = feature_name)[, c("_yhat_", feature_name)]
    fitted_tree <- rpart::rpart(as.formula(paste("`_yhat_` ~", feature_name)),
                                data = ceteris_curve,
                                maxdepth = 2)
    fitted_rule <- as.data.frame(rpart.plot::rpart.rules(fitted_tree))

    interpretable_input <- extract_numerical_feature(fitted_rule,
                                                       new_observation)
    encoded_feature <- ifelse(interpretable_input$interval_ends[1] <= explainer$data[, feature_name]
                              & explainer$data[, feature_name] < interpretable_input$interval_ends[2],
                              interpretable_input$label,
                              "baseline")
    encoded_feature <- factor(encoded_feature,
                              levels = c("baseline", interpretable_input$label))
  } else {
    if(length(unique(explainer$data[, feature_name])) == 2) {
      encoded_feature <- ifelse(
        explainer$data[, feature_name] == new_observation[, feature_name],
        paste(feature_name, "=", new_observation[, feature_name]),
        "baseline"
      )
      encoded_feature <- factor(encoded_feature,
                                levels = c("baseline",
                                           paste(feature_name, "=", new_observation[, feature_name]))
                                )
    } else {
      predicted_response <- explainer$predict_function(explainer$model,
                                                       explainer$data)
      prediction_df <- data.frame(explainer$data[, feature_name],
                                  predicted_response)
      colnames(prediction_df) <- c(feature_name, "y")
      fitted_tree <- rpart::rpart(as.formula(paste("y ~", feature_name)),
                                  data = prediction_df,
                                  maxdepth = 1)
      fitted_rule <- rpart.plot::rpart.rules(fitted_tree)

      interpretable_input <- extract_categorical_feature(fitted_rule,
                                                         new_observation)
      encoded_feature <- ifelse(
        explainer$data[, feature_name] %in% interpretable_input$values,
        interpretable_input$label,
        "baseline"
      )
      encoded_feature <- factor(encoded_feature,
                                levels = c("baseline", interpretable_input$label))
    }
    }

  encoded_feature
}
