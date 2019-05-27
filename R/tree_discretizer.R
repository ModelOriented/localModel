extract_numerical_feature <- function(rules, true_value) {
  rules_df <- as.data.frame(do.call("rbind", strsplit(rules, " ")),
                            stringsAsFactors = FALSE)
  if(ncol(rules_df) == 7) {
    rules_df[, 3] <- as.numeric(rules_df[, 3])
    rules_df[, 7] <- as.numeric(rules_df[, 7])
    for(row_number in 1:nrow(rules_df)) {
      if(rules_df[row_number, 2] != rules_df[row_number, 6]) {
        lower_limit <- min(rules_df[row_number, 3], rules_df[row_number, 7])
        upper_limit <- max(rules_df[row_number, 3], rules_df[row_number, 7])
        if(lower_limit < true_value & true_value <= upper_limit) {
          label <- paste(c(round(lower_limit, 2),
                           "<", rules_df[1, 1],
                           "<=", round(upper_limit, 2)),
                         sep = " ", collapse = " ")
          lower <- lower_limit
          upper <- upper_limit
          break
        }
      } else {
        if(rules_df[row_number, 2] == "<=") {
          upper_limit <- min(rules_df[row_number, 3], rules_df[row_number, 7])
          if(true_value <= upper_limit) {
            label <- paste(c(rules_df[1, 1], "<=", round(upper_limit, 2)),
                           sep = " ", collapse = " ")
            lower <- -Inf
            upper <- upper_limit
            break
          }
        } else {
          lower_limit <- max(rules_df[row_number, 3], rules_df[row_number, 7])
          if(true_value > lower_limit) {
            label <- paste(c(rules_df[1, 1], ">", round(lower_limit, 2)),
                           sep = " ", collapse = " ")
            lower <- lower_limit
            upper <- Inf
          }
        }
      }
    }
  } else {
    if(true_value <= as.numeric(rules_df[1, 3])) {
      label <- paste(rules_df[1, 1], "<=", round(as.numeric(rules_df[1, 3]), 2),
                     sep = " ", collapse = " ")
      lower <- -Inf
      upper <- as.numeric(rules_df[1, 3])
    } else {
      label <- paste(rules_df[1, 1], ">", round(as.numeric(rules_df[1, 3]), 2),
                     sep = " ", collapse = " ")
      lower <- as.numeric(rules_df[1, 3])
      upper <- Inf
    }
  }

  list(
    "label" = label,
    "interval" = as.numeric(c(lower, upper))
  )
}


extract_categorical_feature <- function(rules, true_value, unique_values,
                                        colname) {
  # To avoid cases such as matching both "female" and "male" when "male" is expected.
  true_value <- paste0("\"", true_value, "\"")

  values <- unique_values[sapply(unique_values, function(value) {
    grepl(paste0("\"", value, "\""), rules[which(grepl(true_value, rules))]) })]

  list(
    "label" = paste(
      colname, "=",
      paste(values, sep = ", ", collapse = ", ")
    ),
    "values" = as.character(values)
  )
}

#' @author Krystian Igras

`%:::%` <- function (pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

marginal_relationships <- function(explainer, new_observation, column, predicted_names, is_numerical, ...) {
  if(is_numerical) {
    ceteris <- ingredients::ceteris_paribus(
      explainer, new_observation, ...,
      variables = column)[, c(column, "_yhat_", "_label_")]
    if(all(predicted_names == "yhat")) {
      ceteris_curves <- ceteris
      colnames(ceteris_curves)[2] <- "yhat"
    } else {
      ceteris_curves <- data.frame(lapply(predicted_names, function(name) {
        result <- ceteris[ceteris$`_label_` == paste(explainer$label,
                                                     name,
                                                     sep = "."), 2,
                          drop = FALSE]
        colnames(result) <- name
        result
      }))
    }
    ceteris_curves[, column] <- unique(ceteris[, column])
  } else {
    ceteris_curves <- as.data.frame(
      explainer$predict_function(explainer$model,
                                 explainer$data)
    )
    if(ncol(ceteris_curves) == 1) {
      colnames(ceteris_curves) <- "yhat"
    }
    ceteris_curves[, column] <- explainer$data[, column]
  }
  ceteris_curves
}

fit_tree <- function(ceteris_curves, predicted_names, column, is_numerical) {
  tree_formula <- paste(
    paste(predicted_names, sep = " + ", collapse = " + "),
    column,
    sep = " ~ "
  )

  if(is_numerical) {
    max_depth <- 2
  } else {
    max_depth <- 1
  }

  partykit::ctree(as.formula(tree_formula),
                  data = ceteris_curves,
                  maxdepth = max_depth)
}

prepare_rules <- function(fitted_tree, is_numerical) {
  extract_rules <- "partykit" %:::% ".list.rules.party"
  rules <- extract_rules(fitted_tree)
  if(is_numerical) {
    rules <- sapply(rules, function(rule) {
      if(!grepl("&", rule)) {
        paste(rule, rule, sep = " & ", collapse = " & ")
      } else {
        rule
      }
    })
  }
  rules
}

#' @importFrom stats predict

make_discretization_df <- function(ceteris_curves, predicted_names,
                                   fitted_tree, column) {
  ceteris_curves <- as.data.frame(ceteris_curves)
  if(any(colnames(ceteris_curves) == "_label_")) {
    ceteris_curves <- ceteris_curves[, -which(colnames(ceteris_curves) == "_label_")]
  }

  if(length(predicted_names) > 1) {
    predictions <-  predict(fitted_tree,
                            as.data.frame(list(ceteris_curves[, column]),
                                          col.names = column))
    predictions_names <- paste(colnames(predictions), "discretization",
                               sep = "_")
    discretization <- as.vector(as.matrix(predictions))
  } else {
    discretization <- predict(fitted_tree,
                              as.data.frame(list(ceteris_curves[, column]),
                                            col.names = column))
    predictions_names <- "discretization"
  }

  which_column <- which(colnames(ceteris_curves) == column)
  output_names <- c(predicted_names, predictions_names)
  prepared_yhat <- as.vector(as.matrix(ceteris_curves[, -which_column]))
  data.frame(
    variable_name = column,
    variable = rep(ceteris_curves[, column],
                   times = length(predicted_names) + length(predictions_names)),
    output = rep(output_names, each = nrow(ceteris_curves)),
    value = c(prepared_yhat,
              discretization)
  )
}



feature_representation <- function(explainer, new_observation, column,
                                   predicted_names, ...) {
  is_numerical <- is.numeric(explainer$data[, column])

  ceteris_curves <- marginal_relationships(explainer, new_observation, column,
                                           predicted_names, is_numerical, ...)

  fitted_tree <- fit_tree(ceteris_curves, predicted_names, column, is_numerical)
  rules <- prepare_rules(fitted_tree, is_numerical)

  if(all(rules == " & ") | all(rules == "")) {
    encoded_feature <- as.factor(rep("baseline",
                                     nrow(explainer$data)))
  } else {
    if(is_numerical) {
      interpretable_input <- extract_numerical_feature(rules, new_observation[, column])
      encoded_feature <- ifelse(interpretable_input$interval[1] <= explainer$data[, column]
                                & explainer$data[, column] < interpretable_input$interval[2],
                                interpretable_input$label,
                                "baseline")
    } else {
      interpretable_input <- extract_categorical_feature(
        rules,
        new_observation[, column],
        levels(explainer$data[, column]),
        column
      )
      encoded_feature <- ifelse(
        explainer$data[, column] %in% interpretable_input$values,
        interpretable_input$label,
        "baseline"
      )
    }
  }

  list(
    factor(encoded_feature,
           levels = c("baseline", setdiff(unique(encoded_feature), "baseline"))),
    make_discretization_df(ceteris_curves, predicted_names,
                           fitted_tree, column)
  )

}
