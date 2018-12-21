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
  values <- unique_values[sapply(unique_values, function(value) {
    grepl(value, rules[which(grepl(true_value, rules))]) })]

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

feature_representation <- function(explainer, new_observation, column,
                                   predicted_names, grid_points = 101) {
  is_numerical <- is.numeric(explainer$data[, column])

  if(is_numerical) {
    ceteris <- ceterisParibus2::individual_variable_profile(
      explainer, new_observation, grid_points = grid_points,
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
    ceteris_curves[, column] <- ceteris[1:grid_points, column]
  } else {
    ceteris_curves <- as.data.frame(
      explainer$predict_function(explainer$model,
                                 explainer$data)
    )
    if(ncol(ceteris_curves) == 1)
      colnames(ceteris_curves) <- "yhat"
    ceteris_curves[, column] <- explainer$data[, column]
  }

  tree_formula <- paste(
    paste(predicted_names, sep = " + ", collapse = " + "),
    column,
    sep = " ~ "
  )

  if(is_numerical) {
    fitted_tree <- partykit::ctree(as.formula(tree_formula),
                                   data = ceteris_curves,
                                   maxdepth = 2)
  } else {
    fitted_tree <- partykit::ctree(as.formula(tree_formula),
                                   data = ceteris_curves,
                                   maxdepth = 1)
  }

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

  if(all(rules == " & ") | all(rules == "")) {
    encoded_feature <- as.factor(rep("baseline",
                                     nrow(explainer$data)))
  } else {
    if(is_numerical) {
      interpretable_input <- extract_numerical_feature(rules,
                                                       new_observation[, column])
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

  factor(encoded_feature,
         levels = c("baseline", setdiff(unique(encoded_feature), "baseline")))
}
