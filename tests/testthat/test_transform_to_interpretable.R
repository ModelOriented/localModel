context("Test transform_to_interpretable")

test_that("Column names correspond to row values", {
  # Test data
  exp <- list(data = DALEX::titanic_imputed[1:10, -8])
  new <- DALEX::titanic_imputed[0, -8]
  new[1, ] <- list(gender = "male", age = 8, class = "1st",
                   embarked = "Southampton", fare = 72,
                   sibsp = 0, parch = 0)
  feature_rep <- list(
    list(factor(c(2, 1, 2), labels = c("baseline", "gender = male"))),
    list(factor(c(1, 2, 1), labels = c("baseline", "age <= 15.36"))),
    list(factor(c(1, 1, 2),
                labels = c("baseline", "class = 1st, 2nd, deck crew"))),
    list(factor(c(2, 1, 2),
                labels = c("baseline", "embarked = Belfast, Southampton"))),
    list(factor(c(1, 1, 1), labels = c("baseline"))),
    list(factor(c(1, 1, 1), labels = c("baseline"))),
    list(factor(c(1, 1, 1), labels = c("baseline")))
  )

  check_column_name <- function(df) {
    all(sapply(seq_along(df), function(x) {
      all(grepl(paste0(names(df)[x], "|baseline"), df[, x]))
    }))
  }

  # For assigning column names, those of the dataframe in the explainer
  # are compared to those of the dataframe with the new observation

  # Number and ordering of columns is the same
  df <- transform_to_interpretable(exp, new, feature_rep)
  testthat::expect_true(check_column_name(df))

  # Number of columns is different
  new$extra <- "dummy data"
  df <- transform_to_interpretable(exp, new, feature_rep)
  testthat::expect_true(check_column_name(df))
  new$extra <- NULL

  # Ordering of columns is different
  new <- new[, c("class", "gender", "age", "sibsp",
                 "parch", "fare", "embarked")]
  df <- transform_to_interpretable(exp, new, feature_rep)
  testthat::expect_true(check_column_name(df))
})
