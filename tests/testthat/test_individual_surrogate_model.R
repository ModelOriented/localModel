context("Test individual_surrogate_model")


# random forest and two classes -----------------------------------------------

test_that("individual_surrogate_model works with random forest and two classes", {
  ###########
  ## Setup ##
  ###########
  ## Get the data
  data("HR", package = "DALEX")
  ## Preprocess the data
  HR <- subset(HR, status %in% c("fired", "promoted"))
  ### Set the class of interest to "promoted"
  HR$status <- factor(HR$status, levels = c("fired", "promoted"))
  ## Fit a RF model
  mdl_obj <- randomForest::randomForest(status ~., data = HR[1:500, ], ntree = 10)
  ## Create model explainer
  mdl_exp <- DALEX::explain(model = mdl_obj,
                            data = HR[1:500, ])


  ###########################
  ## Valid Input Arguments ##
  ###########################
  ## Generate a surrogate model
  expect_silent(
    mdl_sur <- individual_surrogate_model(x = mdl_exp,
                                          new_observation = subset(HR[5,], select = -status),
                                          size = 50,
                                          seed = 17)
  )
  ## Plot the surrogate model
  expect_true("ggplot" %in% class(plot(mdl_sur)))
})

# glm and two classes -----------------------------------------------

test_that("individual_surrogate_model works with glm and two classes", {
  ###########
  ## Setup ##
  ###########
  ## Get the data
  data("HR", package = "DALEX")
  ## Preprocess the data
  HR <- subset(HR, status %in% c("fired", "promoted"))
  ### Set the class of interest to "promoted"
  HR$status <- factor(HR$status, levels = c("fired", "promoted"))
  ## Fit a RF model
  mdl_obj <- glm(status ~., family = binomial, data = HR[1:500, ])
  ## Create model explainer
  mdl_exp <- DALEX::explain(model = mdl_obj,
                            data = HR[1:500, ])


  ###########################
  ## Valid Input Arguments ##
  ###########################
  ## Generate a surrogate model
  expect_silent(
    mdl_sur <- individual_surrogate_model(x = mdl_exp,
                                          new_observation = subset(HR[5,], select = -status),
                                          size = 50,
                                          seed = 17)
  )
  ## Plot the surrogate model
  expect_true("ggplot" %in% class(plot(mdl_sur)))
})
