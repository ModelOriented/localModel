context("Test individual_surrogate_model")

test_that("individual_surrogate_model works", {
  ###########
  ## Setup ##
  ###########
  ## Get the data
  data("HR", package = "DALEX")
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
