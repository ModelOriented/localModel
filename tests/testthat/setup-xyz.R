################################################################################
##        Test Setup: The code in this file runs before tests are run         ##
################################################################################
## Clean up the envrionment
suppressWarnings(rm(HR, apartments))

# Get the data
data("HR", package = "DALEX")
data("apartments", package = "DALEX")

## Fit models
m_rf_hr <- randomForest::randomForest(status ~., data = HR[1:500, ], ntree = 10)
m_rf_ap <- randomForest::randomForest(m2.price ~., data = apartments[1:500, ], ntree = 10)

## Create model explainers
hr_explainer <- DALEX::explain(m_rf_hr, HR[1:500, ])
ap_explainer <- DALEX::explain(m_rf_ap, apartments[1:500, ])

## Generate surrogate models
local_model_explainer_hr <-
  localModel::individual_surrogate_model(hr_explainer,
                                         HR[5, -6],
                                         size = 50,
                                         seed = 17)
local_model_explainer_ap <-
  localModel::individual_surrogate_model(ap_explainer,
                                         apartments[5, -1],
                                         size = 50,
                                         seed = 17)
