library(testthat)
library(localModel)
library(DALEX)
library(randomForest)

data('HR')
data('apartments')

m_rf_hr <- randomForest(status ~., data = HR, ntree = 50)
m_rf_ap <- randomForest(m2.price ~., data = apartments, ntree = 50)

hr_explainer <- explain(m_rf_hr, HR,
                        predict_function = function(x, y) predict(x, y,
                                                                  type = "prob"))
ap_explainer <- explain(m_rf_ap, apartments)

local_model_explainer_hr <- individual_surrogate_model(hr_explainer,
                                                       HR[5, -6],
                                                       size = 500)
local_model_explainer_ap <- individual_surrogate_model(ap_explainer,
                                                       apartments[5, -1],
                                                       size = 500)

test_check("localModel")
