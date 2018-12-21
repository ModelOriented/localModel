library(testthat)
library(localModel)
library(DALEX2)
library(randomForest)

data('HR')
data('apartments')

m_rf_hr <- randomForest(status ~., data = HR[1:500, ], ntree = 10)
m_rf_ap <- randomForest(m2.price ~., data = apartments[1:500, ], ntree = 10)

hr_explainer <- explain(m_rf_hr, HR[1:500, ])
ap_explainer <- explain(m_rf_ap, apartments[1:500, ])

local_model_explainer_hr <- individual_surrogate_model(hr_explainer,
                                                       HR[5, -6],
                                                       size = 50,
                                                       seed = 17)
local_model_explainer_ap <- individual_surrogate_model(ap_explainer,
                                                       apartments[5, -1],
                                                       size = 50,
                                                       seed = 17)

test_check("localModel")
