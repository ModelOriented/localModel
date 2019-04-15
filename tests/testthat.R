library(testthat)
library(localModel)
library(DALEX)
library(randomForest)

HR <- get("HR", asNamespace('utils'))

test_check("localModel")
