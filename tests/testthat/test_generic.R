context("Test generics")

testthat::expect_silent({
  plot(local_model_explainer_ap)
  plot(local_model_explainer_ap, geom = "bar")
})

testthat::expect_output({
  print(local_model_explainer_ap)
})
