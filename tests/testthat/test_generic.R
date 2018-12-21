context("Test generics")

testthat::test_that("Plots work with different geoms", {
  testthat::expect_silent({
    plot(local_model_explainer_ap)
    plot(local_model_explainer_ap, geom = "bar")
    plot(local_model_explainer_ap, geom = "arrow")
  })
})

testthat::test_that("Print is okay", {
  testthat::expect_output({
    print(local_model_explainer_ap)
  })
})

testthat::test_that("Kernels work fine", {
  testthat::expect_true({
    identity_kernel(HR[2, ], HR[1, ]) == 1
  })
  testthat::expect_false({
    gaussian_kernel(HR[2, 2:5], HR[1, 2:5]) == 1
  })
})
