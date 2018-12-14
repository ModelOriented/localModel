context("Test main functions")


testthat::test_that(
  "Function works with single- and multi-column output",
  testthat::expect_output(individual_surrogate_model(hr_explainer, HR[5, -6], 50),
                          regexp = NA)
  )

testthat::test_that(
  "Reproducibility", {
  testthat::expect_equal(
    individual_surrogate_model(hr_explainer, HR[5, -6], 50, seed = 17),
    individual_surrogate_model(hr_explainer, HR[5, -6], 50, seed = 17)
  )
  testthat::expect_false(
    all(
      individual_surrogate_model(hr_explainer, HR[5, -6], 50) ==
        individual_surrogate_model(hr_explainer, HR[5, -6], 50)
    )
  )
})

testthat::test_that(
  "Non-uniform sampling is okay", {
    testthat::expect_output(
      individual_surrogate_model(hr_explainer, HR[5, -6], 50,
                                 sampling = "non-uniform"),
      regexp = NA
    )
  }
)
