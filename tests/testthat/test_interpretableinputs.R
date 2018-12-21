context("Functions that create interpretable inputs")


testthat::test_that("Extracting numerical features is okay", {
  testthat::expect_equal(
    extract_numerical_feature(c("x > 4", "x <= 4"), 5)$label,
    "x > 4"
  )
  testthat::expect_equal(
    extract_numerical_feature(c("x > 4", "x <= 4"), 4)$label,
    "x <= 4"
  )
  testthat::expect_equal(
    extract_numerical_feature(c("x <= 4 & x <= 5",
                                "x > 5 & x <= 8"), 3)$label,
    "x <= 4"
  )
})
