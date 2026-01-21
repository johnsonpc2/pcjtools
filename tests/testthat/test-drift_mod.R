test_that("drift_mod works", {

  model <- drift_mod(nsims = 500)

  expect_type(model, "list")
  expect_length(model, 4)
  expect_s3_class(model[[1]], "data.table")
  expect_s3_class(model[[2]], "ggplot")

})
