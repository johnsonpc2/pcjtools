test_that("drift_mod works", {

  model <- drift_mod(nsims = 500)

  expect_type(model, "list")

})
