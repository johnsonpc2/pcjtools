test_that("drift_mod works", {

  model <- drift_mod(nsims = 200)

  expect_type(model, "list")

})
