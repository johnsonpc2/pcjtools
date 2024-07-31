test_that("memory_model works", {
  output <- memory_model()
  expect_length(output, 6)
})
