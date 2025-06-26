test_that("import_data works", {
  test <- import_data(path = NULL)
  expect_vector(test)
  expect_s3_class(test, c("data.table", "data.frame"))
})
