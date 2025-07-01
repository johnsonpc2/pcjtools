test_that("import_data example", {

  test <- import_data(extension = "csv")

  expect_vector(test)

  expect_s3_class(test, c("data.table", "data.frame"))

})
