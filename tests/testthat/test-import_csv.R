test_that("import_csv works", {
  test <- import_csv(file.type = "csv")
  expect_vector(test)
  expect_s3_class(test, c("data.table", "data.frame"))
})
