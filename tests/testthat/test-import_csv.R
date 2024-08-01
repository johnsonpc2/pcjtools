test_that("import_csv works", {
  import_csv(file.type = "csv")
  expect_vector(raw.data)
  expect_s3_class(raw.data, c("data.table", "data.frame"))
})
