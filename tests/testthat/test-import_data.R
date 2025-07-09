test_that("files_info example", {

  info <- files_info()

  expect_s3_class(info, c("data.table"))

  expect_length(info, 6)

})

test_that("import_data example", {

  info <- files_info()

  test <- import_data(x = info)

  expect_vector(test)

  expect_s3_class(test, c("data.table", "data.frame"))

})
