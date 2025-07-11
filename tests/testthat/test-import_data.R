test_that("files_info runs", {

  info <- files_info()

  expect_s3_class(info, c("data.table"))

  expect_length(info, 6)

})

test_that("import_data imports", {

  info <- files_info()

  file <- import_data(x = info$filepath)

  expect_vector(file)

  expect_s3_class(file, c("data.table", "data.frame"))

  expect_length(file, 36)

})
