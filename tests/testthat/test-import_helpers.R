test_that("import_helpers-files_info", {

  info <- files_info(path = NULL, extension = "csv")

  expect_vector(info)

  expect_s3_class(info, c("data.table", "data.frame"))

  expect_length(info, 4)

})


test_that("import_helpers-read_file_list", {

  data <- read_file_list(files = files_info())

  expect_vector(data)

  expect_s3_class(data, c("data.table", "data.frame"))

  expect_length(data, 36)

})
