test_that("read_file reads files", {

  info <- files_info()

  file <- read_file(x = info$filepath)

  expect_vector(file)

  expect_s3_class(file, c("data.table", "data.frame"))

  expect_length(file, 36)

})



test_that("read_file_list concatenates a list of files", {

  info <- files_info()

  data <- read_file_list(files = info$filepath)

  expect_vector(data)

  expect_s3_class(data, c("data.table", "data.frame"))

  expect_length(data, 36)

})
