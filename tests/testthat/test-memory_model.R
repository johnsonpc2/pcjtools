test_that("theme_pcj works", {
  thm <- theme_pcj(base_size = 12)
  expect_s3_class(thm, "theme")
  expect_equal(thm$text$size, 12)
})
