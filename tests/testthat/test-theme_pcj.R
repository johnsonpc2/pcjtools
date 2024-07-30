test_that("theme_pcj works", {
  g1 <- ggplot2::ggplot(data = mtcars, ggplot2::aes(x = mpg, y = wt, color = factor(cyl))) + ggplot2::geom_point()
  thm <- theme_pcj(ggplot.object = g1, base.size = 10)
  expect_s3_class(thm, "ggplot")
  expect_equal(thm$theme$text$size, 10)
  expect_length(thm, 11)
})
