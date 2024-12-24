test_that("theme_pcj works", {
  g1 <- ggplot2::ggplot(
    data = mtcars,
    ggplot2::aes(x = mpg, y = wt, color = factor(cyl), size = cyl)
  ) +
    ggplot2::geom_point()
  thm <- theme_pcj(ggplot_object = g1, base_size = 10,
                   graph_text = c(
                     title = "MPG of Cars Based on Weight and Cylinders",
                     ylab = "Weight",
                     xlab = "MPG",
                     caption = paste("Revised:", Sys.time())),
                   show_caption = TRUE)
  expect_s3_class(thm, "ggplot")
  expect_equal(thm$theme$text$size, 10)
  expect_length(thm, 11)
})
