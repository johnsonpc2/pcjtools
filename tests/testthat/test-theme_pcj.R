test_that("theme_pcj works", {
  g1 <- ggplot2::ggplot(
    data = mtcars,
    ggplot2::aes(x = mpg, y = wt, color = factor(cyl), size = wt)
  ) +
    ggplot2::geom_point()
  thm <- theme_pcj(plot = g1, base_size = 10,
                   plot_text = c(title = "MPG of Cars Based on Weight and
                                 Cylinders",
                                 subtitle = "Look how weight effects MPG",
                                 ylab = "Weight",
                                 xlab = "MPG"),
                   font = "sans",
                   alt_text = TRUE)
  expect_s3_class(thm, "ggplot")
  expect_equal(thm$theme$text$size, 10)
  expect_length(thm, 11)
})

test_that("theme_pcj works with lines", {
  g1 <- ggplot2::ggplot(
    data = mtcars,
    ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
  ) +
    ggplot2::geom_line(linewidth = 2)
  thm <- theme_pcj(plot = g1, base_size = 10,
                   plot_text = c(title = "MPG of Cars Based on
                                 Weight and Cylinders",
                                 ylab = "Weight",
                                 xlab = "MPG"),
                   font = "sans",
                   alt_text = TRUE)

  expect_s3_class(thm, "ggplot")
  expect_equal(thm$theme$text$size, 10)
  expect_length(thm, 11)

  file <- paste0("./", format(Sys.time(), "%Y%m%d_"),
                 "MPG of Cars Based on Weight and Cylinders", ".png")

  if (file.exists(file)) {

    file.remove()

  }

})
