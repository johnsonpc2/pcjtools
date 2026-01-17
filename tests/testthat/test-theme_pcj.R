test_that("theme_pcj works", {
  g1 <- ggplot2::ggplot(
    data = mtcars,
    ggplot2::aes(x = wt, y = mpg, color = factor(cyl), size = wt)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "MPG of Cars Based on Weight and Cylinders",
      subtitle = "Look how weight effects MPG",
      y = "MPG",
      x = "Weight"
    ) +
    theme_pcj(font = "sans")

  expect_s3_class(g1, "ggplot")
})

test_that("theme_pcj works with lines", {
  g2 <- ggplot2::ggplot(
    data = mtcars,
    ggplot2::aes(x = wt, y = mpg, color = factor(cyl))
  ) +
    ggplot2::geom_line(linewidth = 2) +
    ggplot2::labs(
      title = "MPG of Cars Based on Weight and Cylinders",
      ylab = "MPG",
      xlab = "Weight"
    ) +
    theme_pcj(font = "sans")

  expect_s3_class(g2, "ggplot")

})
