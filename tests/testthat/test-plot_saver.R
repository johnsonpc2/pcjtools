test_that("plot_saver can save one or more plots", {

  g1 <- ggplot2::ggplot(
    data = mtcars,
    mapping = ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
  )

  tdir = tempdir()

  plot_saver(plot = g1, dir = tdir)

  file <- paste0(tdir, "\\", format(Sys.time(), "%Y%m%d_"), "Plot.png")

  expect_true(object = file.exists(file))

  if (file.exists(file)) file.remove(file)

})
