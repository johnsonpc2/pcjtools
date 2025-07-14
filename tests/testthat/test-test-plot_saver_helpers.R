test_that("is.ggplot works", {

  g1 <- ggplot2::ggplot(
    data = mtcars,
    ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
  )

  expect_true(object = is.ggplot(x = g1))

})

test_that("save_single_plot works", {

  g1 <- ggplot2::ggplot(
    data = mtcars,
    ggplot2::aes(x = mpg, y = wt, color = factor(cyl))
  )

  save_single_plot(plot = g1, filename = "./ExamplePlot.png")

  expect_true(object = file.exists("./ExamplePlot.png"))

  if (file.exists("./ExamplePlot.png")) {

    file.remove("./ExamplePlot.png")

  }

})
