test_that("diffusion_sim runs a single simulation of a drift diffusion model", {

  drift <- diffusion_sim()

  expect_vector(drift)

  expect_s3_class(drift, c("data.table", "data.frame"))

})



test_that("ezddm sets the conditions for a drift diffusion model", {

  model <- ezddm(prop_correct = .802, rt_correct_variance_seconds = .112,
                 rt_correct_mean_seconds = .723, n_trials = 100)

  expect_true(is.numeric(model))
  expect_type(model, "double")

})



test_that("read_file reads single files", {

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



test_that("theme_pcj_aesthetics formats a plot", {

  g1 <- ggplot2::ggplot(
    data = mtcars,
    mapping = ggplot2::aes(x = wt, y = mpg, color = factor(gear))
  ) +
    ggplot2::geom_point() +
    theme_pcj_aesthetics(
      base_size = 12,
      dark_text = "#000000",
      font = "Atkinson Hyperlegible"
    )

  expect_s3_class(object = g1, class = c("gg", "ggplot"))

})



test_that("theme_pcj_palettes changes the color of the plot data", {

  g1 <- ggplot2::ggplot(
    data = mtcars,
    mapping = ggplot2::aes(x = wt, y = mpg, color = factor(gear))
  ) +
    ggplot2::geom_point() +
    theme_pcj_palettes(
      palette = "ualbany",
      continuous = FALSE
    )

  expect_s3_class(object = g1, class = c("gg", "ggplot"))

})



test_that("theme_pcj_text changes the labels of the plot", {

  g1 <- ggplot2::ggplot(
    data = mtcars,
    mapping = ggplot2::aes(x = wt, y = mpg, color = factor(gear))
  ) +
    ggplot2::geom_point() +
    theme_pcj_text(
      plot_text = c(title = "Is Bigger Better?",
                    subtitle = "Lighter cars travel further!",
                    ylab = "MPG", xlab = "Weight"),
      alt_text = TRUE
    )

  expect_s3_class(object = g1, class = c("gg", "ggplot"))

})
