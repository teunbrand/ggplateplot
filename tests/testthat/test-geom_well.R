
test_that("vanilla geom_well adopts size from scales", {

  p <- ggplot(mapping = aes(x = 1, y = 1)) +
    geom_well()

  test <- p +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2))

  l <- layer_grob(test)[[1]]

  expect_equal(as.numeric(l$r), 0.25)

  test <- p +
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 3)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 3))

  l <- layer_grob(test)[[1]]

  expect_equal(as.numeric(l$r), 0.125)

  test <- p +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 3))

  l <- layer_grob(test)[[1]]

  expect_equal(as.numeric(l$r), 0.125)

  test <- p +
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 3)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2))

  l <- layer_grob(test)[[1]]

  expect_equal(as.numeric(l$r), 0.125)
})

test_that("geom_well adapts in plate context", {
  spec <- new_plate_spec(
    width = 100, height = 100,
    well_spacing = 20, well_diameter = 20,
    ncol = 4, nrow = 4
  )

  df <- expand.grid(x = 1:4, y = 1:4)

  p <- ggplot(df, aes(x, y)) +
    geom_well()

  l <- layer_grob(p + coord_plate(spec = spec))[[1]]

  expect_equal(as.numeric(l$r), rep(0.1, 16))

  spec$well_diameter <- 10

  l <- layer_grob(p + coord_plate(spec = spec))[[1]]

  expect_equal(as.numeric(l$r), rep(0.05, 16))
})
