
test_that("corner locations are correct", {

  spec <- new_plate_spec(
    width = 100, height = 100,
    well_spacing = 20, well_diameter = 15,
    nrow = 4, ncol = 4
  )

  g <- guide_wells(
    theme_grey(),
    x.major = c(0.25, 0.75),
    y.major = c(0.25, 0.75),
    specs   = spec,
    corner  = "all"
  )

  panel <- g$children[[1]]

  expect_length(panel$x, 8)
  expect_length(panel$y, 8)

  g <- guide_wells(
    theme_grey(),
    x.major = c(0.25, 0.75),
    y.major = c(0.25, 0.75),
    specs   = spec,
    corner  = "none"
  )

  panel <- g$children[[1]]

  expect_length(panel$x, 9 * 4)
  expect_length(panel$y, 9 * 4)

  g <- guide_wells(
    theme_grey() + theme(panel.background = element_blank()),
    x.major = c(0.25, 0.75),
    y.major = c(0.25, 0.75),
    specs   = spec,
    corner  = "none"
  )

  panel <- g$children[[1]]

  expect_s3_class(panel, "zeroGrob")
  expect_s3_class(g$children[[4]], "circle")

})
