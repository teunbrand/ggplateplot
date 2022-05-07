
test_that("Standard plate formats look the same", {

  m <- matrix(1:384, nrow = 16, ncol = 24)

  p <- ggplateplot(m)

  vdiffr::expect_doppelganger("384-well plate", p)

  p <- ggplateplot(m[1:8, 1:12])

  vdiffr::expect_doppelganger("96-well plate", p)

  p <- ggplateplot(m[1:6, 1:8])

  vdiffr::expect_doppelganger("48-well plate", p)

  p <- ggplateplot(m[1:4, 1:6])

  vdiffr::expect_doppelganger("24-well plate", p)

  p <- ggplateplot(m[1:3, 1:4])

  vdiffr::expect_doppelganger("12-well plate", p)

  p <- ggplateplot(m[1:2, 1:3])

  vdiffr::expect_doppelganger("6-well plate", p)
})

test_that("We can set custom plates", {

  m <- matrix(1:4, 2, 2)

  spec <- custom_plate_spec(2, 2, width = 100, height = 100)
  expect_snapshot(spec)

  p <- ggplot(melt_array(m), aes(Var2, Var1)) +
    coord_plate(spec = spec)

  vdiffr::expect_doppelganger("custom plate", p)

})

test_that("Auto-choosing invalid layout gives appropriate error", {

  expect_snapshot_error(choose_wellplate(1))

})
