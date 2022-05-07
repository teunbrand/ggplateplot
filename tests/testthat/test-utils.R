test_that("replace waiver replaces waivers", {
  x <- waiver()
  expect_equal(replace_waiver(x, 2), 2)
  expect_equal(replace_waiver(1, 2), 1)
})

test_that("proect_zero_length returns NULL when length is zero", {
  expect_equal(protect_zero_length(1:2), 1:2)
  expect_null(protect_zero_length(numeric(), NULL))
})
