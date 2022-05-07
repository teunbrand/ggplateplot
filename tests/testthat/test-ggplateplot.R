
test_that("ggplateplot array method works as intended", {

  m <- matrix(1:6, 2, 3, dimnames = list(A = 1:2, B = LETTERS[1:3]))

  p <- ggplateplot(m)

  expect_equal(p$labels, list(x = "B", y = "A"))
  expect_equal(dim(p$data), c(6, 3))

})

test_that("ggplateplot default method finds appropriate plate", {

  df <- expand.grid(x = 1:3, y = 1:2)

  p <- ggplateplot(df, aes(x = x, y = y))

  expect_equal(p$coordinates$specs, spec_6well)
})
