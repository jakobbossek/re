context("is.permutation")

test_that("is.permutation", {
  expect_true(is.permutation(shuffle(1:10), s = 1:10))
  expect_false(is.permutation(shuffle(letters[1:3]), s = letters[1:4]))
  expect_false(is.permutation(1:3, 1:2))
})

