context("is_permutation")

test_that("is_permutation", {
  expect_true(is_permutation(shuffle(1:10), s = 1:10))
  expect_false(is_permutation(shuffle(letters[1:3]), s = letters[1:4]))
  expect_false(is_permutation(1:3, 1:2))
  expect_false(is_permutation(c(1, 1, 1), 1:3))
})

