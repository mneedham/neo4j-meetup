

y = function() {
  "initial y"
}

x = function(fn = y) {
  print(paste0("y is ",  fn()))
}

x()
x(fn = function() {
  "new y"
})


test_that("trigonometric functions match identities", {
  expect_that(sin(pi / 4), equals(1 / sqrt(2)))
  expect_that(cos(pi / 4), equals(1 / sqrt(2)))
  expect_that(tan(pi / 4), equals(1))
})

test_that("trigonometric functions match identities", {
  expect_that(sin(pi / 4), equals(1))
})