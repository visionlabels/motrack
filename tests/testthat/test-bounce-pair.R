context("Pairwise collision calculations")

v1 <- c(0, 1)
v2 <- c(0, -1)
v3 <- c(0, -2)
vleft <- c(-1, 0)
vright <- c(1, 0)
a30 <- pi / 6
a60 <- pi / 3
# known directions, 0 = right, counter-clockwise, y > 0 is up
v210 <- c(cos(210 * pi / 180), sin(210 * pi / 180))
v030 <- c(cos( 30 * pi / 180), sin( 30 * pi / 180))

test_that("Direct collision swaps velocities", {
  # same velocity
  b <- bounce_pair(v1, v2, c(0, 0), c(0, 1))
  expect_equal(b[[1]], v2)
  expect_equal(b[[2]], v1)
  # different velocity
  b <- bounce_pair(v1, v3, c(0, 0), c(0, 1))
  expect_equal(b[[1]], v3)
  expect_equal(b[[2]], v1)
})

test_that("Velocity is retained when two objects with same velocities collide", {
  # same vectors, only change angle/distance
  b <- bounce_pair(v1, v2, c(0, 0), c(0, 1))
  expect_equal(sum(b[[1]] ^ 2), 1, tolerance = .001)
  b <- bounce_pair(v1, v2, c(0, 0), c(0, 2))
  expect_equal(sum(b[[1]] ^ 2), 1, tolerance = .001)
  b <- bounce_pair(v1, v2, c(0, 0), c(0, 3))
  expect_equal(sum(b[[1]] ^ 2), 1, tolerance = .001)
  b <- bounce_pair(v1, v2, c(0, 0), c(0, 4))
  expect_equal(sum(b[[1]] ^ 2), 1, tolerance = .001)
})


test_that("Bouncing works for known angles", {
  # 45 deg - hit on left side, turns left, the other right
  b <- bounce_pair(v1, v2, c(0, 0), c(1, 1))
  expect_equal(b[[1]], vleft)
  expect_equal(b[[2]], vright)
  # 45 deg - hit on right side, turns right, the other left
  b <- bounce_pair(v1, v2, c(0, 0), c(1, 1))
  expect_equal(b[[1]], vleft)
  expect_equal(b[[2]], vright)
  # 30 deg
  b <- bounce_pair(v1, v2, c(0, 0), c(sin(a30), cos(a30)))
  expect_equal(b[[1]], v210)
  expect_equal(b[[2]], v030)
})
