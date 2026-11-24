pos <- function(x, y) {
  tibble::tibble(object = seq_along(x), x = x, y = y)
}

test_that("returns TRUE when all pairs are farther apart than the minimum", {
  expect_true(is_distance_at_least(pos(c(0, 10, 20), c(0, 0, 0)), 5))
})

test_that("returns FALSE when any pair is closer than the minimum", {
  expect_false(is_distance_at_least(pos(c(0, 10, 11), c(0, 0, 0)), 5))
})

test_that("a distance exactly equal to the minimum is accepted", {
  # 3-4-5 triangle gives an exact distance of 5
  p <- pos(c(0, 3), c(0, 4))
  expect_true(is_distance_at_least(p, 5))
  expect_false(is_distance_at_least(p, 5 + 1e-9))
})

test_that("distance is Euclidean in both coordinates", {
  p <- pos(c(0, 1), c(0, 1)) # distance sqrt(2)
  expect_true(is_distance_at_least(p, 1.4))
  expect_false(is_distance_at_least(p, 1.5))
})

test_that("the closest pair decides, wherever it is in the table", {
  x <- c(0, 100, 200, 300, 301)
  expect_false(is_distance_at_least(pos(x, rep(0, 5)), 2))
  expect_true(is_distance_at_least(pos(x, rep(0, 5)), 1))
  expect_false(is_distance_at_least(pos(rev(x), rep(0, 5)), 2))
})

test_that("works for the example from the documentation", {
  example_pos <- tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
  expect_true(is_distance_at_least(example_pos, 1))
  expect_false(is_distance_at_least(example_pos, 2))
})

test_that("zero minimum distance is always satisfied", {
  expect_true(is_distance_at_least(pos(c(1, 1), c(2, 2)), 0))
})

test_that("coincident objects violate any positive minimum", {
  expect_false(is_distance_at_least(pos(c(1, 1), c(2, 2)), 0.001))
})

test_that("a single object has no pairs and passes", {
  expect_true(is_distance_at_least(pos(1, 1), 100))
})

test_that("negative coordinates are handled", {
  p <- pos(c(-3, 0), c(-4, 0))
  expect_true(is_distance_at_least(p, 5))
  expect_false(is_distance_at_least(p, 5.1))
})

test_that("extra columns and row order do not matter", {
  p <- pos(c(0, 10, 20), c(0, 0, 0)) |>
    dplyr::mutate(direction = 1, speed = 2)
  expect_equal(
    is_distance_at_least(p, 5),
    is_distance_at_least(p[c(3, 1, 2), ], 5)
  )
})

test_that("missing coordinates do not give a definite TRUE", {
  p <- pos(c(0, NA), c(0, 0))
  expect_false(is_distance_at_least(p, 1))
})
