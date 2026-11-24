# straight line along x, time 0..5, one object
line_traj <- function(object = 1, y = 0, speed = 1, times = 0:5) {
  tibble::tibble(time = times, object = object, x = speed * times, y = y)
}
# L-shaped path: right for t = 0..3, then up for t = 3..6
corner_traj <- function(object = 1) {
  tibble::tibble(
    time = 0:6, object = object,
    x = c(0, 1, 2, 3, 3, 3, 3),
    y = c(0, 0, 0, 0, 1, 2, 3)
  )
}

test_that("straight line keeps only first and last time points", {
  tt <- line_traj()
  res <- simplify_trajectory(tt, snapshot = FALSE)
  expect_equal(res$time, c(0, 5))
})

test_that("corner is kept together with first and last point", {
  res <- simplify_trajectory(corner_traj(), snapshot = FALSE)
  expect_equal(res$time, c(0, 3, 6))
  expect_equal(res$x, c(0, 3, 3))
  expect_equal(res$y, c(0, 0, 3))
})

test_that("simplified trajectory reproduces the original by interpolation", {
  tt <- corner_traj()
  res <- simplify_trajectory(tt, snapshot = FALSE)
  for (tp in c(1, 2, 4, 5)) {
    est <- estimate_position_for_time(res, tp)
    orig <- tt[tt$time == tp, ]
    expect_equal(est$x, orig$x)
    expect_equal(est$y, orig$y)
  }
})

test_that("non-snapshot output has the original columns and values", {
  tt <- corner_traj() |> dplyr::mutate(direction = 0.123, speed = 1)
  res <- simplify_trajectory(tt, snapshot = FALSE)
  expect_equal(names(res), names(tt))
  # direction is carried over from the input, not recomputed
  expect_true(all(res$direction == 0.123))
})

test_that("output is a subset of the input rows", {
  tt <- dplyr::bind_rows(corner_traj(1), line_traj(2, y = 5))
  for (snap in c(TRUE, FALSE)) {
    res <- simplify_trajectory(tt, snapshot = snap)
    expect_equal(nrow(dplyr::anti_join(res, tt, by = names(tt))), 0)
  }
})

test_that("snapshot keeps all objects at every critical time", {
  tt <- dplyr::bind_rows(corner_traj(1), line_traj(2, y = 5, times = 0:6))
  res <- simplify_trajectory(tt, snapshot = TRUE)
  expect_equal(sort(unique(res$time)), c(0, 3, 6))
  counts <- table(res$time)
  expect_true(all(counts == 2))
})

test_that("non-snapshot keeps only changing points per object", {
  tt <- dplyr::bind_rows(corner_traj(1), line_traj(2, y = 5, times = 0:6))
  res <- simplify_trajectory(tt, snapshot = FALSE)
  expect_equal(res$time[res$object == 1], c(0, 3, 6))
  expect_equal(res$time[res$object == 2], c(0, 6))
})

test_that("unsorted input gives the same result as sorted input", {
  tt <- corner_traj()
  shuffled <- tt[c(5, 2, 7, 1, 4, 3, 6), ]
  a <- simplify_trajectory(tt, snapshot = FALSE)
  b <- simplify_trajectory(shuffled, snapshot = FALSE)
  expect_equal(dplyr::arrange(b, time), a)
})

test_that("direction changes smaller than eps are ignored", {
  tt <- line_traj(speed = 1)
  tt$y[4] <- 1e-6 # tiny wobble
  res <- simplify_trajectory(tt, snapshot = FALSE, eps = 1e-3)
  expect_equal(res$time, c(0, 5))
  res_strict <- simplify_trajectory(tt, snapshot = FALSE, eps = 1e-9)
  expect_gt(nrow(res_strict), 2)
})

test_that("a path crossing direction angle 0 does not create false corners", {
  # heading is ~0 with tiny jitter on both sides of the 0 / 2*pi boundary
  tt <- tibble::tibble(
    time = 0:4, object = 1,
    x = 0:4,
    y = c(0, 1e-7, 0, 1e-7, 0)
  )
  res <- simplify_trajectory(tt, snapshot = FALSE)
  expect_equal(res$time, c(0, 4))
})

test_that("stationary object keeps first and last point only", {
  tt <- tibble::tibble(time = 0:4, object = 1, x = 2, y = 3)
  res <- simplify_trajectory(tt, snapshot = FALSE)
  expect_equal(res$time, c(0, 4))
})

test_that("two-point trajectory is returned unchanged", {
  tt <- line_traj(times = c(0, 1))
  expect_equal(simplify_trajectory(tt, snapshot = FALSE), tt)
})

test_that("real trajectory shrinks and keeps start and end", {
  set.seed(42)
  sett <- new_settings(
    speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
    bounce_off_square = FALSE, bounce_off_circle = TRUE
  )
  tt <- make_random_trajectory(
    position8c |> add_random_direction(), seq(0, 4, by = 0.1),
    sett, step_direct
  )
  res <- simplify_trajectory(tt, snapshot = TRUE)
  expect_lt(nrow(res), nrow(tt))
  expect_true(all(c(0, 4) %in% res$time))
})
