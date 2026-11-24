# two objects, three time points: t = 0, 2, 6
make_traj <- function() {
  tibble::tibble(
    time = rep(c(0, 2, 6), each = 2),
    object = rep(1:2, times = 3),
    x = c(0, 10, 4, 10, 12, 10),
    y = c(0, 0, 2, 4, 6, 8),
    speed = 1
  )
}

test_that("midpoint between two time points is interpolated linearly", {
  res <- estimate_position_for_time(make_traj(), 1)
  expect_equal(res$x, c(2, 10))
  expect_equal(res$y, c(1, 2))
})

test_that("interpolation uses the correct interval, not the whole range", {
  # between t = 2 and t = 6, a quarter of the way is t = 3
  res <- estimate_position_for_time(make_traj(), 3)
  expect_equal(res$x, c(4 + 0.25 * 8, 10))
  expect_equal(res$y, c(2 + 0.25 * 4, 4 + 0.25 * 4))
})

test_that("timepoint equal to a recorded time returns that time's rows", {
  tt <- make_traj()
  res <- estimate_position_for_time(tt, 2)
  expect_equal(res, dplyr::filter(tt, time == 2))
})

test_that("first and last time points are valid inputs", {
  tt <- make_traj()
  expect_equal(estimate_position_for_time(tt, 0), dplyr::filter(tt, time == 0))
  expect_equal(estimate_position_for_time(tt, 6), dplyr::filter(tt, time == 6))
})

test_that("timepoint outside the trajectory range is an error", {
  tt <- make_traj()
  expect_error(estimate_position_for_time(tt, -0.1))
  expect_error(estimate_position_for_time(tt, 6.1))
})

test_that("result has one row per object ordered by object", {
  tt <- make_traj() |> dplyr::arrange(dplyr::desc(object), time)
  res <- estimate_position_for_time(tt, 1)
  expect_equal(res$object, 1:2)
})

test_that("unsorted input gives the same result as sorted input", {
  tt <- make_traj()
  shuffled <- tt[c(5, 2, 6, 1, 4, 3), ]
  expect_equal(
    estimate_position_for_time(shuffled, 1),
    estimate_position_for_time(tt, 1)
  )
})

test_that("non-coordinate columns come from the earlier time point", {
  tt <- make_traj() |>
    dplyr::mutate(speed = time + 100, direction = time / 10)
  res <- estimate_position_for_time(tt, 1)
  expect_equal(res$time, c(0, 0))
  expect_equal(res$speed, c(100, 100))
  expect_equal(res$direction, c(0, 0))
})

test_that("columns and number of columns are preserved", {
  tt <- make_traj()
  expect_equal(names(estimate_position_for_time(tt, 1)), names(tt))
})

test_that("a single object works", {
  tt <- dplyr::filter(make_traj(), object == 1)
  res <- estimate_position_for_time(tt, 4)
  expect_equal(nrow(res), 1)
  expect_equal(res$x, 4 + 0.5 * 8)
})

test_that("negative and fractional times are handled", {
  tt <- tibble::tibble(
    time = rep(c(-1, -0.5), each = 1), object = 1,
    x = c(0, 1), y = c(0, -2)
  )
  res <- estimate_position_for_time(tt, -0.75)
  expect_equal(res$x, 0.5)
  expect_equal(res$y, -1)
})

test_that("floating point time grid values are matched", {
  # 0.1 * 3 != 0.3 exactly; seq() generates such values
  tt <- tibble::tibble(time = seq(0, 1, by = 0.1), object = 1,
                       x = seq(0, 1, by = 0.1), y = 0)
  res <- estimate_position_for_time(tt, 0.3)
  expect_equal(res$x, 0.3)
})

test_that("objects missing at one neighbouring time point is an error", {
  tt <- make_traj() |> dplyr::filter(!(time == 2 & object == 2))
  expect_error(estimate_position_for_time(tt, 1))
})

test_that("interpolated positions match a straight simulated trajectory", {
  tt <- tibble::tibble(
    time = rep(seq(0, 4, by = 1), 2),
    object = rep(1:2, each = 5),
    x = c(seq(0, 8, by = 2), rep(1, 5)),
    y = c(rep(0, 5), seq(0, -4, by = -1))
  )
  res <- estimate_position_for_time(tt, 2.5)
  expect_equal(res$x, c(5, 1))
  expect_equal(res$y, c(0, -2.5))
})

test_that("timepoint must be a single number", {
  tt <- make_traj()
  expect_error(estimate_position_for_time(tt, c(1, 2)), "single number")
  expect_error(estimate_position_for_time(tt, NA_real_), "single number")
  expect_error(estimate_position_for_time(tt, "1"), "single number")
})

test_that("missing object error message is informative", {
  tt <- make_traj() |> dplyr::filter(!(time == 2 & object == 2))
  expect_error(estimate_position_for_time(tt, 1), "all objects must be present")
})
