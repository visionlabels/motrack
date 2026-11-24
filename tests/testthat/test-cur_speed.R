make_moment <- function(n = 3) {
  tibble::tibble(object = seq_len(n), x = 0, y = 0, direction = 0)
}

test_that("numeric speed is returned unchanged", {
  m <- make_moment(3) |> dplyr::mutate(speed = c(1, 2, 3))
  expect_equal(cur_speed(m, 0), c(1, 2, 3))
})

test_that("numeric speed does not depend on time", {
  m <- make_moment(2) |> dplyr::mutate(speed = 4)
  expect_equal(cur_speed(m, 0), cur_speed(m, 100))
})

test_that("single speed function is evaluated with time and moment", {
  f <- function(time, moment) time + moment$object
  m <- make_moment(3) |> dplyr::mutate(speed = list(f))
  expect_equal(cur_speed(m, 10), 10 + 1:3)
})

test_that("single function is called once for the whole moment", {
  calls <- 0
  f <- function(time, moment) {
    calls <<- calls + 1
    rep(2, nrow(moment))
  }
  m <- make_moment(4) |> dplyr::mutate(speed = list(f))
  expect_equal(cur_speed(m, 0), rep(2, 4))
  expect_equal(calls, 1)
})

test_that("per-object functions are evaluated one object at a time", {
  fs <- list(
    function(time, moment) 1 + time,
    function(time, moment) 10 + time,
    function(time, moment) 100 + time
  )
  m <- make_moment(3)
  m$speed <- fs
  expect_equal(cur_speed(m, 2), c(3, 12, 102))
})

test_that("per-object functions receive a one-row moment", {
  seen <- integer()
  fs <- list(
    function(time, moment) {
      seen <<- c(seen, nrow(moment))
      1
    },
    function(time, moment) {
      seen <<- c(seen, nrow(moment))
      2
    }
  )
  m <- make_moment(2)
  m$speed <- fs
  cur_speed(m, 0)
  expect_equal(seen, c(1L, 1L))
})

test_that("single and equivalent per-object functions agree", {
  f <- function(time, moment) 5 + cos(time + moment$object)
  m1 <- make_moment(4) |> dplyr::mutate(speed = list(f))
  m2 <- make_moment(4)
  m2$speed <- rep(list(f), 4)
  expect_equal(cur_speed(m1, 1.3), cur_speed(m2, 1.3))
})

test_that("function speed can depend on location", {
  f <- function(time, moment) 1 + abs(moment$x)
  m <- make_moment(2) |>
    dplyr::mutate(x = c(0, 3), speed = list(f))
  expect_equal(cur_speed(m, 0), c(1, 4))
})

test_that("speed of a single object works for both representations", {
  m <- make_moment(1) |> dplyr::mutate(speed = 2)
  expect_equal(cur_speed(m, 0), 2)
  m <- make_moment(1) |> dplyr::mutate(speed = list(function(time, moment) 7))
  expect_equal(cur_speed(m, 0), 7)
})

test_that("invalid speed type gives an informative error", {
  m <- make_moment(2) |> dplyr::mutate(speed = c("a", "b"))
  expect_error(cur_speed(m, 0), "Speed")
})

test_that("missing speed column gives an error", {
  expect_error(cur_speed(make_moment(2), 0))
})

test_that("function speeds drive extrapolate_moment", {
  f <- function(time, moment) 2 * time
  m <- make_moment(2) |> dplyr::mutate(speed = list(f))
  nxt <- extrapolate_moment(m, timestep = 0.5, cur_time = 4)
  expect_equal(nxt$x, c(4, 4)) # 0 + cos(0) * (2 * 4) * 0.5
  expect_equal(nxt$time, c(4.5, 4.5))
})

test_that("function speeds work through make_random_trajectory", {
  set.seed(1)
  f <- function(time, moment) 1 + time
  sett <- new_settings(
    speed = c(f),
    xlim = c(-100, 100),
    ylim = c(-100, 100),
    bounce_off_square = FALSE,
    bounce_off_circle = FALSE,
    bounce_off_others = FALSE
  )
  start <- position8c |> add_random_direction()
  tt <- make_random_trajectory(start, seq(0, 2, by = 0.5), sett, step_direct)
  step_len <- tt |>
    dplyr::filter(.data$object == 1) |>
    dplyr::mutate(
      d = sqrt(
        (.data$x - dplyr::lag(.data$x))^2 +
          (.data$y - dplyr::lag(.data$y))^2
      )
    )
  # step length is speed(t_prev) * dt, so it grows with time
  expect_true(all(diff(step_len$d[-1]) > 0))
})
