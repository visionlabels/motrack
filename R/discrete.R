#' Generate grid-arranged positions
#'
#' @param n Number of points. Use NULL to return all possible positions.
#' @param step Vector of two numbers representing the separation on x-axis and y-axis
#' @param offset Vector of two numbers representing how the grid should be shifted.
#' Potentially the [0,0] point would be moved to this point (unless it is off limits).
#' @param settings Basic properties - namely `xlim`, `ylim`
#'
#' If \code{step} or \code{offeset} vectors have only one element, they are recycled.
#'
#' @return Tibble with `object`, `x` and `y` columns.
#' @export
#'
#' @example
#' \dontrun{
#' p1 <- generate_positions_grid(10, settings = default_settings())
#' plot_position(p1)
#' p1 <- generate_positions_grid(10, c(1, 2), c(0.1, .5), default_settings())
#' plot_position(p1)
#' }
generate_positions_grid <- function(n, step = 1, offset = 0, settings) {
  xlim <- settings$xlim
  ylim <- settings$ylim
  stopifnot(!is.null(xlim))
  stopifnot(!is.null(ylim))
  if (length(step) == 1) step <- c(step, step)
  if (length(offset) == 1) offset <- c(offset, offset)

  # the idea is that [0,0] is shown at offset and all other points
  # are separated by step
  # coordinates as [x, y]
  # TODO check ydirection in the package
  xmin <- ceiling((xlim[1] - offset[1]) / step[1]) * step[1] + offset[1]
  xmax <- floor((xlim[2] - offset[1]) / step[1]) * step[1] + offset[1]
  # cat("\nx range: ", xmin, " to ", xmax)
  ymin <- ceiling((ylim[1] - offset[2]) / step[2]) * step[2] + offset[2]
  ymax <- floor((ylim[2] - offset[2]) / step[2]) * step[2] + offset[2]
  # cat("\ny range: ", ymin, " to ", ymax, "\n")
  p <- tibble::tibble(x = seq(xmin, xmax, step[1])) |>
    tidyr::expand_grid(y = seq(ymin, ymax, step[2]))
  if (!is.null(n)) {
    p <- p |> dplyr::slice_sample(n = n)
  }
  p |>
    dplyr::mutate(object = seq_len(dplyr::n()), .before = dplyr::everything())
}


step_vortex <- function(moment, time_next, settings) {
}

step_grid <- function(moment, time_next, settings) {
  # moment is position + direction + speed
  # just going in the same direction, possibly bouncing
  time_now <- moment$time[1]
  timestep <- time_next - time_now

  while (TRUE) {
    # moment_next <- moment %>%
    #   dplyr::mutate(
    #     direction = (.data$direction +
    #       sample(c(-1, 1) * pi / 2, n(), replace = TRUE))
    #   )
    moment_next <- moment %>%
      dplyr::mutate(
        direction = (sample(c(-1, 0, 1, 2) * pi / 2, n(), replace = TRUE))
      )
    moment_next2 <-
      extrapolate_moment(moment_next, timestep, time_now, time_next)
    d <- distances(moment_next2, moment_next2) |> filter(distance < .5)
    no_overlap <- (nrow(d) == 0)
    within_limits <-
      all(moment_next2$x > settings$xlim[1]) &&
      all(moment_next2$x < settings$xlim[2]) &&
      all(moment_next2$y > settings$ylim[1]) &&
      all(moment_next2$y < settings$ylim[2])
    if (no_overlap && within_limits) break
  }
  moment_next2
}

distances <- function(p1, p2) {
  dd <- expand_grid(
    p1 |> select(o1 = object, x1 = x, y1 = y),
    p1 |> select(o2 = object, x2 = x, y2 = y)
  ) |>
    filter(o1 != o2) |>
    mutate(
      distance = sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
    )
  dd
}


library(tidyverse)
library(motrack)
s <- new_settings(
  speed = 1,
  bounce_off_square = FALSE,
  bounce_off_others = FALSE,
  xlim = c(-4, 4),
  ylim = c(-4, 4)
)
s0 <- new_settings(.from = s, xlim = c(-2, 2), ylim = c(-2, 2))
p1 <- generate_positions_grid(8, settings = s0) |>
  mutate(direction = sample((0:3) * pi / 2, n(), replace = TRUE))
p1 <- expand_grid(x = c(-2, 0, 2), y = c(-2, 0, 2)) |>
  filter(x != 0 | y != 0) |>
  mutate(
    object = 1:n(),
    direction = sample((0:3) * pi / 2, n(), replace = TRUE)
  )
plot_position(p1)
timescale <- seq(0, 8 * 4, by = 1)
t1 <- make_random_trajectory(p1, timescale, s, step_grid)
plot_trajectory(t1)
max(t1$x)
