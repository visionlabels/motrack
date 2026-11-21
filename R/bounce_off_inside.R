#' Check for bouncing off the inner circle of a donut arena
#'
#' Counterpart of [bounce_off_circle()] for the inner border of a donut-shaped
#' arena (`arena_shape = "donut"`). The function extrapolates the objects to
#' the next time point. If an object would end up closer to the arena centre
#' than `settings$arena_inside_radius`, its direction is changed to point
#' directly away from the centre, with optional random jitter.
#'
#' The arena centre is the midpoint of `xlim` and `ylim`, so the arena must be
#' circular, i.e., `xlim` and `ylim` must have the same extent. Otherwise
#' the function stops with an error.
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next timepoint
#' @param settings List with basic properties. Besides `xlim` and `ylim`,
#'   it must contain `arena_inside_radius` (radius of the inner border, which
#'   is an optional setting without a default) and uses
#'   `circle_bounce_jitter`, the amount of uniform angular jitter
#'   (in radians) added to the new direction.
#'
#' @return Like `moment`, but with `direction` adjusted for objects which
#'   would cross the inner border in the next time point.
#' @export
#' @seealso [bounce_off_circle()] for the outer border,
#'   [default_settings()] for setting definitions
bounce_off_inside <- function(moment, time_next, settings) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # extrapolate future
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, NA)
  # arena radius
  dims <- sort(c(abs(diff(settings$xlim)), abs(diff(settings$ylim))))
  if (length(unique(dims)) > 1) {
    stop("Multiple radius options for arena.")
  }
  # arena_radius <- unique(dims)[1] / 2
  midpoint_x <- mean(settings$xlim)
  midpoint_y <- mean(settings$ylim)
  # check sides
  beyond_inside_border <- sqrt(
    (moment_next$x - midpoint_x)^2 +
      (moment_next$y - midpoint_y)^2
  ) <
    settings$arena_inside_radius
  vector_to_midpoint <-
    (atan2(moment$y - midpoint_y, moment$x - midpoint_x)) %% (2 * pi)
  jitter <- stats::runif(
    nrow(moment),
    min = -settings$circle_bounce_jitter,
    max = settings$circle_bounce_jitter
  )
  moment$direction[beyond_inside_border] <-
    (vector_to_midpoint[beyond_inside_border] + jitter[beyond_inside_border]) %%
    (2 * pi)
  moment
}
