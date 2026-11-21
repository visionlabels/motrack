#' Check for circular arena bouncing and recalculate directions
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next timepoint
#' @param settings list with basic properties
#'
#' @return Like `moment`, but some directions are adjusted (reversed, swapped),
#' if there were a collision with arena border in next time point (`timestep`)
#' @export
bounce_off_circle <- function(moment, time_next, settings) {
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
  arena_radius <- unique(dims)[1] / 2
  midpoint_x <- mean(settings$xlim)
  midpoint_y <- mean(settings$ylim)
  # check sides
  beyond_border <- sqrt(
    (moment_next$x - midpoint_x)^2 +
      (moment_next$y - midpoint_y)^2
  ) >
    arena_radius
  vector_to_midpoint <-
    (atan2(moment$y - midpoint_y, moment$x - midpoint_x) + pi) %% (2 * pi)
  jitter <- stats::runif(
    nrow(moment),
    min = -settings$circle_bounce_jitter,
    max = settings$circle_bounce_jitter
  )
  moment$direction[beyond_border] <-
    (vector_to_midpoint[beyond_border] + jitter[beyond_border]) %% (2 * pi)
  moment
}
