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
  arena_radius <- unique(dims)[1] / 2
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
