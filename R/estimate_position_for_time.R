estimate_position_for_time <- function(trajectory, timepoint) {
  stopifnot(timepoint <= max(trajectory$time))
  stopifnot(timepoint >= min(trajectory$time))
  # find one before and one after
  time_all <- sort(unique(trajectory$time))
  time_before <- max(time_all[time_all <= timepoint])
  time_after <- min(time_all[time_all >= timepoint])
  points_before <- trajectory |>
    dplyr::filter(.data$time == time_before) |>
    dplyr::arrange(.data$object)
  points_after <- trajectory |>
    dplyr::filter(.data$time == time_after) |>
    dplyr::arrange(.data$object)
  stopifnot(all(points_before$object == points_after$object))
  if (time_after > time_before) {
    # interpolate - contribution of "after"
    timestep <- (timepoint - time_before) / (time_after - time_before)
    res <- points_before
    res$x <- res$x + timestep * (points_after$x - points_before$x)
    res$y <- res$y + timestep * (points_after$y - points_before$y)
    res
  } else {
    # "before" and "after" are the same, so return one of them
    points_before
  }
}
