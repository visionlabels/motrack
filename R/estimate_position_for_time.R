#' Estimate object positions at an arbitrary time
#'
#' Linearly interpolates the `x` and `y` coordinates of all objects between
#' the two nearest time points of the trajectory (the closest one at or
#' before `timepoint` and the closest one at or after it). If `timepoint`
#' coincides with a time point in the trajectory, that row set is returned
#' unchanged. Used by [render_trajectory_video()] to get positions between
#' recorded frames, e.g., for trajectories reduced by
#' [simplify_trajectory()].
#'
#' Only `x` and `y` are interpolated. All other columns (including `time`,
#' `direction` and `speed`) are taken from the earlier time point, so `time`
#' of the result is *not* `timepoint`.
#'
#' @param trajectory Trajectory tibble with `time`, `object`, `x` and `y`
#'   columns. All objects must be present at both neighbouring time points.
#' @param timepoint Single time value, between the minimum and the maximum
#'   `time` of `trajectory` (inclusive). Otherwise the function stops
#'   with an error.
#'
#' @return Tibble with one row per object (ordered by `object`) and
#'   interpolated `x` and `y`.
#' @noRd
estimate_position_for_time <- function(trajectory, timepoint) {
  stopifnot(
    "`timepoint` must be a single number" =
      is.numeric(timepoint) && length(timepoint) == 1 && !is.na(timepoint)
  )
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
  stopifnot(
    "all objects must be present at both neighbouring time points" =
      identical(points_before$object, points_after$object)
  )
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
