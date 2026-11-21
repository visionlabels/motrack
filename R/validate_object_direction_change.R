#' Check direction changes for single object trajectory
#'
#' @param trajectory1 Trajectory featuring only one object
#' @param change_angle Angle value in degrees.
#' Direction changes beyond this value are considered large.
#' @param min_interval Time in seconds, which should pass between
#' too large direction changes.
#'
#' @return Logical. `TRUE` if each pair of consecutive bounces
#' is separated by at least `min_interval` seconds.
#' @export
validate_object_direction_change <- function(
  trajectory1,
  change_angle = 30,
  min_interval = 0.2
) {
  # check direction changes in single object
  trajectory1 <- trajectory1 |> dplyr::arrange(.data$time)
  change_min <- change_angle / 360 * 2 * pi
  # calculate direction
  tr <- trajectory1 |>
    dplyr::mutate(
      dx = dplyr::lead(.data$x) - .data$x,
      dy = dplyr::lead(.data$y) - .data$y,
      direction = atan2(.data$dy, .data$dx) %% (2 * pi)
    )
  tr <- tr |>
    dplyr::mutate(
      dirchange = (.data$direction - dplyr::lag(.data$direction)) %% (2 * pi),
      dirchange_large = (.data$dirchange > change_min) &
        (.data$dirchange < 2 * pi - change_min)
    )
  changed <- tr |>
    dplyr::filter(.data$dirchange_large) |>
    dplyr::arrange(.data$time)
  all(diff(changed$time) >= min_interval)
}
