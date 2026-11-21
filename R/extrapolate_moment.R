#' Extrapolates object positions based on current state and time
#'
#' Intended for the use in custom step functions.
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param timestep How far into future should the objects move (linearly)
#' @param cur_time Current time, for time-sensitive speed functions
#'        (e.g., accelerations)
#' @param new_time New time value for the returned object.
#'        If omitted, it is calculated automatically as `cur_time + timestep`
#'
#' @return Another moment - position tibble with
#' extra columns `direction` and `speed` corresponding to time_next
#' @export
#'
#' @examples
#' moment <- position8c |> add_random_direction() |>
#'   dplyr::mutate(speed = 3)
#' extrapolate_moment(moment, 0.1, 0)
extrapolate_moment <- function(moment, timestep, cur_time, new_time = NULL) {
  s <- cur_speed(moment, cur_time)
  if (is.null(new_time)) {
    new_time <- cur_time + timestep
  }
  moment_next <- moment |>
    dplyr::mutate(
      x = .data$x + cos(.data$direction) * s * timestep,
      y = .data$y + sin(.data$direction) * s * timestep,
      time = new_time
    )
  moment_next
}
