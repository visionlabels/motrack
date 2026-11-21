#' Zigzag trajectory step function
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next time step to predict
#' @param settings list with basic properties
#' @param ttt Time-to-turn - time when the object should pick new direction
#' @param syncstart Logical, if time-to-turn should be synchronized
#' when it is generated for the first time
#'
#' @return Another moment - position tibble with
#' extra columns `direction`, `speed` and `ttt` corresponding to time_next
#' @export
#' @family step_functions
step_zigzag <- function(
  moment,
  time_next,
  settings,
  ttt = c(.5, 1.5),
  syncstart = FALSE
) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # ttt is time-to-turn - when object should turn randomly
  n <- nrow(moment)
  if (!"ttt" %in% names(moment)) {
    times <- stats::runif(n, min = ttt[1], max = ttt[2])
    if (!syncstart) {
      passed <- stats::runif(n, min = 0, max = ttt[1])
      times <- times - passed
    }
    moment <- moment |> dplyr::mutate(ttt = times)
  }
  which_turn <- moment$ttt < time_next
  if (any(which_turn)) {
    moment$direction[which_turn] <-
      stats::runif(sum(which_turn), 0, 2 * pi)
    moment$ttt[which_turn] <-
      moment$ttt[which_turn] +
      stats::runif(sum(which_turn), min = ttt[1], max = ttt[2])
  }
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, time_next)
  moment_next
}
