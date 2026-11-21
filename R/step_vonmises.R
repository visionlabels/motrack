#' Von Mises trajectory step function
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next time step to predict
#' @param settings list with basic properties
#' @param kappa concentration parameter
#'
#' @return Another moment - position tibble with
#' extra columns `direction` and `speed` corresponding to time_next
#' @export
#' @family step_functions
step_vonmises <- function(moment, time_next, settings, kappa) {
  # moment is position + direction + speed
  # just goint in the same direction, possibly bouncing
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  n <- nrow(moment)
  moment_next <- moment |>
    dplyr::mutate(
      direction = (.data$direction +
        as.numeric(
          circular::rvonmises(n, circular::circular(0), kappa = kappa)
        )) %%
        (2 * pi)
    )
  moment_next <-
    extrapolate_moment(moment_next, timestep, time_now, time_next)
  moment_next
}
