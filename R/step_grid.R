#' Grid trajectory step function
#'
#' Step function for objects moving on a grid. In each step, every object
#' picks one of four directions (east, north, west or south) at random and
#' moves by `speed * (time_next - time)`. The directions of all objects are
#' resampled until the result is valid, that is:
#'
#' - no two objects are closer than 0.5 to each other, and
#' - all objects lie strictly inside the arena (`xlim`, `ylim`).
#'
#' Directions follow the clock analogy used elsewhere in the package:
#' 0 = 3 o'clock, pi/2 = 12 o'clock, pi = 9 o'clock, 3*pi/2 = 6 o'clock.
#'
#' Because the whole configuration is resampled, the function can loop for
#' a long time (or forever) when no valid move exists, e.g., when the grid
#' is densely occupied.
#'
#' @param moment Position tibble with extra columns `direction` and `speed`.
#' @param time_next Next time step to predict.
#' @param settings List with basic properties - namely `xlim` and `ylim`.
#'
#' @return Another moment - position tibble with extra columns `direction`
#'   and `speed` corresponding to `time_next`.
#' @family step_functions
#' @keywords internal
step_grid <- function(moment, time_next, settings) {
  # moment is position + direction + speed
  # just going in the same direction, possibly bouncing
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  max_tries <- 100
  n_tries <- 0

  while (n_tries < max_tries) {
    moment_dir <- moment |>
      dplyr::mutate(
        direction = (sample(
          c(-1, 0, 1, 2) * pi / 2,
          dplyr::n(),
          replace = TRUE
        ))
      )
    moment_next <-
      extrapolate_moment(moment_dir, timestep, time_now, time_next)
    d <- distances(moment_next, moment_next) |>
      dplyr::filter(.data$distance < .5)
    no_overlap <- (nrow(d) == 0)
    within_limits <-
      all(moment_next$x > settings$xlim[1]) &&
      all(moment_next$x < settings$xlim[2]) &&
      all(moment_next$y > settings$ylim[1]) &&
      all(moment_next$y < settings$ylim[2])
    if (no_overlap && within_limits) {
      return(moment_next)
    }
    n_tries <- n_tries + 1
  }
  stop("Maximum number of retries reached in step_grid.")
}
