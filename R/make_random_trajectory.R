#' Create random trajectory
#'
#' @param start Position tibble with starting configuration
#' @param timescale Numeric vector with time points for the trajectory
#' @param settings list with basic properties
#' @param step_function function which is called in every time step
#' to calculate next position in sequence
#' @param ... Extra parameters passed to `step_function`
#'
#' @return Tibble trajectory object
#' @export
#'
#' @examples
#' sett_move <-
#'   new_settings(
#'     speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
#'     bounce_off_square = FALSE,
#'     bounce_off_circle = TRUE, circle_bounce_jitter = pi / 6
#'   )
#' moment <- position8c |> add_random_direction()
#' tt <- make_random_trajectory(
#'   moment, seq(0, 8, by = 0.1), sett_move, step_direct
#' )
make_random_trajectory <- function(
  start,
  timescale,
  settings,
  step_function,
  ...
) {
  # take start position
  # are direction/speed present? - add if not
  # make step
  stopifnot(length(timescale) > 1)
  if ("direction" %in% names(start)) {
    moment <- start
  } else {
    moment <- start |> add_random_direction()
  }

  if (!"speed" %in% names(moment)) {
    moment <- moment |> dplyr::mutate(speed = settings$speed)
  }
  moment <- moment |> dplyr::mutate(time = timescale[1])

  # moment is position + direction + speed
  # here we have tibble of moments with two columns:
  #   - time (from timescale)
  #   - position (embedded moment tibble for each time)
  # Later we expand tibbles into trajectory tibble
  moment_tbl <- tibble::tibble(
    time = timescale,
    position = list(tibble::tibble)
  )
  moment_tbl$position[[1]] <- moment
  for (i in 2:length(timescale)) {
    if (settings$bounce_off_square) {
      moment <- bounce_off_square(moment, timescale[i], settings)
    }
    if (settings$bounce_off_circle) {
      moment <- bounce_off_circle(moment, timescale[i], settings)
    }
    if (settings$bounce_off_inside) {
      moment <- bounce_off_inside(moment, timescale[i], settings)
    }
    if (settings$bounce_off_others) {
      moment <- bounce_off_others(moment, timescale[i], settings)
    }
    moment_next <- step_function(moment, timescale[i], settings, ...)

    moment_tbl$position[[i]] <- moment_next
    moment <- moment_next
  }
  moment_tbl |>
    dplyr::select(-"time") |>
    tidyr::unnest(cols = c("position"))
}
