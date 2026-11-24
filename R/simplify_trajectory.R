#' Simplify trajectory
#'
#' To save space we can simplify the trajectory data. Trajectories
#' usually have very fine timeline, because small time steps allows to
#' (1) minimize potential collision artifacts,
#' (2) allow smooth directional changes like in `step_vonmises`.
#' When simplifying, we go through time points and detect
#' for which timepoints we can interpolate all object positions.
#' We keep these positions and remove the rest.
#' The resulting trajectory has irregular timeline.
#' If `snapshot`, then for each
#' timepoint positions of all objects are included
#' (i.e., also positions for these,
#' which could be interpolated for a given timepoint).
#' If `snapshot == FALSE`, then only the data for changing points are included.
#'
#' @param trajectory Trajectory data
#' @param snapshot Logical, if each timepoint should include data for all point.
#' @param eps Direction changes smaller than `eps` (in radians) are ignored
#'
#' @return Subset of the original data. The timeline is irregular,
#' the positions which could be interpolated are omitted.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' sett_move <-
#' new_settings(
#'   speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
#'   bounce_off_square = FALSE,
#'   bounce_off_circle = TRUE, circle_bounce_jitter = pi / 6
#' )
#' moment <- position8c |> add_random_direction()
#' tt1 <- make_random_trajectory(
#'   moment, seq(0, 8, by = 0.1), sett_move, step_direct)
#' tt2 <- simplify_trajectory(tt1, snapshot = TRUE)
#' tt3 <- simplify_trajectory(tt1, snapshot = FALSE)
#' ggplot(tt1, aes(x = x, y = y, group = object)) +
#'   geom_path() +
#'   geom_point(data = tt2)
#' # number of lines reduced down to:
#' nrow(tt2) / nrow(tt1)
#' nrow(tt3) / nrow(tt1)
#' }
simplify_trajectory <- function(trajectory, snapshot = T, eps = 1e-4) {
  # calculate directions
  calculate_direction_for_single_object <- function(t1) {
    t1 |>
      dplyr::arrange(.data$time) |>
      dplyr::mutate(
        dx = dplyr::lead(.data$x) - .data$x,
        dy = dplyr::lead(.data$y) - .data$y,
        direction = atan2(.data$dy, .data$dx) %% (2 * pi)
      )
  }
  # difference of angles wrapped to (-pi, pi], so that headings just
  # below 2*pi and just above 0 are recognised as nearly identical
  angle_difference <- function(a, b) {
    atan2(sin(a - b), cos(a - b))
  }
  large_or_na <- function(x) {
    is.na(x) | abs(x) > eps
  }
  small_and_not_na <- function(x) {
    !is.na(x) & abs(x) < eps
  }
  find_critical_timepoints <- function(t1) {
    # assumptions: one object, time sorted, direction column
    # keep: 1st, last, direction changes
    t1 |>
      dplyr::mutate(
        keep = large_or_na(
          angle_difference(.data$direction, dplyr::lag(.data$direction))
        )
      )
  }
  events <- trajectory |>
    dplyr::group_split(.data$object) |>
    purrr::map(~ dplyr::arrange(., .data$time)) |>
    purrr::map(~ calculate_direction_for_single_object(.)) |>
    purrr::map(~ find_critical_timepoints(.)) |>
    purrr::map(~ dplyr::filter(., .data$keep)) |>
    dplyr::bind_rows()
  if (snapshot) {
    trajectory |> dplyr::filter(.data$time %in% unique(events$time))
  } else {
    # we return the original rows (direction was recomputed in `events`)
    trajectory |>
      dplyr::arrange(.data$object, .data$time) |>
      dplyr::semi_join(events, by = c("object", "time"))
  }
}
