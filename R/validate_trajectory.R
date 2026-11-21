#' Sanity check for trajectory
#'
#' Sometimes, we want have extra requirements for trajectories. The idea is to
#' have automated check about possible errors or artifacts which could appear
#' in trajectory data.
#'
#' We test for following conditions:
#'
#' - *Sudden motion changes* - Sometimes objects bounce too frequently,
#'   which may attract attention. For each object, we test what is the minimal
#'   time interval between two consecutive large direction changes.
#' - *Minimum interobject distance* - The minimum distance is checked
#'   when generating positions and also in each step of generating trajectories.
#'   Here, we make final check if the minimum distance is complied.
#'
#' @param trajectory Tibble object with trajectory
#' @param change_angle Angle value in degrees.
#' Direction changes beyond this value are considered large.
#' @param min_interval Time in seconds, which should pass between
#' too large direction changes.
#' @param min_distance Minimum inter-object spacing, which should be
#' complied in every frame.
#' @param verbose If `TRUE`, results for each subtest are printed on console.
#' Default `FALSE`.
#'
#' @return Logical. `TRUE` if all conditions are passed.
#' @export
#'
#' @examples
#' validate_trajectory(trajectory8c)
validate_trajectory <- function(
  trajectory,
  change_angle = 30,
  min_interval = 0.2,
  min_distance = 1,
  verbose = FALSE
) {
  # for each object we will check the direction changes and report if there
  # are two changes too soon after each other
  crit1 <- TRUE
  for (o in unique(trajectory$object)) {
    ok <- validate_object_direction_change(
      trajectory |> dplyr::filter(.data$object == o),
      change_angle,
      min_interval
    )
    if (!ok) crit1 <- FALSE
  }
  crit2 <- TRUE
  for (tt in unique(trajectory$time)) {
    ok <- is_distance_at_least(
      trajectory |> dplyr::filter(.data$time == tt),
      min_distance
    )
    if (!ok) crit2 <- FALSE
  }
  if (verbose) {
    message(ifelse(crit1, "PASS", "FAIL"), " Sudden motion changes")
    message(ifelse(crit2, "PASS", "FAIL"), " Minimum interobject distance")
  }
  crit1 & crit2
}
