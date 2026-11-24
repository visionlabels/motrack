#' Tests inter-object distances
#'
#' The function calculated all pairwise distances and checks
#' if the are larger or equal.
#'
#' @param position tibble
#' @param min_distance Minimum distance to be present between each pair
#'
#' @return TRUE if all distances are at least `min_distance`
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_distance_at_least(example_pos, 1)
#' is_distance_at_least(example_pos, 2)
is_distance_at_least <- function(position, min_distance) {
  dist_triangle <-
    position |>
    dplyr::select("x", "y") |>
    as.matrix() |>
    stats::dist()
  all(dist_triangle >= min_distance)
}
