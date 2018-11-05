
#' Is the object a valid position
#'
#' The function tests an object (tibble) for several requirements:
#' \itemize{
#'   \item columns `object`, `x` and `y` are present.
#'   \item `object` column contains unique values
#'   \item no value missing in `x` and `y`
#'   \item `x` and `y` are numeric
#' }
#'
#' No restriction about data type of `object` is placed.
#'
#' @param position tibble
#'
#' @return TRUE if conditions above are met, FALSE otherwise
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_valid_position(example_pos)
is_valid_position <- function(position) {
  # requirements:
  # 1) each object only once
  # 2) no missing x, y
  tmp <- position %>%
    dplyr::group_by(object) %>%
    dplyr::summarise(n = n())
  all(tmp$n == 1) &&
    all(!is.na(position$x)) &&
    all(!is.na(position$y)) &&
    all(is.numeric(position$x)) &&
    all(is.numeric(position$y))
}

#' Tests inter-object distances
#'
#' The function calculated all pairwise distances and checks
#' if the are larger or equal.
#'
#' @param position
#' @param min_distance Minimum distance to be present between each pair
#'
#' @return TRUE if all distances are larger than `min_distance`
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_distance_at_least(example_pos, 1)
#' is_distance_at_least(example_pos, 2)
is_distance_at_least <- function(position, min_distance) {
  dist_triangle <-
    position %>%
    dplyr::select(x, y) %>%
    as.matrix() %>%
    dist()
  all(dist_triangle >= min_distance)
}