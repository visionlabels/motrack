#' Is the object a valid position
#'
#' The function tests an object (tibble) for several requirements:
#'
#' - columns `object`, `x` and `y` are present.
#' - `object` column contains unique values
#' - no value missing in `x` and `y`
#' - `x` and `y` are numeric
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
  tmp <- position |>
    dplyr::group_by(.data$object) |>
    dplyr::summarise(n = dplyr::n())
  all(tmp$n == 1) &&
    all(!is.na(position$x)) &&
    all(!is.na(position$y)) &&
    is.numeric(position$x) &&
    is.numeric(position$y)
}
