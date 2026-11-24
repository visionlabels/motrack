#' Is the object a valid position
#'
#' The function tests an object (tibble) for several requirements:
#'
#' - columns `object`, `x` and `y` are present.
#' - there is at least one row
#' - `object` column contains unique values and no missing values
#' - `x` and `y` are numeric
#' - all values in `x` and `y` are finite (no `NA`, `NaN`, `Inf` or `-Inf`)
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
  if (!all(c("object", "x", "y") %in% names(position))) {
    return(FALSE)
  }
  if (nrow(position) == 0) {
    return(FALSE)
  }
  # each object only once, and no missing object identifiers
  !anyNA(position$object) &&
    !anyDuplicated(position$object) &&
    is.numeric(position$x) &&
    is.numeric(position$y) &&
    all(is.finite(position$x)) &&
    all(is.finite(position$y))
}
