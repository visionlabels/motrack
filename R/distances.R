#' Pairwise distances between objects
#'
#' Calculates Euclidean distances between every object in `p1` and every
#' object in `p2`. Pairs with the same `object` identifier are dropped, so
#' when `p1` and `p2` are the same tibble, an object is never paired with
#' itself and each pair of objects appears twice (A-B and B-A). Used, for
#' example, by [step_grid()] to detect overlapping objects.
#'
#' @param p1 Position tibble with `object`, `x` and `y` columns.
#' @param p2 Position tibble with `object`, `x` and `y` columns.
#'
#' @return Tibble with columns `o1`, `x1`, `y1` (object from `p1`), `o2`,
#'   `x2`, `y2` (object from `p2`) and `distance`.
#' @export
distances <- function(p1, p2) {
  dd <- tidyr::expand_grid(
    p1 |> dplyr::select(o1 = .data$object, x1 = .data$x, y1 = .data$y),
    p2 |> dplyr::select(o2 = .data$object, x2 = .data$x, y2 = .data$y)
  ) |>
    dplyr::filter(.data$o1 != .data$o2) |>
    dplyr::mutate(
      distance = sqrt(
        (.data$x1 - .data$x2) *
          (.data$x1 - .data$x2) +
          (.data$y1 - .data$y2) *
            (.data$y1 - .data$y2)
      )
    )
  dd
}
