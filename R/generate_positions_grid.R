#' Generate grid-arranged positions
#'
#' Places objects on the nodes of a regular grid within the arena limits.
#' If `step` or `offset` vectors have only one element, they are recycled.
#'
#' @param n Number of points. Use `NULL` to return all possible positions.
#' @param step Vector of two numbers representing the separation on x-axis
#'   and y-axis.
#' @param offset Vector of two numbers representing how the grid should be
#'   shifted. Potentially the point `[0, 0]` would be moved to this point
#'   (unless it is off limits).
#' @param settings Basic properties - namely `xlim`, `ylim`.
#'
#' @return Tibble with `object`, `x` and `y` columns.
#' @export
#'
#' @examples
#' \dontrun{
#' p1 <- generate_positions_grid(10, settings = default_settings())
#' plot_position(p1)
#' p1 <- generate_positions_grid(10, c(1, 2), c(0.1, .5), default_settings())
#' plot_position(p1)
#' }
generate_positions_grid <- function(n, step = 1, offset = 0, settings) {
  xlim <- settings$xlim
  ylim <- settings$ylim
  stopifnot(!is.null(xlim))
  stopifnot(!is.null(ylim))
  if (length(step) == 1) {
    step <- c(step, step)
  }
  if (length(offset) == 1) {
    offset <- c(offset, offset)
  }

  # the idea is that [0,0] is shown at offset and all other points
  # are separated by step coordinates as [x, y]
  # TODO check ydirection in the package
  xmin <- ceiling((xlim[1] - offset[1]) / step[1]) * step[1] + offset[1]
  xmax <- floor((xlim[2] - offset[1]) / step[1]) * step[1] + offset[1]
  # cat("\nx range: ", xmin, " to ", xmax)
  ymin <- ceiling((ylim[1] - offset[2]) / step[2]) * step[2] + offset[2]
  ymax <- floor((ylim[2] - offset[2]) / step[2]) * step[2] + offset[2]
  # cat("\ny range: ", ymin, " to ", ymax, "\n")
  p <- tibble::tibble(x = seq(xmin, xmax, step[1])) |>
    tidyr::expand_grid(y = seq(ymin, ymax, step[2]))
  if (!is.null(n)) {
    p <- p |> dplyr::slice_sample(n = n)
  }
  p |>
    dplyr::mutate(object = seq_len(dplyr::n()), .before = dplyr::everything())
}
