#' Random object positions
#'
#' The function expects a square arena defined by `xlim` and `ylim` settings.
#' The objects are placed randomly (uniform distribution sampling)
#' into the arena.
#' If `check_distance` is `TRUE`, the process is repeated
#' until the minimum pairwise distance is met.
#' `border_distance` specifies, whether objects should keep some
#' initial distance
#' from arena borders. This distance is not meant to be the bouncing distance,
#' it helps to put objects more together in the beginning.
#'
#' @param n Number of objects
#' @param settings Basic properties - namely `xlim`, `ylim`,
#' possibly `min_distance`
#' @param check_distance Logical.
#' The positions are generated until
#' minimum pairwise distance of `min_distance` is met.
#' @param border_distance Distance from arena borders.
#'
#' @return Tibble with `object`, `x` and `y` columns.
#' @export
#'
#' @seealso
#' [`default_settings()`] for setting definitions,
#' [`new_settings()`] for adjusting default settings,
#' [`is_distance_at_least()`] for checking distances
#'
#' @examples
#' # sample positions with no other requirements
#' pos <- generate_positions_random(8, default_settings())
#' # when starting positions should be further from borders
#' pos <- generate_positions_random(8, default_settings(), border_distance = 3)
generate_positions_random <- function(
  n,
  settings,
  check_distance = TRUE,
  border_distance = 0
) {
  xlim <- settings$xlim
  ylim <- settings$ylim
  stopifnot(!is.null(xlim))
  stopifnot(!is.null(ylim))
  shapes <- c("square", "circle", "donut")
  shape <- pmatch(settings$arena_shape, shapes)
  if (is.na(shape)) {
    stop("Arena shape unknown")
  }
  while (TRUE) {
    p <- switch(
      shapes[shape],
      square = random_coords_in_square(
        n,
        xlim[1] + border_distance,
        xlim[2] - border_distance,
        ylim[1] + border_distance,
        ylim[2] - border_distance
      ),
      circle = random_coords_in_circle(
        n,
        mean(xlim),
        mean(ylim),
        sum(c(diff(xlim), diff(ylim)) / 4) - border_distance
      ),
      donut = random_coords_in_donut(
        n,
        mean(xlim),
        mean(ylim),
        sum(c(diff(xlim), diff(ylim)) / 4) - border_distance,
        settings$arena_inside_radius + border_distance
      )
    )
    p <- p |>
      dplyr::mutate(object = 1:n) |>
      dplyr::select("object", dplyr::everything())
    if (check_distance) {
      if (is_distance_at_least(p, min_distance = settings$min_distance)) {
        return(p)
      }
    } else {
      return(p)
    }
  }
}

random_coords_in_square <- function(n, xmin, xmax, ymin, ymax) {
  tibble::tibble(
    x = stats::runif(n, xmin, xmax),
    y = stats::runif(n, ymin, ymax)
  )
}

random_coords_in_circle <- function(n, xmid, ymid, radius) {
  stopifnot(radius > 0)
  res <- tibble::tibble(x = rep(NA_real_, n), y = rep(NA_real_, n))
  i <- 1
  while (i <= n) {
    xx <- stats::runif(1, -radius, radius)
    yy <- stats::runif(1, -radius, radius)
    d <- sqrt((xx^2) + (yy^2))
    if (d < radius) {
      res$x[i] <- xmid + xx
      res$y[i] <- ymid + yy
      i <- i + 1
    }
  }
  res
}

random_coords_in_donut <- function(n, xmid, ymid, radius_out, radius_in) {
  stopifnot(radius_out > 0)
  stopifnot(radius_in > 0)
  stopifnot(radius_out > radius_in)
  res <- tibble::tibble(x = rep(NA_real_, n), y = rep(NA_real_, n))
  i <- 1
  while (i <= n) {
    xx <- stats::runif(1, -radius_out, radius_out)
    yy <- stats::runif(1, -radius_in, radius_in)
    d <- sqrt((xx^2) + (yy^2))
    if (d < radius_out && d > radius_in) {
      res$x[i] <- xmid + xx
      res$y[i] <- ymid + yy
      i <- i + 1
    }
  }
  res
}
