
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

#' Default settings for position/trajectory code
#'
#' Returns a list of values used for position/trajectory generation or
#' presentation. The values refer to the "physical properties"
#' (e.g., definition of the objects or arena) or
#' to the presentation propoerties
#' (e.g., objects' colour, presence of border).
#' The list is passed as a parameter to other functions.
#'
#' Currently used properties include:
#' \describe{
#'   \item{xlim}{A vector of form `c(xmin, xmax)`.
#'   Represents x-limit of the arena square, objects are limited to this area.}
#'   \item{ylim}{A vector of form `c(ymin, ymax)`.
#'   Represents y-limit of the arena square, objects are limited to this area.}
#'   \item{min_dist}{Minimum pairwise distance between centres of objects.}
#'   \item{r}{Radius of the object. Object is considered to be a circle.}
#'   \item{arena_border}{Logical. Whether arena border should be drawn.}
#'   \item{fill_object}{Character. Colour code of default fill colour.}
#'   \item{fill_target}{Character. Colour code of target fill colour.}
#'   \item{border_object}{Character. Colour code of default border.}
#'   \item{border_target}{Character. Colour code of target border.}
#' }
#'
#' @return list of parameters
#' @export
#'
#' @examples
default_settings <- function() {
  list(
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    min_dist = 1,
    r = 0.5,
    arena_border = T,
    fill_object = "gray",
    fill_target = "green",
    border_object = "black",
    border_target = "black"
  )
}


#' Creates a custom copy of settings
#'
#' @param ... named list of properties to override default settings.
#'
#' @return
#' @export
#'
#' @seealso \code{\link{default_settings}}
#'
#' @examples
#' settings(xlim = c(0, 10), ylim = c(0, 10))
settings <- function(...) {
  settings_list <- default_settings()
  dots <- list(...)
  for (key in names(dots)) {
    settings_list[[key]] <- dots[[key]]
  }
  settings_list
}

#' Random object positions
#'
#' The function expects a square arena defined by `xlim` and `ylim` settings.
#' The objects are placed randomly (uniform distribution sampling) into the arena.
#' If `check_distance` is `TRUE`, the process is repeated
#' until the minimum pairwise distance is met.
#' `border_distance` specifies, whether objects should keep some initial distance
#' from arena borders. This distance is not meant to be the bouncing distance,
#' it helps to put objects more together in the beginning.
#'
#' @param n Number of objects
#' @param settings Basic properties - namely `xlim`, `ylim`,
#' possibly `min_distance`
#' @param check_distance Logical.
#' The positions are generated until
#' minimum pairwise distance of `min_dist` is met.
#' @param border_distance Distance from arena borders.
#'
#' @return Tibble with `object`, `x` and `y` columns.
#' @export
#'
#' @seealso
#' \code{\link{default_settings}} for setting definitions,
#' \code{\link{settings}} for adjusting default settings,
#' \code{\link{is_distance_at_least}} for checking distances
#'
#' @examples
#' # sample positions with no other requirements
#' pos <- generate_positions_random(8, default_settings())
#' # when starting positions should be further from borders
#' pos <- generate_positions_random(8, default_settings(), border_distance = 3)
generate_positions_random <- function(
  n, settings, check_distance = T, border_distance = 0) {
  xlim <- settings$xlim
  ylim <- settings$ylim
  stopifnot(!is.null(xlim))
  stopifnot(!is.null(ylim))
  while (T) {
    p <- tibble::tibble(
      object = 1:n,
      x = runif(n, xlim[1] + border_distance, xlim[2] - border_distance),
      y = runif(n, ylim[1] + border_distance, ylim[2] - border_distance)
    )
    if (check_distance) {
      if (is_distance_at_least(p, min_distance = settings$min_distance)) {
        return(p)
      }
    } else {
      return(p)
    }
  }
}

