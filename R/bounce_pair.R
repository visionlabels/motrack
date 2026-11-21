#' Calculate collision vectors for two objects
#'
#' @param v1 Velocity vector of object 1. Two-dimensional numeric vector.
#' @param v2 Velocity vector of object 2. Two-dimensional numeric vector.
#' @param x1 Center of object 1 in the time collision.
#'           Two-dimensional numeric vector.
#' @param x2 Center of object 1 in the time collision.
#'           Two-dimensional numeric vector.
#'
#' @return list of the two two-dimension vectors - new speed for object 1 and 2
#' @export
#'
#' @examples
#' # direct collision, speeds swap
#' bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(0, 1))
#'
#' # 45deg contact angle, makes 90deg turns
#' bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(1, 1))
#' bounce_pair(c(0, 1), c(0, -1), c(0, 1), c(1, 0))
#'
#' # distance is not relevant, direct or 45 deg contact
#' bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(0, 2))
#' bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(2, 2))
#'
#' # Works also for different velocities
#' bounce_pair(c(0, 1), c(0, -2), c(0, 0), c(0, 1))
#' bounce_pair(c(0, 1), c(0, -2), c(0, 0), c(1, 1))
bounce_pair <- function(v1, v2, x1, x2) {
  v1n <- v1 -
    sum((v1 - v2) * (x1 - x2)) /
      sum((x1 - x2)^2) *
      (x1 - x2)
  v2n <- v2 -
    sum((v2 - v1) * (x2 - x1)) /
      sum((x2 - x1)^2) *
      (x2 - x1)
  list(v1n, v2n)
}
