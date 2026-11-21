#' Example trajectory
#'
#' Eight objects in circular arena (radius = 10, speed = 5, 8 seconds).
#'
#' @format A tibble with 648 rows and 7 variables:
#'
#' - `time`: Time for each position, from 0 to 8 with 0.1 steps
#' - `object`: Identifier of each object, 1 to 8
#' - `x`: x-coordinate, in deg
#' - `y`: y-coordinate, in deg
#' - `direction`: Current direction, in rad
#' - `speed`: Object speed, in deg/s
#' - `time1`: ?
#'
#' @source Generated with script
"trajectory8c"

#' Example position
#'
#' Eight objects in circular arena (radius = 10).
#'
#' @format A tibble with 8 rows and 3 variables:
#'
#' - `object`: Identifier of each object, 1 to 8
#' - `x`: x-coordinate, in deg
#' - `y`: y-coordinate, in deg
#'
#' @source Generated with script
"position8c"
