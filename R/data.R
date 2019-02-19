#' Example trajectory
#'
#' Eight objects in circular arena (radius = 10, speed = 5, 8 seconds).
#'
#' @format A tibble with 648 rows and 7 variables:
#' \describe{
#'   \item{time}{Time for each position, from 0 to 8 with 0.1 steps}
#'   \item{object}{Identifier of each object, 1 to 8}
#'   \item{x}{x-coordinate, in deg}
#'   \item{y}{y-coordinate, in deg}
#'   \item{direction}{Current direction, in rad}
#'   \item{speed}{Object speed, in deg/s}
#'   \item{time1}{?}
#' }
#' @source Generated with script
"trajectory8c"

# set.seed(123)
# sett_generate <-
#   new_settings(xlim = c(-7, 7), ylim = c(-7, 7), min_distance = 2,
#                arena_shape = "circle")
# sett_move <-
#   new_settings(speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
#                bounce_off_square = F,
#                bounce_off_circle = T, circle_bounce_jitter = pi / 6)
# sett_show <-
#   new_settings(show_labels = T)
# pos <- generate_positions_random(8, sett_generate)
# plot_position(pos, sett_move)
# trajectory8c <- make_random_trajectory(pos, seq(0, 8, by = 0.1),
#                                        sett_move, step_direct)
#
# plot_trajectory(trajectory8c, sett_show)
# usethis::use_data(trajectory8c, overwrite = T, compress = "gzip")
