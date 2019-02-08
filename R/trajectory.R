#' Is the object a valid trajectory
#'
#' The function tests an object (tibble) for several requirements:
#' \itemize{
#'   \item columns `time`, `object`, `x` and `y` are present.
#'   \item `object` column contains unique values
#'   \item no value missing in `x` and `y`
#'   \item `x` and `y` are numeric
#' }
#'
#' No restriction about data type of `object` is placed.
#'
#' @param trajectory tibble
#'
#' @return TRUE if conditions above are met, FALSE otherwise
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_valid_position(example_pos)
is_valid_trajectory <- function(trajectory) {
  object <- time <- NULL # pipe check hack
  # requirements:
  # 1) each object*time only once
  # 2) no missing x, y
  tmp <- trajectory %>%
    dplyr::group_by(object, time) %>%
    dplyr::summarise(n = dplyr::n())
  all(tmp$n == 1) &&
    all(!is.na(trajectory$x)) &&
    all(!is.na(trajectory$y)) &&
    is.numeric(trajectory$x) &&
    is.numeric(trajectory$y) &&
    is.numeric(trajectory$time)
}

#' Add random direction
#'
#' Simple fuction to assign random direction to each object in position object.
#' Random means uniformly distributed value from 0 to 2 * pi (0 to 360 deg).
#' The orientation guide using clock analogy:
#' 0 = 3 o'clock, pi/2 = 12 o'clock, pi = 9 o'clock, 3*pi/2 = 6 o'clock
#'
#' @param position tibble
#'
#' @return same tibble with extra direction column
#' @export
#'
#' @examples
add_random_direction <- function(position) {
  position %>%
    dplyr::mutate(direction = stats::runif(dplyr::n(), 0, 2 * pi))
}

make_random_trajectory <- function(start, timescale, settings, step_function) {
  # take start position
  # are direction/speed present? - add if not
  # make step
  stopifnot(length(timescale) > 1)
  if ("direction" %in% names(start)) {
    moment <- start
  } else {
    moment <- start %>% add_random_direction()
  }
  return
  if (!"speed" %in% names(moment)) {
    moment <- moment %>% dplyr::mutate(speed = settings$speed)
  }
  moment <- moment %>% dplyr::mutate(time = timescale[1])

  # moment is position + direction + speed
  # here we have tibble of moments with two columns:
  #   - time (from timescale)
  #   - position (embedded moment tibble for each time)
  # Later we expand tibbles into trajectory tibble
  moment_tbl <- tibble::tibble(time = timescale, position = list(tibble::tibble))
  moment_tbl$position[[1]] <- moment
  for (i in 2:length(timescale)) {
    moment_next <- step_function(moment, timescale[i], settings)
    moment_tbl$position[[i]] <- moment_next
    moment <- moment_next
  }
  moment_tbl %>% tidyr::unnest()
}

step_square_arena <- function(moment, time_next, settings) {
  # moment is position + direction + speed

}

step_direct <- function(moment, time_next, settings) {
  # moment is position + direction + speed
  # just goint in the same direction, possibly bouncing
  x <- y <- speed <- direction <- NULL # pipe check hack
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  if (settings$bounce_off_square) {
    moment <- bounce_off_square(moment, timestep, settings)
  }
  if (settings$bounce_off_others) {
    moment <- bounce_off_others(moment, timestep, settings)
  }
  moment_next <- moment %>%
    dplyr::mutate(
      x = x + cos(direction) * speed * timestep,
      y = y + sin(direction) * speed * timestep,
      time = time_next)
  moment_next
}

bounce_off_others <- function(moment, timestep, settings) {
  object <- x <- y <- speed <- direction <- NULL # pipe check hack
  # extrapolate future
  moment <- moment %>% dplyr::arrange(object)
  moment_next <- moment %>%
    dplyr::mutate(
      x = x + cos(direction) * speed * timestep,
      y = y + sin(direction) * speed * timestep,
      time = NA)
  # check distances
  dist_next <- moment_next %>%
    dplyr::select(x, y) %>%
    as.matrix() %>%
    stats::dist()
  groups <- stats::cutree(stats::hclust(dist_next, "single"),
                          h = settings$min_dist)
  for (i in unique(groups)) {
    involved <- groups == i
    n_involved <- sum(involved)
    if (n_involved == 2) {
      # swap directions
      moment$direction[involved] <- rev(moment$direction[involved])
    }
    if (n_involved > 2) {
      # reverse directions
      moment$direction[involved] <- (moment$direction[involved] + pi) %% (2 * pi)
    }
  }
  moment
}

bounce_off_square <- function(moment, timestep, settings) {
  x <- y <- speed <- direction <- NULL # pipe check hack
  # extrapolate future
  moment_next <- moment %>%
    dplyr::mutate(
      x = x + cos(direction) * speed * timestep,
      y = y + sin(direction) * speed * timestep,
      time = NA)
  # check sides
  beyond_left   <- moment_next$x < min(settings$xlim)
  beyond_right  <- moment_next$x > max(settings$xlim)
  beyond_top    <- moment_next$y > max(settings$ylim)
  beyond_bottom <- moment_next$y < min(settings$ylim)
  # if more than one, then just reverse
  beyond_more   <- (beyond_left + beyond_right + beyond_top + beyond_bottom) > 1
  moment$direction[beyond_more] <-
    (moment$direction[beyond_more] + pi) %% (2 * pi)
  # bounce from left/right
  which_lr <- (beyond_left | beyond_right) & !beyond_more
  moment$direction[which_lr] <-
    (pi - moment$direction[which_lr]) %% (2 * pi)
  # bounce from top/bottom
  which_ud <- (beyond_top | beyond_bottom) & !beyond_more
  moment$direction[which_ud] <-
    (2 * pi - moment$direction[which_ud]) %% (2 * pi)
  moment
}

#' #' Plot object trajectories
#'
#' The function plots trajectory tibble based on x, y, time values and potentially
#' extra values from settings.
#' trajectory is expected to be trajectory tibble,
#' but it may contain extra columns for graphics (same as plot_position):
#' target (Logical vector indicating whether object is target or distractor),
#' fill (Character vector with colour names of object interiors.),
#' border (Character vector with colour names of object borders.).
#'
#' @param trajectory tibble
#' @param settings list with basic properties
#' @param targets Which objects should be treated as targets
#'
#' @return ggplot2 figure
#' @export
#'
#' @examples
#' # sample positions with no other requirements
#' library(dplyr) # for pipe and mutate
#' sett_generate <-
#'   new_settings(xlim = c(-5, 5), ylim = c(-5, 5), min_distance = 2)
#' sett <- new_settings(speed = 1)
#' pos <- generate_positions_random(8, sett_generate)
#' plot_position(pos, sett)
#' moment <- add_random_direction(pos) %>% mutate(speed = 1, time = 0)
#' traj <- make_random_trajectory(moment, 0:5, sett, step_direct)
#' plot_trajectory(traj, new_settings())
#'
#' # first four objects are targets
#' plot_trajectory(traj, sett, 1:4)
plot_trajectory <- function(trajectory,
                            settings = default_settings(),
                            targets = NULL) {
  object <- x <- y <- time <- NULL # pipe check hack
  start_time <- min(trajectory$time)
  fig <- plot_position(trajectory %>% dplyr::filter(time == start_time),
                       settings = settings,
                       targets = targets)
  fig <- fig +
    ggplot2::geom_path(
      ggplot2::aes(x = x, y = y, group = object,
          fill = NULL, colour = NULL),
      data = trajectory %>% dplyr::arrange(time),
      colour = "blue") +
    ggforce::geom_circle(ggplot2::aes(r = settings$r)) +
    NULL
  if (settings$show_labels) {
    fig <-
      fig +
      ggplot2::geom_text(
        ggplot2::aes(x = x, y = y, label = object),
        colour = I("red"))
  }
  fig
}


# # ---
# if (F) {
# library(tidyverse)
# sett_generate <- new_settings(xlim = c(-5, 5), ylim = c(-5, 5), min_distance = 2)
# sett <- new_settings(speed = 1, bounce_off_square = T)
# pos <- generate_positions_random(8, sett_generate)
# plot_position(pos, sett)
# moment <- add_random_direction(pos) %>% mutate(speed = 3, time = 0)
# traj <- make_random_trajectory(moment, seq(0, 5, by = 0.1), sett, step_direct)
# plot_trajectory(traj, new_settings(show_labels = T))
#
# pos2 <- tibble(object = 1:2, x = c(-5, 5), y = c(-5, -5))
# mom2 <- pos2 %>% mutate(direction = c(1, 3) / 4 * pi) %>% mutate(speed = 3, time = 0)
# traj2 <- make_random_trajectory(mom2, seq(0, 5, by = 0.1), sett, step_direct)
# plot_trajectory(traj2, new_settings(show_labels = T))
#
# }
