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
#' @param position tibble
#'
#' @return TRUE if conditions above are met, FALSE otherwise
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_valid_position(example_pos)
is_valid_trajectory <- function(trajectory) {
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

add_random_direction <- function(position) {
  position %>%
    dplyr::mutate(direction = stats::runif(dplyr::n(), 0, 2 * pi))
}

make_random_trajectory <- function(start, timescale, settings, step_function) {
  # take start position
  # are direction present? - add if not
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
  print(moment)
  moment_list <- list()
  moment_list[timescale[1]] <- moment
  for (t in timescale[-1]) {
    moment_next <- step_function(moment, t, settings)
    moment_list[t] <- moment_next
    moment <- moment_next
  }
  print(moment_list)
  dplyr::bind_rows(moment_list)
}
make_random_trajectory(mom, 0:5, new_settings(speed = 1), step_direct)


step_square_arena <- function(moment, time_next, settings) {
  # moment is position + direction + speed

}

step_direct <- function(moment, time_next, settings) {
  # silly function
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  moment_next <- moment %>%
    dplyr::mutate(
      x = x + cos(direction) * speed * timestep,
      y = y + sin(direction) * speed * timestep,
      time = time_next)
  moment_next
}

# pos <- generate_positions_random(8, default_settings(), border_distance = 3)
# traj <- make_random_trajectory(mom, 0:5, new_settings(speed = 1), step_direct)
# mom <- add_random_direction(pos) %>% mutate(speed = 1, time = 0)
# step_direct(mom, 1, default_settings())
