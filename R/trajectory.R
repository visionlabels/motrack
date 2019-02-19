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
  # requirements:
  # 1) each object*time only once
  # 2) no missing x, y
  tmp <- trajectory %>%
    dplyr::group_by(.data$object, .data$time) %>%
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
#' position8c %>% add_random_direction()
add_random_direction <- function(position) {
  position %>%
    dplyr::mutate(direction = stats::runif(dplyr::n(), 0, 2 * pi))
}

#' Create random trajectory
#'
#' @param start Position tibble with starting configuration
#' @param timescale Numeric vector with time points for the trajectory
#' @param settings list with basic properties
#' @param step_function function which is called in every time step
#' to calculate next position in sequence
#' @param ... Extra parameters passed to `step_function`
#'
#' @return Tibble trajectory object
#' @export
#'
#' @examples
#' sett_move <-
#'   new_settings(speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
#'                bounce_off_square = FALSE,
#'                bounce_off_circle = TRUE, circle_bounce_jitter = pi / 6)
#' moment <- position8c %>% add_random_direction()
#' tt <- make_random_trajectory(
#'   moment, seq(0, 8, by = 0.1), sett_move, step_direct
#' )
make_random_trajectory <- function(start, timescale, settings, step_function, ...) {
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
    moment_next <- step_function(moment, timescale[i], settings, ...)
    moment_tbl$position[[i]] <- moment_next
    moment <- moment_next
  }
  moment_tbl %>% tidyr::unnest() %>% dplyr::select(-.data$time1)
}

#' Simple trajectory step function
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next time step to predict
#' @param settings list with basic properties
#'
#' @return Another moment - position tibble with
#' extra columns `direction` and `speed` corresponding to time_next
#' @export
step_direct <- function(moment, time_next, settings) {
  # moment is position + direction + speed
  # just goint in the same direction, possibly bouncing
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  if (settings$bounce_off_square) {
    moment <- bounce_off_square(moment, timestep, settings)
  }
  if (settings$bounce_off_circle) {
    moment <- bounce_off_circle(moment, timestep, settings)
  }
  if (settings$bounce_off_others) {
    moment <- bounce_off_others(moment, timestep, settings)
  }
  moment_next <- moment %>%
    dplyr::mutate(
      x = .data$x + cos(.data$direction) * .data$speed * timestep,
      y = .data$y + sin(.data$direction) * .data$speed * timestep,
      time = time_next)
  moment_next
}

step_zigzag <- function(moment, time_next, settings, ttb = c(.5, 1.5)) {

}

bounce_off_others <- function(moment, timestep, settings) {
  # extrapolate future
  moment <- moment %>% dplyr::arrange(.data$object)
  moment_next <- moment %>%
    dplyr::mutate(
      x = .data$x + cos(.data$direction) * .data$speed * timestep,
      y = .data$y + sin(.data$direction) * .data$speed * timestep,
      time = NA)
  # check distances
  dist_next <- moment_next %>%
    dplyr::select(.data$x, .data$y) %>%
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
  # extrapolate future
  moment_next <- moment %>%
    dplyr::mutate(
      x = .data$x + cos(.data$direction) * .data$speed * timestep,
      y = .data$y + sin(.data$direction) * .data$speed * timestep,
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

bounce_off_circle <- function(moment, timestep, settings) {
  # extrapolate future
  moment_next <- moment %>%
    dplyr::mutate(
      x = .data$x + cos(.data$direction) * .data$speed * timestep,
      y = .data$y + sin(.data$direction) * .data$speed * timestep,
      time = NA)
  # arena radius
  dims <- sort(abs(c(settings$xlim, settings$ylim)))
  if (length(unique(dims)) > 1) {
    stop("Multiple radius options for arena.")
  }
  arena_radius <- unique(dims)[1]
  midpoint_x   <- 0
  midpoint_y   <- 0
  # check sides
  beyond_border <- sqrt(
    (moment_next$x - midpoint_x) ^ 2 +
    (moment_next$y - midpoint_y) ^ 2
  ) > arena_radius
  vector_to_midpoint <-
    (atan2(moment$y - midpoint_y, moment$x - midpoint_x) + pi) %% (2 * pi)
  jitter <- stats::runif(
    nrow(moment),
    min = -settings$circle_bounce_jitter,
    max =  settings$circle_bounce_jitter)
  moment$direction[beyond_border] <-
    (vector_to_midpoint[beyond_border] + jitter[beyond_border]) %% (2 * pi)
  moment
}

#' Plot object trajectories
#'
#' The function plots trajectory tibble based on x, y,
#' time values and potentially
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
#' plot_trajectory(trajectory8c, default_settings())
#'
#' # first four objects are targets
#' plot_trajectory(trajectory8c, default_settings(), 1:4)
plot_trajectory <- function(trajectory,
                            settings = default_settings(),
                            targets = NULL) {
  start_time <- min(trajectory$time)
  fig <- plot_position(trajectory %>% dplyr::filter(.data$time == start_time),
                       settings = settings,
                       targets = targets)
  fig <- fig +
    ggplot2::geom_path(
      ggplot2::aes_string(x = "x", y = "y", group = "object",
          fill = NULL, colour = NULL),
      data = trajectory %>% dplyr::arrange(.data$time),
      colour = "blue") +
    ggforce::geom_circle(ggplot2::aes_string(r = "settings$r")) +
    NULL
  if (settings$show_labels) {
    fig <-
      fig +
      ggplot2::geom_text(
        ggplot2::aes_string(x = "x", y = "y", label = "object"),
        colour = I("red"))
  }
  fig
}

estimate_position_for_time <- function(trajectory, timepoint) {
  stopifnot(timepoint <= max(trajectory$time))
  stopifnot(timepoint >= min(trajectory$time))
  # find one before and one after
  time_all    <- sort(unique(trajectory$time))
  time_before <- max(time_all[time_all <= timepoint])
  time_after  <- min(time_all[time_all >= timepoint])
  points_before <- trajectory %>%
    dplyr::filter(.data$time == time_before) %>%
    dplyr::arrange(.data$object)
  points_after <- trajectory %>%
    dplyr::filter(.data$time == time_after) %>%
    dplyr::arrange(.data$object)
  stopifnot(all(points_before$object == points_after$object))
  if (time_after > time_before) {
    # interpolate - contribution of "after"
    timestep <- (timepoint - time_before) / (time_after - time_before)
    res <- points_before
    res$x <- res$x + timestep * (points_after$x - points_before$x)
    res$y <- res$y + timestep * (points_after$y - points_before$y)
    res
  } else {
    # "before" and "after" are the same, so return one of them
    points_before
  }
}

#' Render video from trajectory
#'
#' The function repeatedly calls plot_position function to generate preview
#' of the trajectory motion.
#' Extra time is added before and after the motion sequence (2 seconds each).
#'
#' @param filename the file name of output video (e.g. "trajectory.mp4")
#' @param trajectory tibble with trajectory data
#' @param settings list with basic properties
#' @param targets Which objects should be treated as targets
#' @param outdir output directory where the video is saved
#'
#' @return No return value
#' @export
#'
#' @examples
#' # set ffmpeg in necessary
#'
#' # ani.options(ffmpeg = "/PATH/TO/ffmpeg/ffmpeg")
#' render_trajectory_video("trajectory.mp4", trajectory8c,
#'   new_settings(show_labels = T), targets = 1:4)
render_trajectory_video <- function(filename,
                                   trajectory,
                                   settings = default_settings(),
                                   targets = NULL,
                                   outdir = getwd()) {
  # animation parameters
  fps <- 25
  video_width  <- 600
  video_height <- 600
  preview_seconds <- 2
  respond_seconds <- 2

  tmin <- min(trajectory$time)
  tmax <- max(trajectory$time)
  tlen <- tmax - tmin
  oopt <- animation::ani.options(
    interval = 1 / fps, nmax = fps * tlen * 2,
    outdir = outdir, ani.width = video_width, ani.height = video_height
  )
  animation::saveVideo({
    # preview
    for (i in 1:(preview_seconds * fps)) {
      p <- estimate_position_for_time(trajectory, tmin)
      fig <- plot_position(p, settings = settings, targets = targets)
      print(fig)
    }
    for (tim in seq(tmin, tmax, 1 / fps)) {
      p <- estimate_position_for_time(trajectory, tim)
      fig <- plot_position(p, settings = settings, targets = targets)
      print(fig)
    }
    for (i in 1:(respond_seconds * fps)) {
      p <- estimate_position_for_time(trajectory, tmax)
      fig <- plot_position(p, settings = settings, targets = targets)
      print(fig)
    }
  }, video.name = filename, other.opts = "-pix_fmt yuv420p -b 600k", clean = T)
  animation::ani.options(oopt)
}

#' Save trajectory to file
#'
#' @param trajectory Trajectory object to be exported to file
#' @param filename Name of output file (e.g., .csv)
#' @param delim Single character used to separate fields within a record. Default `","`
#'
#' @return Invisibly returns data in wide format (as saved to file)
#' @export
#'
#' @examples
#' filename <- "test-csv"
#' save_trajectory(trajectory8c, filename)
#' unlink(filename)
save_trajectory <- function(trajectory, filename, delim = ",") {
  # we aim for format: time, x1, y1, x2, y2 ...
  # sort them by names and check if same
  xpart <- trajectory %>%
    dplyr::select(.data$time, .data$object, .data$x) %>%
    tidyr::spread(key = .data$object, value = .data$x) %>%
    dplyr::select(noquote(order(colnames(.)))) %>%
    dplyr::select(.data$time, dplyr::everything())
  ypart <- trajectory %>%
    dplyr::select(.data$time, .data$object, .data$y) %>%
    tidyr::spread(key = .data$object, value = .data$y) %>%
    dplyr::select(sort(names(.))) %>%
    dplyr::select(.data$time, dplyr::everything())
  n <- ncol(xpart) - 1
  stopifnot(all(names(xpart) == names(ypart)))
  stopifnot(all(xpart$time == ypart$time))
  names(xpart)[-1] <- stringr::str_c("x", names(xpart)[-1])
  names(ypart)[-1] <- stringr::str_c("y", names(ypart)[-1])
  merged <- dplyr::bind_cols(
    xpart,
    ypart %>% dplyr::select(-.data$time)
  )
  column_index <- c(1:n, 0.5 + (1:n))
  merged <- merged[, c(1, order(column_index) + 1)]
  merged %>%
    readr::write_delim(filename, delim = delim, col_names = F)
  invisible(merged)
}

#' Load trajectory from file
#'
#' @param filename Name of input file (e.g., .csv)
#' @param delim Single character used to separate fields within a record. Default `","`
#' @param ... Parameters passed to `readr::read_delim`
#'
#' @return Tibble trajectory object with loaded data
#' @export
#'
#' @examples
load_trajectory <- function(filename, delim = ",", ...) {
  input <- readr::read_delim(
    filename,
    delim = delim, col_names = F,
    col_types = readr::cols(.default = readr::col_double()),
    ...)
  ncol_all <- ncol(input)
  n <- (ncol_all - 1) %/% 2
  stopifnot(ncol_all == n + n + 1)
  xcols <- (1:n) * 2
  ycols <- xcols + 1
  xx <- as.numeric(as.matrix(input[, xcols]))
  yy <- as.numeric(as.matrix(input[, ycols]))
  trajectory <- tibble::tibble(
    time = rep(input %>% dplyr::select(1) %>% dplyr::pull(), n),
    object = rep(1:n, each = nrow(input)),
    x = xx,
    y = yy
  ) %>% dplyr::arrange(.data$time)
  trajectory
}

#' Sanity check for trajectory
#'
#' Sometimes, we want have extra requirements for trajectories. The idea is to
#' have automated check about possible errors or artifacts which could appear
#' in trajectory data.
#'
#' We test for following conditions:
#' \itemize{
#'   \item *Sudden motion changes* - Sometimes objects bounce too frequently,
#'   which may attract attention. For each object, we test what is the minimal
#'   time interval between two consecutive large direction changes.
#'   \item *Minimum interobject distance* - The minimum distance is checked
#'   when generating positions and also in each step of generating trajectories.
#'   Here, we make final check if the minimum distance is complied.
#' }
#'
#' @param trajectory Tibble object with trajectory
#' @param change_angle Angle value in degrees.
#' Direction changes beyond this value are considered large.
#' @param min_interval Time in seconds, which should pass between
#' too large direction changes.
#' @param min_distance Minimum inter-object spacing, which should be
#' complied in every frame.
#'
#' @return Logical. `TRUE` if all conditions are passed.
#' @export
#'
#' @examples
#' validate_trajectory(trajectory8c)
validate_trajectory <- function(trajectory, change_angle = 30,
                                min_interval = 0.2,
                                min_distance = 1) {
  # for each object we will check the direction changes and report if there
  # are two changes too soon after each other
  for (o in unique(trajectory$object)) {
    ok <- validate_object_direction_change(
      trajectory %>% dplyr::filter(.data$object == o),
      change_angle, min_interval
    )
    if (!ok) return(F)
  }
  for (tt in unique(trajectory$time)) {
    ok <- is_distance_at_least(
      trajectory %>% dplyr::filter(.data$time == tt),
      min_distance
    )
    if (!ok) return(F)
  }
  return(T)
}

#' Check direction changes for single object trajectory
#'
#' @param trajectory1 Trajectory featuring only one object
#' @param change_angle Angle value in degrees.
#' Direction changes beyond this value are considered large.
#' @param min_interval Time in seconds, which should pass between
#' too large direction changes.
#'
#' @return Logical. `TRUE` if each pair of consecutive bounces
#' is separated by at least `min_interval` seconds.
#' @export
validate_object_direction_change <- function(trajectory1,
                                             change_angle = 30,
                                             min_interval = 0.2) {
  # check direction changes in single object
  trajectory1 <- trajectory1 %>% dplyr::arrange(.data$time)
  change_min <- change_angle / 360 * 2 * pi
  # calculate direction
  tr <- trajectory1 %>%
    dplyr::mutate(
      dx = dplyr::lead(.data$x) - .data$x,
      dy = dplyr::lead(.data$y) - .data$y,
      direction = atan2(.data$dy, .data$dx) %% (2 * pi))
  tr <- tr %>%
    dplyr::mutate(
      dirchange = .data$direction - dplyr::lag(.data$direction),
      dirchange_large =
        (.data$dirchange > change_min) &
        (.data$dirchange < 2 * pi - change_min))
  changed <- tr %>%
    dplyr::filter(.data$dirchange_large) %>%
    dplyr::arrange(.data$time)
  all(diff(changed$time) >= min_interval)
}

