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

#' Get current speed
#'
#' If speeds are constant, returns `speed` column of the `moment`.
#' If speeds are functions, evaluates the speed functions and returns their results.
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param cur_time Current time
#'
#' Speeds can be defined as functions and
#' thus it is possible to make trajectories with speeds sensitive
#' to time (harmonic movements or accelerations)
#' or location (slower speeds in the center).
#'
#' If there is only single speed function, all speeds are evaluated
#' in one call. If there is more than one speed function,
#' speeds are evaluated individually, one object at time.
#'
#' Speed functions follow the template
#' `function(time, moment)` and return a vector of speeds.
#' They should be written to handle both vector calculations
#' (moment is a tibble with multiple rows) and individual values
#' (moment is a one-row-tibble).
#'
#' Speed functions could be introduced via `settings$speed` or
#' added directly into the `speed` column of the initial position tibble.
#' When passing functions they should be encapsulated in a list
#' (`settings$speed <- c(my_fun)`).
#'
#' @return numerical vector of speed
#' @export
#'
#' @examples
#' f2 <- function(time, moment) {
#'   5 + cos(time) + (moment$object %% 2) * 2
#' }
#' sett_move <-
#'   new_settings(
#'     speed = c(f2), xlim = c(-9, 9), ylim = c(-9, 9),
#'     bounce_off_square = FALSE,
#'     bounce_off_circle = TRUE, circle_bounce_jitter = pi / 6
#'   )
#' moment <- position8c %>% add_random_direction()
#' tt <- make_random_trajectory(
#'    moment, seq(0, 8, by = 0.1), sett_move, step_direct
#' )
cur_speed <- function(moment, cur_time = NULL) {
  if (is.numeric(moment$speed)) {
    s <- moment$speed
  }
  if (is.list(moment$speed)) {
    # single speed function?
    if (length(unique(moment$speed)) == 1) {
      f <- moment$speed[[1]]
      s <- f(cur_time, moment)
    }
    # multiple speed functions?
    s <- numeric(nrow(moment))
    for (i in 1:length(s)) {
      f <- moment$speed[[i]]
      s[i] <- f(cur_time, moment[i, ])
    }
  }
  s
}

#' Extrapolates object positions based on current state and time
#'
#' Intended for the use in custom step functions.
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param timestep How far into future should the objects move (linearly)
#' @param cur_time Current time, for time-sensitive speed functions (e.g., accelerations)
#' @param new_time New time value for the returned object. If omitted, it is calculated automatically as `cur_time + timestep`
#'
#' @return Another moment - position tibble with
#' extra columns `direction` and `speed` corresponding to time_next
#' @export
#'
#' @examples
#' moment <- position8c %>% add_random_direction() %>%
#'   dplyr::mutate(speed = 3)
#' extrapolate_moment(moment, 0.1, 0)
extrapolate_moment <- function(moment, timestep, cur_time, new_time = NULL) {
  s <- cur_speed(moment, cur_time)
  if (is.null(new_time)) new_time <- cur_time + timestep
  moment_next <- moment %>%
    dplyr::mutate(
      x = .data$x + cos(.data$direction) * s * timestep,
      y = .data$y + sin(.data$direction) * s * timestep,
      time = new_time
    )
  moment_next
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
#'   new_settings(
#'     speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
#'     bounce_off_square = FALSE,
#'     bounce_off_circle = TRUE, circle_bounce_jitter = pi / 6
#'   )
#' moment <- position8c %>% add_random_direction()
#' tt <- make_random_trajectory(
#'   moment, seq(0, 8, by = 0.1), sett_move, step_direct
#' )
make_random_trajectory <- function(
  start,
  timescale,
  settings,
  step_function,
  ...
) {
  # take start position
  # are direction/speed present? - add if not
  # make step
  stopifnot(length(timescale) > 1)
  if ("direction" %in% names(start)) {
    moment <- start
  } else {
    moment <- start %>% add_random_direction()
  }

  if (!"speed" %in% names(moment)) {
    moment <- moment %>% dplyr::mutate(speed = settings$speed)
  }
  moment <- moment %>% dplyr::mutate(time = timescale[1])

  # moment is position + direction + speed
  # here we have tibble of moments with two columns:
  #   - time (from timescale)
  #   - position (embedded moment tibble for each time)
  # Later we expand tibbles into trajectory tibble
  moment_tbl <- tibble::tibble(
    time = timescale,
    position = list(tibble::tibble)
  )
  moment_tbl$position[[1]] <- moment
  for (i in 2:length(timescale)) {
    if (settings$bounce_off_square) {
      moment <- bounce_off_square(moment, timescale[i], settings)
    }
    if (settings$bounce_off_circle) {
      moment <- bounce_off_circle(moment, timescale[i], settings)
    }
    if (settings$bounce_off_inside) {
      moment <- bounce_off_inside(moment, timescale[i], settings)
    }
    if (settings$bounce_off_others) {
      moment <- bounce_off_others(moment, timescale[i], settings)
    }
    moment_next <- step_function(moment, timescale[i], settings, ...)

    moment_tbl$position[[i]] <- moment_next
    moment <- moment_next
  }
  moment_tbl %>%
    dplyr::select(-.data$time) %>%
    tidyr::unnest(cols = c("position"))
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
#' @family step_functions
step_direct <- function(moment, time_next, settings) {
  # moment is position + direction + speed
  # just going in the same direction, possibly bouncing
  time_now <- moment$time[1]
  timestep <- time_next - time_now

  moment_next <-
    extrapolate_moment(moment, timestep, time_now, time_next)
  moment_next
}

#' Zigzag trajectory step function
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next time step to predict
#' @param settings list with basic properties
#' @param ttt Time-to-turn - time when the object should pick new direction
#' @param syncstart Logical, if time-to-turn should be synchronized
#' when it is generated for the first time
#'
#' @return Another moment - position tibble with
#' extra columns `direction`, `speed` and `ttt` corresponding to time_next
#' @export
#' @family step_functions
step_zigzag <- function(
  moment,
  time_next,
  settings,
  ttt = c(.5, 1.5),
  syncstart = F
) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # ttt is time-to-turn - when object should turn randomly
  n <- nrow(moment)
  if (!"ttt" %in% names(moment)) {
    times <- stats::runif(n, min = ttt[1], max = ttt[2])
    if (!syncstart) {
      passed <- stats::runif(n, min = 0, max = ttt[1])
      times <- times - passed
    }
    moment <- moment %>% dplyr::mutate(ttt = times)
  }
  which_turn <- moment$ttt < time_next
  if (any(which_turn)) {
    moment$direction[which_turn] <-
      stats::runif(sum(which_turn), 0, 2 * pi)
    moment$ttt[which_turn] <-
      moment$ttt[which_turn] +
      stats::runif(sum(which_turn), min = ttt[1], max = ttt[2])
  }
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, time_next)
  moment_next
}

#' Wait and move step function
#'
#' Step function for wait-and-move movement pattern
#'
#' Using this step function, objects changes between two states - moving and waiting.
#' After each state, new state is sampled with given probability.
#'
#' When probability of waiting is set to zero, objects move in a same way as in  \link{step_zigzag} step function
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next time step to predict
#' @param settings List with basic properties
#' @param move_time Range of move phase duration
#' @param wait_time Range of wait phase duration
#' @param wait_prob Probability of waiting
#' @param syncstart Logical, if time-to-change should be synchronized
#' when it is generated for the first time
#'
#' @return Another moment - position tibble with
#' extra columns `direction`, `speed`, `speed_scale`, `ttc` and `mov_state`
#' @export
#' @family step_functions
step_waitandmove <- function(
  moment,
  time_next,
  settings,
  move_time = c(.5, 1.5),
  wait_time = c(.2, .5),
  wait_prob = 0.1,
  syncstart = F
) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # move_time is time of movement
  # wait_time is time os waiting
  # after this time, object change its statewhen object change their state (either move of wait)
  n <- nrow(moment)

  if (!"mov_state" %in% names(moment)) {
    moment <- moment %>%
      dplyr::mutate(
        mov_state = sample(
          c("w", "m"),
          n,
          replace = T,
          prob = c(wait_prob, 1 - wait_prob)
        )
      )
  }
  # all objects are in certain state
  if (!"ttc" %in% names(moment)) {
    # first call, time to change should be defined based on the their state
    times <- vector(mode = "numeric", length = n)
    times[moment$mov_state == "m"] <- stats::runif(
      sum(moment$mov_state == "m"),
      min = move_time[1],
      max = move_time[2]
    )
    times[moment$mov_state == "w"] <- stats::runif(
      sum(moment$mov_state == "w"),
      min = wait_time[1],
      max = wait_time[2]
    )
    if (any(is.na(times))) {
      stop("Missing movement state")
    }
    if (!syncstart) {
      passed <- vector(mode = "numeric", length = n)
      passed[moment$mov_state == "m"] <- stats::runif(
        sum(moment$mov_state == "m"),
        min = 0,
        max = move_time[1]
      )
      passed[moment$mov_state == "w"] <- stats::runif(
        sum(moment$mov_state == "w"),
        min = 0,
        max = wait_time[1]
      )
      times <- times - passed
    }

    moment <- moment %>% dplyr::mutate(ttc = times)
  }
  # all objects have defined time to change

  if (!"speed_scale" %in% names(moment)) {
    speed_scales <- vector(mode = "numeric", length = n)
    speed_scales[moment$mov_state == "m"] <- 1
    speed_scales[moment$mov_state == "w"] <- 0
    moment <- moment %>% dplyr::mutate(speed_scale = speed_scales)
  }

  # which objects should change their state
  which_change <- moment$ttc < time_next

  if (any(which_change)) {
    # we have some objects that need to change their behavior
    # change state of objects
    moment$mov_state[which_change] <- sample(
      c("w", "m"),
      sum(which_change),
      replace = T,
      prob = c(wait_prob, 1 - wait_prob)
    )

    # do computations separately for moving and waiting objects
    which_wait <- which_change & moment$mov_state == "w"
    which_move <- which_change & moment$mov_state == "m"

    # set speed scaling factor
    # this is basically a trick, how to stop object from moving, we are not modifying the speed, as it could be used by other functions
    moment$speed_scale[which_wait] <- 0 # stop the movement
    moment$speed_scale[which_move] <- 1 # start the movement

    # for moving objects, set new direction
    moment$direction[which_move] <-
      stats::runif(sum(which_move), 0, 2 * pi)

    # set time to change
    moment$ttc[which_move] <- # for moving objects sample from move_time range
      moment$ttc[which_move] +
      stats::runif(sum(which_move), min = move_time[1], max = move_time[2])

    moment$ttc[which_wait] <- # for waiting objects sample from wait_time range
      moment$ttc[which_wait] +
      stats::runif(sum(which_wait), min = wait_time[1], max = wait_time[2])
  }
  # compute new positions, scaling factors sets, whether objects are moving or not
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, time_next)
  moment_next
}

#' Von Mises trajectory step function
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next time step to predict
#' @param settings list with basic properties
#' @param kappa concentration parameter
#'
#' @return Another moment - position tibble with
#' extra columns `direction` and `speed` corresponding to time_next
#' @export
#' @family step_functions
step_vonmises <- function(moment, time_next, settings, kappa) {
  # moment is position + direction + speed
  # just goint in the same direction, possibly bouncing
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  n <- nrow(moment)
  moment_next <- moment %>%
    dplyr::mutate(
      direction = (.data$direction +
        as.numeric(
          circular::rvonmises(n, circular::circular(0), kappa = kappa)
        )) %%
        (2 * pi)
    )
  moment_next <-
    extrapolate_moment(moment_next, timestep, time_now, time_next)
  moment_next
}


#' Calculate collision vectors for two objects
#'
#' @param v1 Velocity vector of object 1. Two-dimensional numeric vector.
#' @param v2 Velocity vector of object 2. Two-dimensional numeric vector.
#' @param x1 Center of object 1 in the time collision. Two-dimensional numeric vector.
#' @param x2 Center of object 1 in the time collision. Two-dimensional numeric vector.
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

#' Check for inter-object bouncing and recalculate directions
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next timepoint
#' @param settings list with basic properties
#'
#' @return Like `moment`, but some directions are adjusted (reversed, swapped),
#' if there were a collision in next time point (`timestep`)
#' @export
bounce_off_others <- function(moment, time_next, settings) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # extrapolate future
  moment <- moment %>% dplyr::arrange(.data$object)

  moment_next <-
    extrapolate_moment(moment, timestep, time_now, NA)

  # check distances
  dist_next <- moment_next %>%
    dplyr::select(.data$x, .data$y) %>%
    as.matrix() %>%
    stats::dist()
  groups <- stats::cutree(
    stats::hclust(dist_next, "single"),
    h = settings$min_distance
  )
  for (i in unique(groups)) {
    involved <- groups == i
    n_involved <- sum(involved)
    if (n_involved == 2) {
      # swap directions
      if (settings$simplified_bouncing) {
        moment$direction[involved] <- rev(moment$direction[involved])
      } else {
        v1 <- c(
          cos(moment$direction[involved][1]),
          sin(moment$direction[involved][1])
        )
        v2 <- c(
          cos(moment$direction[involved][2]),
          sin(moment$direction[involved][2])
        )
        x1 <- c(moment$x[involved][1], moment$y[involved][1])
        x2 <- c(moment$x[involved][2], moment$y[involved][2])
        after <- bounce_pair(v1, v2, x1, x2)
        moment$direction[involved][1] <- atan2(after[[1]][2], after[[1]][1])
        moment$direction[involved][2] <- atan2(after[[2]][2], after[[2]][1])
      }
    }
    if (n_involved > 2) {
      # reverse directions
      moment$direction[involved] <- (moment$direction[involved] + pi) %%
        (2 * pi)
    }
  }
  moment
}

#' Check for square-arena bouncing and recalculate directions
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next timepoint
#' @param settings list with basic properties
#'
#' @return Like `moment`, but some directions are adjusted (reversed, swapped),
#' if there were a collision with arena border in next time point (`timestep`)
#' @export
bounce_off_square <- function(moment, time_next, settings) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # extrapolate future
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, NA)
  # check sides
  beyond_left <- moment_next$x < min(settings$xlim)
  beyond_right <- moment_next$x > max(settings$xlim)
  beyond_top <- moment_next$y > max(settings$ylim)
  beyond_bottom <- moment_next$y < min(settings$ylim)
  # if more than one, then just reverse
  beyond_more <- (beyond_left + beyond_right + beyond_top + beyond_bottom) > 1
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

#' Check for circular arena bouncing and recalculate directions
#'
#' @param moment Position tibble with extra columns `direction` and `speed`
#' @param time_next Next timepoint
#' @param settings list with basic properties
#'
#' @return Like `moment`, but some directions are adjusted (reversed, swapped),
#' if there were a collision with arena border in next time point (`timestep`)
#' @export
bounce_off_circle <- function(moment, time_next, settings) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # extrapolate future
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, NA)
  # arena radius
  dims <- sort(c(abs(diff(settings$xlim)), abs(diff(settings$ylim))))
  if (length(unique(dims)) > 1) {
    stop("Multiple radius options for arena.")
  }
  arena_radius <- unique(dims)[1] / 2
  midpoint_x <- mean(settings$xlim)
  midpoint_y <- mean(settings$ylim)
  # check sides
  beyond_border <- sqrt(
    (moment_next$x - midpoint_x)^2 +
      (moment_next$y - midpoint_y)^2
  ) >
    arena_radius
  vector_to_midpoint <-
    (atan2(moment$y - midpoint_y, moment$x - midpoint_x) + pi) %% (2 * pi)
  jitter <- stats::runif(
    nrow(moment),
    min = -settings$circle_bounce_jitter,
    max = settings$circle_bounce_jitter
  )
  moment$direction[beyond_border] <-
    (vector_to_midpoint[beyond_border] + jitter[beyond_border]) %% (2 * pi)
  moment
}

bounce_off_inside <- function(moment, time_next, settings) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # extrapolate future
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, NA)
  # arena radius
  dims <- sort(c(abs(diff(settings$xlim)), abs(diff(settings$ylim))))
  if (length(unique(dims)) > 1) {
    stop("Multiple radius options for arena.")
  }
  arena_radius <- unique(dims)[1] / 2
  midpoint_x <- mean(settings$xlim)
  midpoint_y <- mean(settings$ylim)
  # check sides
  beyond_inside_border <- sqrt(
    (moment_next$x - midpoint_x)^2 +
      (moment_next$y - midpoint_y)^2
  ) <
    settings$arena_inside_radius
  vector_to_midpoint <-
    (atan2(moment$y - midpoint_y, moment$x - midpoint_x)) %% (2 * pi)
  jitter <- stats::runif(
    nrow(moment),
    min = -settings$circle_bounce_jitter,
    max = settings$circle_bounce_jitter
  )
  moment$direction[beyond_inside_border] <-
    (vector_to_midpoint[beyond_inside_border] + jitter[beyond_inside_border]) %%
    (2 * pi)
  moment
}

#' Simplify trajectory
#'
#' To save space we can simplify the trajectory data. Trajectories
#' usually have very fine timeline, because small time steps allows to
#' (1) minimize potential collision artifacts,
#' (2) allow smooth directional changes like in `step_vonmises`.
#' When simplifying, we go through time points and detect
#' for which timepoints we can interpolate all object positions.
#' We keep these positions and remove the rest.
#' The resulting trajectory has irregular timeline.
#' If `snapshot`, then for each
#' timepoint positions of all objects are included
#' (i.e., also positions for these, which could be interpolated for a given timepoint).
#' If `snapshot == FALSE`, then only the data for changing points are included.
#'
#' @param trajectory Trajectory data
#' @param snapshot Logical, if each timepoint should include data for all point.
#' @param eps Direction changes smaller than `eps` (in radians) are ignored
#'
#' @return Subset of the original data. The timeline is irregular,
#' the positions which could be interpolated are omitted.
#' @export
#'
#' @examples
#' library(ggplot2)
#' sett_move <-
#' new_settings(
#'   speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
#'   bounce_off_square = FALSE,
#'   bounce_off_circle = TRUE, circle_bounce_jitter = pi / 6
#' )
#' moment <- position8c %>% add_random_direction()
#' tt1 <- make_random_trajectory(
#'   moment, seq(0, 8, by = 0.1), sett_move, step_direct)
#' tt2 <- simplify_trajectory(tt1, snapshot = TRUE)
#' tt3 <- simplify_trajectory(tt1, snapshot = FALSE)
#' ggplot(tt1, aes(x = x, y = y, group = object)) +
#'   geom_path() +
#'   geom_point(data = tt2)
#' # number of lines reduced down to:
#' nrow(tt2) / nrow(tt1)
#' nrow(tt3) / nrow(tt1)
simplify_trajectory <- function(trajectory, snapshot = T, eps = 1e-4) {
  # calculate directions
  calculate_direction_for_single_object <- function(t1) {
    t1 %>%
      dplyr::arrange(.data$time) %>%
      dplyr::mutate(
        dx = dplyr::lead(.data$x) - .data$x,
        dy = dplyr::lead(.data$y) - .data$y,
        direction = atan2(.data$dy, .data$dx) %% (2 * pi)
      )
  }
  large_or_na <- function(x) {
    is.na(x) | abs(x) > eps
  }
  small_and_not_na <- function(x) {
    !is.na(x) & abs(x) < eps
  }
  find_critical_timepoints <- function(t1) {
    # assumptions: one object, time sorted, direction column
    # keep: 1st, last, direction changes
    t1 %>%
      dplyr::mutate(
        keep = large_or_na(.data$direction - dplyr::lag(.data$direction))
      )
  }
  events <- trajectory %>%
    dplyr::group_split(.data$object) %>%
    purrr::map(~ dplyr::arrange(., .data$time)) %>%
    purrr::map(~ calculate_direction_for_single_object(.)) %>%
    purrr::map(~ find_critical_timepoints(.)) %>%
    purrr::map(~ dplyr::filter(., .data$keep)) %>%
    dplyr::bind_rows()
  if (snapshot)
    trajectory %>% dplyr::filter(.data$time %in% unique(events$time)) else
    # we return only the original columns
    events %>% dplyr::select(tidyselect::all_of(names(trajectory)))
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
#' @param ... Parameters passed to `plot_position`
#'
#' @return ggplot2 figure
#' @export
#'
#' @examples
#' plot_trajectory(trajectory8c, default_settings())
#'
#' # first four objects are targets
#' plot_trajectory(trajectory8c, default_settings(), 1:4)
plot_trajectory <- function(
  trajectory,
  settings = default_settings(),
  targets = NULL,
  ...
) {
  start_time <- min(trajectory$time)
  fig <- plot_position(
    trajectory %>% dplyr::filter(.data$time == start_time),
    settings = settings,
    targets = targets,
    ...
  )
  fig <- fig +
    ggplot2::geom_path(
      ggplot2::aes_string(
        x = "x",
        y = "y",
        group = "object",
        fill = NULL,
        colour = NULL
      ),
      data = trajectory %>% dplyr::arrange(.data$time),
      colour = "blue"
    ) +
    ggforce::geom_circle(ggplot2::aes_string(r = "settings$r")) +
    NULL
  if (settings$show_labels) {
    fig <-
      fig +
      ggplot2::geom_text(
        ggplot2::aes_string(x = "x", y = "y", label = "object"),
        colour = I("red")
      )
  }
  fig
}

estimate_position_for_time <- function(trajectory, timepoint) {
  stopifnot(timepoint <= max(trajectory$time))
  stopifnot(timepoint >= min(trajectory$time))
  # find one before and one after
  time_all <- sort(unique(trajectory$time))
  time_before <- max(time_all[time_all <= timepoint])
  time_after <- min(time_all[time_all >= timepoint])
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
#' @param ... Parameters passed to `plot_position`
#'
#' @return No return value
#' @export
#'
#' @examples
#' \dontrun{
#' # this example can take 30 seconds to run
#' # set ffmpeg in necessary
#' # ani.options(ffmpeg = "/PATH/TO/ffmpeg/ffmpeg")
#' render_trajectory_video("trajectory.mp4", trajectory8c,
#'   new_settings(show_labels = TRUE),
#'   targets = 1:4
#' )
#' }
render_trajectory_video <- function(
  filename,
  trajectory,
  settings = default_settings(),
  targets = NULL,
  outdir = getwd(),
  ...
) {
  # animation parameters
  fps <- 25
  video_width <- 600
  video_height <- 600
  preview_seconds <- 2
  respond_seconds <- 2

  tmin <- min(trajectory$time)
  tmax <- max(trajectory$time)
  tlen <- tmax - tmin
  oopt <- animation::ani.options(
    interval = 1 / fps,
    nmax = fps * tlen * 2,
    outdir = outdir,
    ani.width = video_width,
    ani.height = video_height
  )
  animation::saveVideo(
    {
      # preview
      for (i in 1:(preview_seconds * fps)) {
        p <- estimate_position_for_time(trajectory, tmin)
        fig <- plot_position(p, settings = settings, targets = targets, ...)
        print(fig)
      }
      for (tim in seq(tmin, tmax, 1 / fps)) {
        p <- estimate_position_for_time(trajectory, tim)
        fig <- plot_position(p, settings = settings, targets = targets, ...)
        print(fig)
      }
      for (i in 1:(respond_seconds * fps)) {
        p <- estimate_position_for_time(trajectory, tmax)
        fig <- plot_position(p, settings = settings, targets = targets, ...)
        print(fig)
      }
    },
    video.name = filename,
    other.opts = "-pix_fmt yuv420p -b 600k",
    clean = T
  )
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
#' filename <- "test.csv"
#' save_trajectory(trajectory8c, filename)
#' unlink(filename)
save_trajectory <- function(trajectory, filename, delim = ",") {
  # we aim for format: time, x1, y1, x2, y2 ...
  # sort them by names and check if same
  xpart <- trajectory %>%
    dplyr::select(.data$time, .data$object, .data$x) %>%
    tidyr::spread(key = .data$object, value = .data$x) %>%
    dplyr::select(sort(names(.))) %>%
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
  merged <- merged %>% dplyr::select(c(1, order(column_index) + 1))
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
load_trajectory <- function(filename, delim = ",", ...) {
  input <- readr::read_delim(
    filename,
    delim = delim,
    col_names = F,
    col_types = readr::cols(.default = readr::col_double()),
    ...
  )
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
  ) %>%
    dplyr::arrange(.data$time)
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
#' @param verbose If `TRUE`, results for each subtest are printed on console.
#' Default `FALSE`.
#'
#' @return Logical. `TRUE` if all conditions are passed.
#' @export
#'
#' @examples
#' validate_trajectory(trajectory8c)
validate_trajectory <- function(
  trajectory,
  change_angle = 30,
  min_interval = 0.2,
  min_distance = 1,
  verbose = F
) {
  # for each object we will check the direction changes and report if there
  # are two changes too soon after each other
  crit1 <- T
  for (o in unique(trajectory$object)) {
    ok <- validate_object_direction_change(
      trajectory %>% dplyr::filter(.data$object == o),
      change_angle,
      min_interval
    )
    if (!ok) crit1 <- F
  }
  crit2 <- T
  for (tt in unique(trajectory$time)) {
    ok <- is_distance_at_least(
      trajectory %>% dplyr::filter(.data$time == tt),
      min_distance
    )
    if (!ok) crit2 <- F
  }
  if (verbose) {
    message(ifelse(crit1, "PASS", "FAIL"), " Sudden motion changes")
    message(ifelse(crit2, "PASS", "FAIL"), " Minimum interobject distance")
  }
  return(crit1 & crit2)
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
validate_object_direction_change <- function(
  trajectory1,
  change_angle = 30,
  min_interval = 0.2
) {
  # check direction changes in single object
  trajectory1 <- trajectory1 %>% dplyr::arrange(.data$time)
  change_min <- change_angle / 360 * 2 * pi
  # calculate direction
  tr <- trajectory1 %>%
    dplyr::mutate(
      dx = dplyr::lead(.data$x) - .data$x,
      dy = dplyr::lead(.data$y) - .data$y,
      direction = atan2(.data$dy, .data$dx) %% (2 * pi)
    )
  tr <- tr %>%
    dplyr::mutate(
      dirchange = (.data$direction - dplyr::lag(.data$direction)) %% (2 * pi),
      dirchange_large = (.data$dirchange > change_min) &
        (.data$dirchange < 2 * pi - change_min)
    )
  changed <- tr %>%
    dplyr::filter(.data$dirchange_large) %>%
    dplyr::arrange(.data$time)
  all(diff(changed$time) >= min_interval)
}
