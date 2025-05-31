#' Generate grid-arranged positions
#'
#' @param n Number of points. Use NULL to return all possible positions.
#' @param step Vector of two numbers representing the separation on x-axis and y-axis
#' @param offset Vector of two numbers representing how the grid should be shifted.
#' Potentially the [0,0] point would be moved to this point (unless it is off limits).
#' @param settings Basic properties - namely `xlim`, `ylim`
#'
#' If \code{step} or \code{offeset} vectors have only one element, they are recycled.
#'
#' @return Tibble with `object`, `x` and `y` columns.
#' @export
#'
#' @example
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
  if (length(step) == 1) step <- c(step, step)
  if (length(offset) == 1) offset <- c(offset, offset)

  # the idea is that [0,0] is shown at offset and all other points
  # are separated by step
  # coordinates as [x, y]
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

step_vortex <- function(moment, time_next, settings) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  global_speed <- settings$speed
  # limits are valid locations (xlim...)
  offset <- c(sample(0:1, 1), sample(0:1, 1))
  # print(offset)
  # offset_code <- sprintf("[%d;%d]", offset[1], offset[2])
  v <- coords_to_vortex_coords(moment, offset, settings)
  vm <- calculate_incomplete_vortex(v, offset, settings)
  v <- v |>
    left_join(vm, by = join_by(vx, vy)) |>
    mutate(direction = NA_real_, speed = global_speed)
  v <- v |> select(-vx, -vy)
  # print(v)
  # The orientation guide using clock analogy:
  # 0 = 3 o'clock, pi/2 = 12 o'clock, pi = 9 o'clock, 3*pi/2 = 6 o'clock
  number_of_objects_in_vortex <- function(tibble_vortex, code_vortex) {
    tibble_vortex |> filter(code == code_vortex) |> nrow()
  }
  dir_E <- 0
  dir_N <- pi / 2
  dir_S <- (3 * pi / 2)
  dir_W <- pi
  for (r in seq_len(nrow(v))) {
    if (v$ok[r]) {
      # we can make circles
      if ((v$pos[r] == "NW") && (v$rot[r] == "CW")) v$direction[r] <- dir_E
      if ((v$pos[r] == "SW") && (v$rot[r] == "CW")) v$direction[r] <- dir_N
      if ((v$pos[r] == "SE") && (v$rot[r] == "CW")) v$direction[r] <- dir_W
      if ((v$pos[r] == "NE") && (v$rot[r] == "CW")) v$direction[r] <- dir_S

      if ((v$pos[r] == "NW") && (v$rot[r] == "CCW")) v$direction[r] <- dir_S
      if ((v$pos[r] == "SW") && (v$rot[r] == "CCW")) v$direction[r] <- dir_E
      if ((v$pos[r] == "SE") && (v$rot[r] == "CCW")) v$direction[r] <- dir_N
      if ((v$pos[r] == "NE") && (v$rot[r] == "CCW")) v$direction[r] <- dir_W
    }
    if (v$ok_horizontal[r] && !v$ok_vertical[r]) {
      # we can only go W and E
      if (number_of_objects_in_vortex(v, v$code[r]) == 1) {
        if (v$pos[r] %in% c("NW", "SW")) v$direction[r] <- dir_E
        if (v$pos[r] %in% c("NE", "SE")) v$direction[r] <- dir_W
      } else {
        v$direction[r] <- dir_E
        v$speed[r] <- 0
      }
    }
    if (!v$ok_horizontal[r] && v$ok_vertical[r]) {
      # we can only go N and S
      if (number_of_objects_in_vortex(v, v$code[r]) == 1) {
        if (v$pos[r] %in% c("NW", "NE")) v$direction[r] <- dir_S
        if (v$pos[r] %in% c("SW", "SE")) v$direction[r] <- dir_N
      } else {
        v$direction[r] <- dir_E
        v$speed[r] <- 0
      }
    }
    if (!v$ok_horizontal[r] && !v$ok_vertical[r]) {
      # we cannot go anywhere
      v$direction[r] <- dir_E
      v$speed[r] <- 0
    }
  }

  moment_next <-
    extrapolate_moment(v, timestep, time_now, time_next) |>
    mutate(x = round(x), y = round(y))
  moment_next |>
    select(x, y, object, time, pos, direction, speed) #|>
  #mutate(offset = offset_code)
}


#' Calculate small vortex positions
#'
#' Vortex is a small 2x2 field, where objects rotate.
#' The function takes coordinates of the points and potential offset,
#' and calculates new columns:
#'
#' vx, vy - coordinates of the vortex
#' pos
#'
#' NOTE: object coordinates (and vortex coordinates vy) assume
#' the y-axis have positive values upwards (like a chart, not like a screen)
#' @export
coords_to_vortex_coords <- function(moment, offset, settings) {
  m <- moment |>
    dplyr::mutate(
      vx = (.data$x + offset[1]) %/% 2,
      vy = (.data$y + offset[2]) %/% 2,
      pos = dplyr::case_when(
        (((.data$x + offset[1]) %% 2) == 0) &
          (((.data$y + offset[2]) %% 2) == 0) ~
          "SW",
        (((.data$x + offset[1]) %% 2) == 1) &
          (((.data$y + offset[2]) %% 2) == 0) ~
          "SE",
        (((.data$x + offset[1]) %% 2) == 0) &
          (((.data$y + offset[2]) %% 2) == 1) ~
          "NW",
        (((.data$x + offset[1]) %% 2) == 1) &
          (((.data$y + offset[2]) %% 2) == 1) ~
          "NE",
        .default = NA_character_
      )
    )
  m
}

calculate_incomplete_vortex <- function(moment, offset, settings) {
  v <- moment |> distinct(vx, vy)
  # is NW within limit? >= xlim[1]?
  v <- v |>
    mutate(
      code = str_glue("[{vx};{vy}]"),
      ok_horizontal = ((v$vx * 2 - offset[1]) >= settings$xlim[1]) &
        ((v$vx * 2 - offset[1] + 1) <= settings$xlim[2]),
      ok_vertical = ((v$vy * 2 - offset[2]) >= settings$ylim[1]) &
        ((v$vy * 2 - offset[2] + 1) <= settings$ylim[2]),
      ok = ok_vertical & ok_horizontal,
      rot = sample(c("CW", "CCW"), n(), replace = TRUE)
    )

  v
}

step_grid <- function(moment, time_next, settings) {
  # moment is position + direction + speed
  # just going in the same direction, possibly bouncing
  time_now <- moment$time[1]
  timestep <- time_next - time_now

  while (TRUE) {
    # moment_next <- moment %>%
    #   dplyr::mutate(
    #     direction = (.data$direction +
    #       sample(c(-1, 1) * pi / 2, n(), replace = TRUE))
    #   )
    moment_next <- moment %>%
      dplyr::mutate(
        direction = (sample(c(-1, 0, 1, 2) * pi / 2, n(), replace = TRUE))
      )
    moment_next2 <-
      extrapolate_moment(moment_next, timestep, time_now, time_next)
    d <- distances(moment_next2, moment_next2) |> filter(distance < .5)
    no_overlap <- (nrow(d) == 0)
    within_limits <-
      all(moment_next2$x > settings$xlim[1]) &&
      all(moment_next2$x < settings$xlim[2]) &&
      all(moment_next2$y > settings$ylim[1]) &&
      all(moment_next2$y < settings$ylim[2])
    if (no_overlap && within_limits) break
  }
  moment_next2
}

distances <- function(p1, p2) {
  dd <- expand_grid(
    p1 |> select(o1 = object, x1 = x, y1 = y),
    p1 |> select(o2 = object, x2 = x, y2 = y)
  ) |>
    filter(o1 != o2) |>
    mutate(
      distance = sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
    )
  dd
}
