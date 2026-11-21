#' Vortex trajectory step function
#'
#' Step function for objects moving on an integer grid. The grid is divided
#' into small 2x2 fields ("vortices") and the objects in a vortex rotate
#' clockwise or counter-clockwise by one grid step per call. The vortex grid
#' is randomly shifted in each step. Objects in vortices that are not fully
#' within the arena limits move only along the allowed axis or stay put.
#'
#' Directions follow the clock analogy used elsewhere in the package:
#' 0 = 3 o'clock, pi/2 = 12 o'clock, pi = 9 o'clock, 3*pi/2 = 6 o'clock.
#'
#' @param moment Position tibble with extra columns `direction` and `speed`,
#'   with integer `x` and `y` coordinates.
#' @param time_next Next time step to predict.
#' @param settings List with basic properties - namely `xlim`, `ylim`
#'   and `speed`.
#'
#' @return Another moment - position tibble with extra columns `pos`
#'   (position within the vortex), `direction` and `speed` corresponding
#'   to `time_next`.
#' @family step_functions
#' @keywords internal
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
    dplyr::left_join(vm, by = dplyr::join_by("vx", "vy")) |>
    dplyr::mutate(direction = NA_real_, speed = global_speed)
  v <- v |> dplyr::select(-"vx", -"vy")
  # print(v)
  # The orientation guide using clock analogy:
  # 0 = 3 o'clock, pi/2 = 12 o'clock, pi = 9 o'clock, 3*pi/2 = 6 o'clock
  number_of_objects_in_vortex <- function(tibble_vortex, code_vortex) {
    tibble_vortex |> dplyr::filter(.data$code == code_vortex) |> nrow()
  }
  dir_E <- 0
  dir_N <- pi / 2
  dir_S <- (3 * pi / 2)
  dir_W <- pi
  for (r in seq_len(nrow(v))) {
    if (v$ok[r]) {
      # we can make circles
      if ((v$pos[r] == "NW") && (v$rot[r] == "CW")) {
        v$direction[r] <- dir_E
      }
      if ((v$pos[r] == "SW") && (v$rot[r] == "CW")) {
        v$direction[r] <- dir_N
      }
      if ((v$pos[r] == "SE") && (v$rot[r] == "CW")) {
        v$direction[r] <- dir_W
      }
      if ((v$pos[r] == "NE") && (v$rot[r] == "CW")) {
        v$direction[r] <- dir_S
      }

      if ((v$pos[r] == "NW") && (v$rot[r] == "CCW")) {
        v$direction[r] <- dir_S
      }
      if ((v$pos[r] == "SW") && (v$rot[r] == "CCW")) {
        v$direction[r] <- dir_E
      }
      if ((v$pos[r] == "SE") && (v$rot[r] == "CCW")) {
        v$direction[r] <- dir_N
      }
      if ((v$pos[r] == "NE") && (v$rot[r] == "CCW")) v$direction[r] <- dir_W
    }
    if (v$ok_horizontal[r] && !v$ok_vertical[r]) {
      # we can only go W and E
      if (number_of_objects_in_vortex(v, v$code[r]) == 1) {
        if (v$pos[r] %in% c("NW", "SW")) {
          v$direction[r] <- dir_E
        }
        if (v$pos[r] %in% c("NE", "SE")) v$direction[r] <- dir_W
      } else {
        v$direction[r] <- dir_E
        v$speed[r] <- 0
      }
    }
    if (!v$ok_horizontal[r] && v$ok_vertical[r]) {
      # we can only go N and S
      if (number_of_objects_in_vortex(v, v$code[r]) == 1) {
        if (v$pos[r] %in% c("NW", "NE")) {
          v$direction[r] <- dir_S
        }
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
    dplyr::mutate(x = round(.data$x), y = round(.data$y))
  moment_next |>
    dplyr::select("x", "y", "object", "time", "pos", "direction", "speed")
}
