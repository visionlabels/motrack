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
