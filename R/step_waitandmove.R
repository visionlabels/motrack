#' Wait and move step function
#'
#' Step function for wait-and-move movement pattern
#'
#' Using this step function, objects changes between
#' two states - moving and waiting.
#' After each state, new state is sampled with given probability.
#'
#' When probability of waiting is set to zero,
#' objects move in a same way as in  [step_zigzag()] step function
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
  syncstart = FALSE
) {
  time_now <- moment$time[1]
  timestep <- time_next - time_now
  # move_time is time of movement
  # wait_time is time os waiting
  # after this time,
  # object change its statewhen object change their state
  # (either move of wait)
  n <- nrow(moment)

  if (!"mov_state" %in% names(moment)) {
    moment <- moment |>
      dplyr::mutate(
        mov_state = sample(
          c("w", "m"),
          n,
          replace = TRUE,
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

    moment <- moment |> dplyr::mutate(ttc = times)
  }
  # all objects have defined time to change

  if (!"speed_scale" %in% names(moment)) {
    speed_scales <- vector(mode = "numeric", length = n)
    speed_scales[moment$mov_state == "m"] <- 1
    speed_scales[moment$mov_state == "w"] <- 0
    moment <- moment |> dplyr::mutate(speed_scale = speed_scales)
  }

  # which objects should change their state
  which_change <- moment$ttc < time_next

  if (any(which_change)) {
    # we have some objects that need to change their behavior
    # change state of objects
    moment$mov_state[which_change] <- sample(
      c("w", "m"),
      sum(which_change),
      replace = TRUE,
      prob = c(wait_prob, 1 - wait_prob)
    )

    # do computations separately for moving and waiting objects
    which_wait <- which_change & moment$mov_state == "w"
    which_move <- which_change & moment$mov_state == "m"

    # set speed scaling factor
    # this is basically a trick,
    # how to stop object from moving,
    # we are not modifying the speed,
    # as it could be used by other functions
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
  # compute new positions, scaling factors sets,
  # whether objects are moving or not
  moment_next <-
    extrapolate_moment(moment, timestep, time_now, time_next)
  moment_next
}
