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
  moment <- moment |> dplyr::arrange(.data$object)

  moment_next <-
    extrapolate_moment(moment, timestep, time_now, NA)

  # check distances
  dist_next <- moment_next |>
    dplyr::select("x", "y") |>
    as.matrix() |>
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
