#' Save trajectory to file
#'
#' @param trajectory Trajectory object to be exported to file
#' @param filename Name of output file (e.g., .csv)
#' @param delim Single character used to separate fields
#' within a record. Default `","`
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
  xpart <- trajectory |>
    dplyr::select("time", "object", "x") |>
    tidyr::spread(key = .data$object, value = .data$x) |>
    dplyr::select("time", dplyr::everything())
  ypart <- trajectory |>
    dplyr::select("time", "object", "y") |>
    tidyr::spread(key = .data$object, value = .data$y) |>
    dplyr::select("time", dplyr::everything())

  n <- ncol(xpart) - 1
  stopifnot(all(names(xpart) == names(ypart)))
  stopifnot(all(xpart$time == ypart$time))
  names(xpart)[-1] <- stringr::str_c("x", names(xpart)[-1])
  names(ypart)[-1] <- stringr::str_c("y", names(ypart)[-1])
  merged <- dplyr::bind_cols(
    xpart,
    ypart |> dplyr::select(-"time")
  )
  column_index <- c(1:n, 0.5 + (1:n))
  merged <- merged |> dplyr::select(c(1, order(column_index) + 1))
  merged |>
    readr::write_delim(filename, delim = delim, col_names = FALSE)
  invisible(merged)
}
