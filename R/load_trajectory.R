#' Load trajectory from file
#'
#' @param filename Name of input file (e.g., .csv)
#' @param delim Single character used to separate fields within a record.
#'        Default `","`
#' @param ... Parameters passed to `readr::read_delim`
#'
#' @return Tibble trajectory object with loaded data
#' @export
load_trajectory <- function(filename, delim = ",", ...) {
  input <- readr::read_delim(
    filename,
    delim = delim,
    col_names = FALSE,
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
    time = rep(input |> dplyr::select(1) |> dplyr::pull(), n),
    object = rep(1:n, each = nrow(input)),
    x = xx,
    y = yy
  ) |>
    dplyr::arrange(.data$time)
  trajectory
}
