#' Calculate small vortex positions
#'
#' Vortex is a small 2x2 field, where objects rotate.
#' The function takes coordinates of the points and potential offset,
#' and calculates new columns:
#'
#' - `vx`, `vy`: coordinates of the vortex the object belongs to
#' - `pos`: position of the object within the vortex, one of
#'   `"SW"`, `"SE"`, `"NW"`, `"NE"`
#'
#' NOTE: object coordinates (and vortex coordinates `vy`) assume
#' the y-axis has positive values upwards (like a chart, not like a screen).
#'
#' @param moment Position tibble with integer `x` and `y` columns.
#' @param offset Vector of two numbers (`x` and `y`) by which the vortex grid
#'   is shifted.
#' @param settings List with basic properties. Currently not used.
#'
#' @return `moment` with extra columns `vx`, `vy` and `pos`.
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
