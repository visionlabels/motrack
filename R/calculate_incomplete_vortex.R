#' Find vortices that do not fit within the arena limits
#'
#' Helper for [step_vortex()]. Takes the objects with vortex coordinates
#' (see [coords_to_vortex_coords()]) and, for each distinct vortex, checks
#' whether the whole 2x2 field lies within `xlim` and `ylim`. It also draws
#' a random rotation direction for each vortex.
#'
#' A vortex is "incomplete" if it sticks out of the arena horizontally
#' and/or vertically. Objects in such vortices cannot rotate and are
#' restricted to the allowed axis (or stay still).
#'
#' @param moment Position tibble with vortex columns `vx` and `vy`,
#'   as returned by [coords_to_vortex_coords()].
#' @param offset Vector of two numbers (`x` and `y`) by which the vortex grid
#'   is shifted. Must be the same offset used to calculate `vx` and `vy`.
#' @param settings List with basic properties - namely `xlim` and `ylim`.
#'
#' @return Tibble with one row per distinct vortex and columns:
#'   - `vx`, `vy`: vortex coordinates
#'   - `code`: vortex label of the form `"[vx;vy]"`
#'   - `ok_horizontal`: `TRUE` if the vortex fits within `xlim`
#'   - `ok_vertical`: `TRUE` if the vortex fits within `ylim`
#'   - `ok`: `TRUE` if the vortex fits in both directions
#'   - `rot`: random rotation direction, `"CW"` or `"CCW"`
#' @noRd
calculate_incomplete_vortex <- function(moment, offset, settings) {
  v <- moment |> dplyr::distinct(.data$vx, .data$vy)
  # is NW within limit? >= xlim[1]?
  v <- v |>
    dplyr::mutate(
      code = stringr::str_glue("[{vx};{vy}]"),
      ok_horizontal = ((v$vx * 2 - offset[1]) >= settings$xlim[1]) &
        ((v$vx * 2 - offset[1] + 1) <= settings$xlim[2]),
      ok_vertical = ((v$vy * 2 - offset[2]) >= settings$ylim[1]) &
        ((v$vy * 2 - offset[2] + 1) <= settings$ylim[2]),
      ok = .data$ok_vertical & .data$ok_horizontal,
      rot = sample(c("CW", "CCW"), dplyr::n(), replace = TRUE)
    )
  v
}
