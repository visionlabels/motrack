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
    trajectory |> dplyr::filter(.data$time == start_time),
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
      data = trajectory |> dplyr::arrange(.data$time),
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
