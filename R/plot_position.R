#' Plot object positions
#'
#' The function plots position tibble based on x, y values and potentially
#' extra values from settings. Position is expected to be position tibble,
#' but it may contain extra columns for graphics:
#' target (Logical vector indicating whether object is target or distractor),
#' fill (Character vector with colour names of object interiors.),
#' border (Character vector with colour names of object borders.).
#'
#' @param position tibble
#' @param settings list with basic properties
#' @param targets Which objects should be treated as targets
#' @param background_annotation Annotation rendered behind the objects (texture)
#' @param foreground_annotation Annotation rendered over the objects (aperture)
#'
#' @return ggplot2 figure
#' @export
#'
#' @examples
#' # sample positions with no other requirements
#' set.seed(100)
#' pos <- generate_positions_random(8, default_settings())
#' plot_position(pos, default_settings())
#' # first four objects are targets
#' plot_position(pos, default_settings(), 1:4)
#' pos$fill <- rainbow(8)
#' plot_position(pos, default_settings())
plot_position <- function(
  position,
  settings = default_settings(),
  targets = NULL,
  background_annotation = NULL,
  foreground_annotation = NULL
) {
  # check extra parameters
  if (!is.null(targets)) {
    # passed in call
    position$target <- FALSE
    position$target[targets] <- TRUE
  } else {
    if (!"target" %in% names(position)) {
      position$target <- FALSE
      # otherwise we have it in dataset
    }
  }
  if (!"fill" %in% names(position)) {
    position$fill <- settings$fill_object
    position$fill[position$target] <- settings$fill_target
  }
  if (!"border" %in% names(position)) {
    position$border <- settings$border_object
    position$border[position$target] <- settings$border_target
  }
  fig <-
    ggplot2::ggplot(
      position,
      ggplot2::aes_string(
        x0 = "x",
        y0 = "y",
        fill = "I(fill)",
        colour = "I(border)"
      )
    ) +
    background_annotation +
    ggforce::geom_circle(ggplot2::aes_string(r = "settings$r")) +
    foreground_annotation +
    ggplot2::theme(panel.background = ggplot2::element_blank()) +
    ggplot2::coord_fixed(
      xlim = settings$xlim,
      ylim = settings$ylim,
      expand = FALSE
    ) +
    NULL
  if (settings$show_labels) {
    fig <-
      fig +
      ggplot2::geom_text(
        ggplot2::aes_string(x = "x", y = "y", label = "object"),
        colour = I("red")
      )
  }
  if (settings$arena_border) {
    if (settings$arena_shape == "square") {
      fig <- fig +
        ggplot2::annotate(
          "segment",
          x = settings$xlim[1],
          y = settings$ylim[1],
          xend = settings$xlim[1],
          yend = settings$ylim[2]
        ) +
        ggplot2::annotate(
          "segment",
          x = settings$xlim[1],
          y = settings$ylim[1],
          xend = settings$xlim[2],
          yend = settings$ylim[1]
        ) +
        ggplot2::annotate(
          "segment",
          x = settings$xlim[1],
          y = settings$ylim[2],
          xend = settings$xlim[2],
          yend = settings$ylim[2]
        ) +
        ggplot2::annotate(
          "segment",
          x = settings$xlim[2],
          y = settings$ylim[1],
          xend = settings$xlim[2],
          yend = settings$ylim[2]
        )
    }
    if (settings$arena_shape == "circle") {
      xm <- mean(settings$xlim)
      ym <- mean(settings$ylim)
      rr <- sum(c(diff(settings$xlim), diff(settings$ylim)) / 4)
      fig <- fig +
        ggforce::geom_circle(
          ggplot2::aes_string(
            x0 = "xx",
            y0 = "yy",
            r = "rr",
            fill = "NULL",
            border = "NULL",
            colour = "I(\"black\")"
          ),
          data = tibble::tibble(xx = xm, yy = ym, rr = rr)
        )
    }
    if (settings$arena_shape == "donut") {
      xm <- mean(settings$xlim)
      ym <- mean(settings$ylim)
      rr <- sum(c(diff(settings$xlim), diff(settings$ylim)) / 4)
      fig <- fig +
        ggforce::geom_circle(
          ggplot2::aes_string(
            x0 = "xx",
            y0 = "yy",
            r = "rr",
            fill = "NULL",
            border = "NULL",
            colour = "I(\"black\")"
          ),
          data = tibble::tibble(
            xx = c(xm, xm),
            yy = c(ym, ym),
            rr = c(rr, settings$arena_inside_radius)
          )
        )
    }
  }
  fig
}
