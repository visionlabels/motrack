#' Draw bitmap image as ggplot2 annotation
#'
#' @param image rasterGrob object or filename (png)
#' @param xlim x-coordinates
#' @param ylim y-coordinates
#' @param settings list with named parameters,
#'        from which default values for xlim and ylim are taken
#'
#' @return ggplot2 layer (as in `ggplot2::annotation_custom`)
#' @export
#'
#' @examples
#' image <- system.file("img", "Rlogo.png", package="png")
#' s <- new_settings()
#' plot_position(position8c, s,
#'   background_annotation = annotate_image(image, settings = s))
annotate_image <- function(image, xlim = NULL, ylim = NULL, settings = NULL) {
  if (is.null(xlim)) {
    xlim <- settings$xlim
  }
  if (is.null(ylim)) {
    ylim <- settings$ylim
  }
  if (is.character(image)) {
    pic <- png::readPNG(image)
    image <- grid::rasterGrob(pic, interpolate = FALSE)
  }
  ggplot2::annotation_custom(
    image,
    xmin = xlim[1],
    ymin = ylim[1],
    xmax = xlim[2],
    ymax = ylim[2]
  )
}

#' Draw bitmap image as ggplot2 annotation and cover rest with filled colour
#'
#' @param image rasterGrob object or filename (png)
#' @param xlim_in x-coordinates of the image
#' @param ylim_in y-coordinates of the image
#' @param colour fill colour for the remaining area
#' @param alpha Transparency for remaining area (not for the drawn image)
#' @param xlim_out x-coordinates of the covered area
#' @param ylim_out y-coordinates of the covered area
#' @param settings list with named parameters,
#'        default values for xlim_out and ylim_out
#'        can be taken from xlim and ylim
#'
#' @return ggplot2 layer (as in `ggplot2::annotate`)
#' @export
#'
#' @examples
#' image <- system.file("extdata/aperture_white.png", package="motrack")
#' s <- new_settings()
#' plot_position(position8c, s,
#'   foreground_annotation = annotate_aperture(
#'     image, xlim_in = c(-6, 6), ylim_in = c(-6, 6), colour = "white", settings = s
#' ))
annotate_aperture <- function(
  image,
  xlim_in,
  ylim_in,
  colour,
  alpha = 1,
  xlim_out = NULL,
  ylim_out = NULL,
  settings = NULL
) {
  if (is.null(xlim_out)) {
    xlim_out <- settings$xlim
  }
  if (is.null(ylim_out)) {
    ylim_out <- settings$ylim
  }
  if (is.character(image)) {
    pic <- png::readPNG(image)
    image <- grid::rasterGrob(pic, interpolate = FALSE)
  }
  list(
    ggplot2::annotation_custom(
      image,
      xmin = xlim_in[1],
      ymin = ylim_in[1],
      xmax = xlim_in[2],
      ymax = ylim_in[2]
    ),
    # bottom, top, left, right
    ggplot2::annotate(
      "rect",
      xmin = c(xlim_out[1], xlim_out[1], xlim_out[1], xlim_in[2]),
      ymin = c(ylim_out[1], ylim_in[2], ylim_in[1], ylim_in[1]),
      xmax = c(xlim_out[2], xlim_out[2], xlim_in[1], xlim_out[2]),
      ymax = c(ylim_in[1], ylim_out[2], ylim_in[2], ylim_in[2]),
      fill = rep(colour, 4),
      colour = rep(NA, 4),
      alpha = rep(alpha, 4)
    )
  )
}
