
#' Is the object a valid position
#'
#' The function tests an object (tibble) for several requirements:
#' \itemize{
#'   \item columns `object`, `x` and `y` are present.
#'   \item `object` column contains unique values
#'   \item no value missing in `x` and `y`
#'   \item `x` and `y` are numeric
#' }
#'
#' No restriction about data type of `object` is placed.
#'
#' @param position tibble
#'
#' @return TRUE if conditions above are met, FALSE otherwise
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_valid_position(example_pos)
is_valid_position <- function(position) {
  # requirements:
  # 1) each object only once
  # 2) no missing x, y
  tmp <- position %>%
    dplyr::group_by(.data$object) %>%
    dplyr::summarise(n = dplyr::n())
  all(tmp$n == 1) &&
    all(!is.na(position$x)) &&
    all(!is.na(position$y)) &&
    is.numeric(position$x) &&
    is.numeric(position$y)
}

#' Tests inter-object distances
#'
#' The function calculated all pairwise distances and checks
#' if the are larger or equal.
#'
#' @param position tibble
#' @param min_distance Minimum distance to be present between each pair
#'
#' @return TRUE if all distances are larger than `min_distance`
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_distance_at_least(example_pos, 1)
#' is_distance_at_least(example_pos, 2)
is_distance_at_least <- function(position, min_distance) {
  dist_triangle <-
    position %>%
    dplyr::select(.data$x, .data$y) %>%
    as.matrix() %>%
    stats::dist()
  all(dist_triangle >= min_distance)
}

#' Default settings for position/trajectory code
#'
#' Returns a list of values used for position/trajectory generation or
#' presentation. The values refer to the "physical properties"
#' (e.g., definition of the objects or arena) or
#' to the presentation propoerties
#' (e.g., objects' colour, presence of border).
#' The list is passed as a parameter to other functions.
#'
#' Currently used properties include:
#' \describe{
#'   \item{xlim}{A vector of form `c(xmin, xmax)`.
#'   Represents x-limit of the arena square, objects are limited to this area.}
#'   \item{ylim}{A vector of form `c(ymin, ymax)`.
#'   Represents y-limit of the arena square, objects are limited to this area.}
#'   \item{min_distance}{Minimum pairwise distance between centres of objects.}
#'   \item{r}{Radius of the object. Object is considered to be a circle.}
#'   \item{arena_border}{Logical. Whether arena border should be drawn.}
#'   \item{arena_shape}{Character. "square" or "circle"}
#'   \item{fill_object}{Character. Colour code of default fill colour.}
#'   \item{fill_target}{Character. Colour code of target fill colour.}
#'   \item{border_object}{Character. Colour code of default border.}
#'   \item{border_target}{Character. Colour code of target border.}
#'   \item{show_labels}{Logical. Should we show object numbers in plots?}
#'   \item{bounce_off_square}{Logical. Should objects bounce off square arena?}
#'   \item{bounce_off_others}{Logical. Should objects bounce off each other?}
#'   \item{simplified_bouncing}{Logical.
#'     Should pairwise collisions respect contact angle or
#'     just swap velocity vectors (simplified bouncing)? }
#'   \item{bounce_off_circle}{Logical. Should objects bounce off circular arena?}
#'   \item{bounce_off_inside}{Logical. Should objects bounce off inside of circular arena (for donut shapes)?}
#'   \item{circle_bounce_jitter}{Real. Amount of uniform angular jitter after bouncing, in radians.}
#' }
#'
#' @return list of parameters
#' @export
#'
#' @examples
#' default_settings()
default_settings <- function() {
  list(
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    min_distance = 1,
    r = 0.5,
    arena_border = T,
    arena_shape = "square",
    fill_object = "gray",
    fill_target = "green",
    border_object = "black",
    border_target = "black",
    show_labels = F,
    bounce_off_square = F,
    bounce_off_others = T,
    bounce_off_inside = F,
    simplified_bouncing = F,
    bounce_off_circle = F,
    circle_bounce_jitter = 0
  )
}


#' Creates a custom copy of settings
#'
#' @param .from optional parent from which defaults are inherited
#' @param ... named list of properties to override default settings.
#'
#' @return Updated list of settings
#' @export
#'
#' @seealso \code{\link{default_settings}}
#'
#' @examples
#' s1 <- new_settings(xlim = c(0, 10), ylim = c(0, 10))
#' s2 <- new_settings(.from = s1, show_labels = TRUE)
new_settings <- function(.from = NULL, ...) {
  # what names are valid?
  default_settings_list <- default_settings()
  optional_parameters <- c("speed", "arena_inside_radius")
  valid_parameter_names <- union(names(default_settings_list), optional_parameters)
  # is parent provided?
  if (is.null(.from)) {
    parent <- default_settings()
  } else {
    parent <- .from
    stopifnot(all(names(parent) %in% valid_parameter_names))
  }
  dots <- list(...)
  for (key in names(dots)) {
    if (!key %in% valid_parameter_names) {
      stop("Unknown parameter: ", key)
    }
    parent[[key]] <- dots[[key]]
  }
  parent
}

#' Random object positions
#'
#' The function expects a square arena defined by `xlim` and `ylim` settings.
#' The objects are placed randomly (uniform distribution sampling) into the arena.
#' If `check_distance` is `TRUE`, the process is repeated
#' until the minimum pairwise distance is met.
#' `border_distance` specifies, whether objects should keep some initial distance
#' from arena borders. This distance is not meant to be the bouncing distance,
#' it helps to put objects more together in the beginning.
#'
#' @param n Number of objects
#' @param settings Basic properties - namely `xlim`, `ylim`,
#' possibly `min_distance`
#' @param check_distance Logical.
#' The positions are generated until
#' minimum pairwise distance of `min_distance` is met.
#' @param border_distance Distance from arena borders.
#'
#' @return Tibble with `object`, `x` and `y` columns.
#' @export
#'
#' @seealso
#' \code{\link{default_settings}} for setting definitions,
#' \code{\link{new_settings}} for adjusting default settings,
#' \code{\link{is_distance_at_least}} for checking distances
#'
#' @examples
#' # sample positions with no other requirements
#' pos <- generate_positions_random(8, default_settings())
#' # when starting positions should be further from borders
#' pos <- generate_positions_random(8, default_settings(), border_distance = 3)
generate_positions_random <- function(
                                      n, settings, check_distance = T, border_distance = 0) {
  xlim <- settings$xlim
  ylim <- settings$ylim
  stopifnot(!is.null(xlim))
  stopifnot(!is.null(ylim))
  shapes <- c("square", "circle", "donut")
  shape <- pmatch(settings$arena_shape, shapes)
  if (is.na(shape)) {
    stop("Arena shape unknown")
  }
  while (T) {
    p <- switch(
      shapes[shape],
      square = random_coords_in_square(
        n,
        xlim[1] + border_distance, xlim[2] - border_distance,
        ylim[1] + border_distance, ylim[2] - border_distance
      ),
      circle = random_coords_in_circle(
        n,
        mean(xlim), mean(ylim),
        sum(c(diff(xlim), diff(ylim)) / 4) - border_distance
      ),
      donut = random_coords_in_donut(
        n,
        mean(xlim), mean(ylim),
        sum(c(diff(xlim), diff(ylim)) / 4) - border_distance,
        settings$arena_inside_radius + border_distance
      )
    )
    p <- p %>%
      dplyr::mutate(object = 1:n) %>%
      dplyr::select(.data$object, dplyr::everything())
    if (check_distance) {
      if (is_distance_at_least(p, min_distance = settings$min_distance)) {
        return(p)
      }
    } else {
      return(p)
    }
  }
}

random_coords_in_square <- function(n, xmin, xmax, ymin, ymax) {
  tibble::tibble(
    x = stats::runif(n, xmin, xmax),
    y = stats::runif(n, ymin, ymax)
  )
}

random_coords_in_circle <- function(n, xmid, ymid, radius) {
  stopifnot(radius > 0)
  res <- tibble::tibble(x = rep(NA_real_, n), y = rep(NA_real_, n))
  i <- 1
  while (i <= n) {
    xx <- stats::runif(1, -radius, radius)
    yy <- stats::runif(1, -radius, radius)
    d <- sqrt((xx^2) + (yy^2))
    if (d < radius) {
      res$x[i] <- xmid + xx
      res$y[i] <- ymid + yy
      i <- i + 1
    }
  }
  res
}

random_coords_in_donut <- function(n, xmid, ymid, radius_out, radius_in) {
  stopifnot(radius_out > 0)
  stopifnot(radius_in > 0)
  stopifnot(radius_out > radius_in)
  res <- tibble::tibble(x = rep(NA_real_, n), y = rep(NA_real_, n))
  i <- 1
  while (i <= n) {
    xx <- stats::runif(1, -radius_out, radius_out)
    yy <- stats::runif(1, -radius_in, radius_in)
    d <- sqrt((xx^2) + (yy^2))
    if (d < radius_out && d > radius_in) {
      res$x[i] <- xmid + xx
      res$y[i] <- ymid + yy
      i <- i + 1
    }
  }
  res
}

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
plot_position <- function(position,
                          settings = default_settings(),
                          targets = NULL,
                          background_annotation = NULL,
                          foreground_annotation = NULL) {
  # check extra parameters
  if (!is.null(targets)) {
    # passed in call
    position$target <- F
    position$target[targets] <- T
  } else {
    if (!"target" %in% names(position)) {
      position$target <- F
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
        x0 = "x", y0 = "y",
        fill = "I(fill)", colour = "I(border)"
      )
    ) +
    background_annotation +
    ggforce::geom_circle(ggplot2::aes_string(r = "settings$r")) +
    foreground_annotation +
    ggplot2::theme(panel.background = ggplot2::element_blank()) +
    ggplot2::coord_fixed(xlim = settings$xlim, ylim = settings$ylim, expand = F) +
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
        ggplot2::annotate("segment",
          x = settings$xlim[1], y = settings$ylim[1],
          xend = settings$xlim[1], yend = settings$ylim[2]
        ) +
        ggplot2::annotate("segment",
          x = settings$xlim[1], y = settings$ylim[1],
          xend = settings$xlim[2], yend = settings$ylim[1]
        ) +
        ggplot2::annotate("segment",
          x = settings$xlim[1], y = settings$ylim[2],
          xend = settings$xlim[2], yend = settings$ylim[2]
        ) +
        ggplot2::annotate("segment",
          x = settings$xlim[2], y = settings$ylim[1],
          xend = settings$xlim[2], yend = settings$ylim[2]
        )
    }
    if (settings$arena_shape == "circle") {
      xm <- mean(settings$xlim)
      ym <- mean(settings$ylim)
      rr <- sum(c(diff(settings$xlim), diff(settings$ylim)) / 4)
      fig <- fig +
        ggforce::geom_circle(
          ggplot2::aes_string(
            x0 = "xx", y0 = "yy", r = "rr", fill = "NULL", border = "NULL", colour = "I(\"black\")"),
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
            x0 = "xx", y0 = "yy", r = "rr", fill = "NULL", border = "NULL", colour = "I(\"black\")"),
          data = tibble::tibble(
            xx = c(xm, xm), yy = c(ym, ym), rr = c(rr, settings$arena_inside_radius)
          )
        )
    }
  }
  fig
}

#' Draw bitmap image as ggplot2 annotation
#'
#' @param image rasterGrob object or filename (png)
#' @param xlim x-coordinates
#' @param ylim y-coordinates
#' @param settings list with named parameters, from which default values for xlim and ylim are taken
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
    image <- grid::rasterGrob(pic, interpolate=FALSE)
  }
  ggplot2::annotation_custom(image, xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2])
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
#' @param settings list with named parameters, default values for xlim_out and ylim_out
#' can be taken from xlim and ylim
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
annotate_aperture <- function(image,
                              xlim_in, ylim_in,
                              colour, alpha = 1,
                              xlim_out = NULL, ylim_out = NULL,
                              settings = NULL) {
  if (is.null(xlim_out)) {
    xlim_out <- settings$xlim
  }
  if (is.null(ylim_out)) {
    ylim_out <- settings$ylim
  }
  if (is.character(image)) {
    pic <- png::readPNG(image)
    image <- grid::rasterGrob(pic, interpolate=FALSE)
  }
  list(
    ggplot2::annotation_custom(
      image, xmin = xlim_in[1], ymin = ylim_in[1], xmax = xlim_in[2], ymax = ylim_in[2]
    ),
    # bottom, top, left, right
    ggplot2::annotate(
      "rect",
      xmin = c(xlim_out[1], xlim_out[1], xlim_out[1], xlim_in[2]),
      ymin = c(ylim_out[1], ylim_in[2],  ylim_in[1],  ylim_in[1]),
      xmax = c(xlim_out[2], xlim_out[2], xlim_in[1],  xlim_out[2]),
      ymax = c(ylim_in[1],  ylim_out[2], ylim_in[2],  ylim_in[2]),
      fill = rep(colour, 4),
      colour = rep(NA, 4),
      alpha = rep(alpha, 4)
    )
  )
}

