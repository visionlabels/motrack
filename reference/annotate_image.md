# Draw bitmap image as ggplot2 annotation

Draw bitmap image as ggplot2 annotation

## Usage

``` r
annotate_image(image, xlim = NULL, ylim = NULL, settings = NULL)
```

## Arguments

- image:

  rasterGrob object or filename (png)

- xlim:

  x-coordinates

- ylim:

  y-coordinates

- settings:

  list with named parameters, from which default values for xlim and
  ylim are taken

## Value

ggplot2 layer (as in
[`ggplot2::annotation_custom`](https://ggplot2.tidyverse.org/reference/annotation_custom.html))

## Examples

``` r
image <- system.file("img", "Rlogo.png", package="png")
s <- new_settings()
plot_position(position8c, s,
  background_annotation = annotate_image(image, settings = s))
```
