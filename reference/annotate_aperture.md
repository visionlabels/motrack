# Draw bitmap image as ggplot2 annotation and cover rest with filled colour

Draw bitmap image as ggplot2 annotation and cover rest with filled
colour

## Usage

``` r
annotate_aperture(
  image,
  xlim_in,
  ylim_in,
  colour,
  alpha = 1,
  xlim_out = NULL,
  ylim_out = NULL,
  settings = NULL
)
```

## Arguments

- image:

  rasterGrob object or filename (png)

- xlim_in:

  x-coordinates of the image

- ylim_in:

  y-coordinates of the image

- colour:

  fill colour for the remaining area

- alpha:

  Transparency for remaining area (not for the drawn image)

- xlim_out:

  x-coordinates of the covered area

- ylim_out:

  y-coordinates of the covered area

- settings:

  list with named parameters, default values for xlim_out and ylim_out
  can be taken from xlim and ylim

## Value

ggplot2 layer (as in
[`ggplot2::annotate`](https://ggplot2.tidyverse.org/reference/annotate.html))

## Examples

``` r
image <- system.file("extdata/aperture_white.png", package="motrack")
s <- new_settings()
plot_position(position8c, s,
  foreground_annotation = annotate_aperture(
    image, xlim_in = c(-6, 6), ylim_in = c(-6, 6), colour = "white", settings = s
))
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the motrack package.
#>   Please report the issue to the authors.
```
