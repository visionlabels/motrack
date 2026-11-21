# Default settings for position/trajectory code

Returns a list of values used for position/trajectory generation or
presentation. The values refer to the "physical properties" (e.g.,
definition of the objects or arena) or to the presentation propoerties
(e.g., objects' colour, presence of border). The list is passed as a
parameter to other functions.

## Usage

``` r
default_settings()
```

## Value

list of parameters

## Details

Currently used properties include:

- `xlim`: A vector of form `c(xmin, xmax)`. Represents x-limit of the
  arena square, objects are limited to this area.

- `ylim`: A vector of form `c(ymin, ymax)`. Represents y-limit of the
  arena square, objects are limited to this area.

- `min_distance`: Minimum pairwise distance between centres of objects.

- `r`: Radius of the object. Object is considered to be a circle.

- `arena_border`: Logical. Whether arena border should be drawn.

- `arena_shape`: Character. "square" or "circle"

- `fill_object`: Character. Colour code of default fill colour.

- `fill_target`: Character. Colour code of target fill colour.

- `border_object`: Character. Colour code of default border.

- `border_target`: Character. Colour code of target border.

- `show_labels`: Logical. Should we show object numbers in plots?

- `bounce_off_square`: Logical. Should objects bounce off square arena?

- `bounce_off_others`: Logical. Should objects bounce off each other?

- `simplified_bouncing`: Logical. Should pairwise collisions respect
  contact angle or just swap velocity vectors (simplified bouncing)?

- `bounce_off_circle`: Logical. Should objects bounce off circular
  arena?

- `bounce_off_inside`: Logical. Should objects bounce off inside of
  circular arena (for donut shapes)?

- `circle_bounce_jitter`: Real. Amount of uniform angular jitter after
  bouncing, in radians.

## Examples

``` r
default_settings()
#> $xlim
#> [1] -10  10
#> 
#> $ylim
#> [1] -10  10
#> 
#> $min_distance
#> [1] 1
#> 
#> $r
#> [1] 0.5
#> 
#> $arena_border
#> [1] TRUE
#> 
#> $arena_shape
#> [1] "square"
#> 
#> $fill_object
#> [1] "gray"
#> 
#> $fill_target
#> [1] "green"
#> 
#> $border_object
#> [1] "black"
#> 
#> $border_target
#> [1] "black"
#> 
#> $show_labels
#> [1] FALSE
#> 
#> $bounce_off_square
#> [1] FALSE
#> 
#> $bounce_off_others
#> [1] TRUE
#> 
#> $bounce_off_inside
#> [1] FALSE
#> 
#> $simplified_bouncing
#> [1] FALSE
#> 
#> $bounce_off_circle
#> [1] FALSE
#> 
#> $circle_bounce_jitter
#> [1] 0
#> 
```
