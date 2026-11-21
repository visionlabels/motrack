# Generate grid-arranged positions

Places objects on the nodes of a regular grid within the arena limits.
If `step` or `offset` vectors have only one element, they are recycled.

## Usage

``` r
generate_positions_grid(n, step = 1, offset = 0, settings)
```

## Arguments

- n:

  Number of points. Use `NULL` to return all possible positions.

- step:

  Vector of two numbers representing the separation on x-axis and
  y-axis.

- offset:

  Vector of two numbers representing how the grid should be shifted.
  Potentially the point `[0, 0]` would be moved to this point (unless it
  is off limits).

- settings:

  Basic properties - namely `xlim`, `ylim`.

## Value

Tibble with `object`, `x` and `y` columns.

## Examples

``` r
if (FALSE) { # \dontrun{
p1 <- generate_positions_grid(10, settings = default_settings())
plot_position(p1)
p1 <- generate_positions_grid(10, c(1, 2), c(0.1, .5), default_settings())
plot_position(p1)
} # }
```
