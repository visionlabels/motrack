# Random object positions

The function expects a square arena defined by `xlim` and `ylim`
settings. The objects are placed randomly (uniform distribution
sampling) into the arena. If `check_distance` is `TRUE`, the process is
repeated until the minimum pairwise distance is met. `border_distance`
specifies, whether objects should keep some initial distance from arena
borders. This distance is not meant to be the bouncing distance, it
helps to put objects more together in the beginning.

## Usage

``` r
generate_positions_random(
  n,
  settings,
  check_distance = TRUE,
  border_distance = 0
)
```

## Arguments

- n:

  Number of objects

- settings:

  Basic properties - namely `xlim`, `ylim`, possibly `min_distance`

- check_distance:

  Logical. The positions are generated until minimum pairwise distance
  of `min_distance` is met.

- border_distance:

  Distance from arena borders.

## Value

Tibble with `object`, `x` and `y` columns.

## See also

[`default_settings()`](https://visionlabels.github.io/motrack/reference/default_settings.md)
for setting definitions,
[`new_settings()`](https://visionlabels.github.io/motrack/reference/new_settings.md)
for adjusting default settings,
[`is_distance_at_least()`](https://visionlabels.github.io/motrack/reference/is_distance_at_least.md)
for checking distances

## Examples

``` r
# sample positions with no other requirements
pos <- generate_positions_random(8, default_settings())
# when starting positions should be further from borders
pos <- generate_positions_random(8, default_settings(), border_distance = 3)
```
