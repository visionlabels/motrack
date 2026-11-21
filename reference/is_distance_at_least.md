# Tests inter-object distances

The function calculated all pairwise distances and checks if the are
larger or equal.

## Usage

``` r
is_distance_at_least(position, min_distance)
```

## Arguments

- position:

  tibble

- min_distance:

  Minimum distance to be present between each pair

## Value

TRUE if all distances are larger than `min_distance`

## Examples

``` r
example_pos <-
  tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
is_distance_at_least(example_pos, 1)
#> [1] TRUE
is_distance_at_least(example_pos, 2)
#> [1] FALSE
```
