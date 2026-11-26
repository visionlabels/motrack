# Pairwise distances between objects

Calculates Euclidean distances between every object in `p1` and every
object in `p2`. Pairs with the same `object` identifier are dropped, so
when `p1` and `p2` are the same tibble, an object is never paired with
itself and each pair of objects appears twice (A-B and B-A). Used, for
example, by
[`step_grid()`](https://visionlabels.github.io/motrack/reference/step_grid.md)
to detect overlapping objects.

## Usage

``` r
distances(p1, p2)
```

## Arguments

- p1:

  Position tibble with `object`, `x` and `y` columns.

- p2:

  Position tibble with `object`, `x` and `y` columns.

## Value

Tibble with columns `o1`, `x1`, `y1` (object from `p1`), `o2`, `x2`,
`y2` (object from `p2`) and `distance`.
