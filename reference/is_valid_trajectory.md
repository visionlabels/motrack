# Is the object a valid trajectory

The function tests an object (tibble) for several requirements:

## Usage

``` r
is_valid_trajectory(trajectory)
```

## Arguments

- trajectory:

  tibble

## Value

TRUE if conditions above are met, FALSE otherwise

## Details

- columns `time`, `object`, `x` and `y` are present.

- `object` column contains unique values

- no value missing in `x` and `y`

- `x` and `y` are numeric

No restriction about data type of `object` is placed.

## Examples

``` r
example_pos <-
  tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
is_valid_position(example_pos)
#> [1] TRUE
```
