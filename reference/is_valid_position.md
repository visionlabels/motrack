# Is the object a valid position

The function tests an object (tibble) for several requirements:

## Usage

``` r
is_valid_position(position)
```

## Arguments

- position:

  tibble

## Value

TRUE if conditions above are met, FALSE otherwise

## Details

- columns `object`, `x` and `y` are present.

- there is at least one row

- `object` column contains unique values and no missing values

- `x` and `y` are numeric

- all values in `x` and `y` are finite (no `NA`, `NaN`, `Inf` or `-Inf`)

No restriction about data type of `object` is placed.

## Examples

``` r
example_pos <-
  tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
is_valid_position(example_pos)
#> [1] TRUE
```
