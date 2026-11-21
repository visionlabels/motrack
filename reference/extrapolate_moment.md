# Extrapolates object positions based on current state and time

Intended for the use in custom step functions.

## Usage

``` r
extrapolate_moment(moment, timestep, cur_time, new_time = NULL)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`

- timestep:

  How far into future should the objects move (linearly)

- cur_time:

  Current time, for time-sensitive speed functions (e.g., accelerations)

- new_time:

  New time value for the returned object. If omitted, it is calculated
  automatically as `cur_time + timestep`

## Value

Another moment - position tibble with extra columns `direction` and
`speed` corresponding to time_next

## Examples

``` r
moment <- position8c %>% add_random_direction() %>%
  dplyr::mutate(speed = 3)
extrapolate_moment(moment, 0.1, 0)
#> # A tibble: 8 × 6
#>   object      x      y direction speed  time
#>    <int>  <dbl>  <dbl>     <dbl> <dbl> <dbl>
#> 1      1 -1.40  -2.07      4.03      3   0.1
#> 2      2 -3.65  -0.190     1.26      3   0.1
#> 3      3 -3.29   4.71      4.67      3   0.1
#> 4      4 -6.19  -1.05      5.33      3   0.1
#> 5      5  4.37  -5.53      5.36      3   0.1
#> 6      6  0.952 -4.39      5.05      3   0.1
#> 7      7 -4.92   3.59      0.150     3   0.1
#> 8      8  5.62  -2.05      5.00      3   0.1
```
