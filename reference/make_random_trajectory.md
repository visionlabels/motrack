# Create random trajectory

Create random trajectory

## Usage

``` r
make_random_trajectory(start, timescale, settings, step_function, ...)
```

## Arguments

- start:

  Position tibble with starting configuration

- timescale:

  Numeric vector with time points for the trajectory

- settings:

  list with basic properties

- step_function:

  function which is called in every time step to calculate next position
  in sequence

- ...:

  Extra parameters passed to `step_function`

## Value

Tibble trajectory object

## Examples

``` r
sett_move <-
  new_settings(
    speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
    bounce_off_square = FALSE,
    bounce_off_circle = TRUE, circle_bounce_jitter = pi / 6
  )
moment <- position8c |> add_random_direction()
tt <- make_random_trajectory(
  moment, seq(0, 8, by = 0.1), sett_move, step_direct
)
```
