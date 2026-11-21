# Von Mises trajectory step function

Von Mises trajectory step function

## Usage

``` r
step_vonmises(moment, time_next, settings, kappa)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`

- time_next:

  Next time step to predict

- settings:

  list with basic properties

- kappa:

  concentration parameter

## Value

Another moment - position tibble with extra columns `direction` and
`speed` corresponding to time_next

## See also

Other step_functions:
[`step_direct()`](https://visionlabels.github.io/motrack/reference/step_direct.md),
[`step_grid()`](https://visionlabels.github.io/motrack/reference/step_grid.md),
[`step_vortex()`](https://visionlabels.github.io/motrack/reference/step_vortex.md),
[`step_waitandmove()`](https://visionlabels.github.io/motrack/reference/step_waitandmove.md),
[`step_zigzag()`](https://visionlabels.github.io/motrack/reference/step_zigzag.md)
