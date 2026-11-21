# Simple trajectory step function

Simple trajectory step function

## Usage

``` r
step_direct(moment, time_next, settings)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`

- time_next:

  Next time step to predict

- settings:

  list with basic properties

## Value

Another moment - position tibble with extra columns `direction` and
`speed` corresponding to time_next

## See also

Other step_functions:
[`step_grid()`](https://visionlabels.github.io/motrack/reference/step_grid.md),
[`step_vonmises()`](https://visionlabels.github.io/motrack/reference/step_vonmises.md),
[`step_vortex()`](https://visionlabels.github.io/motrack/reference/step_vortex.md),
[`step_waitandmove()`](https://visionlabels.github.io/motrack/reference/step_waitandmove.md),
[`step_zigzag()`](https://visionlabels.github.io/motrack/reference/step_zigzag.md)
