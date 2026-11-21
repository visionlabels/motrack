# Zigzag trajectory step function

Zigzag trajectory step function

## Usage

``` r
step_zigzag(moment, time_next, settings, ttt = c(0.5, 1.5), syncstart = F)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`

- time_next:

  Next time step to predict

- settings:

  list with basic properties

- ttt:

  Time-to-turn - time when the object should pick new direction

- syncstart:

  Logical, if time-to-turn should be synchronized when it is generated
  for the first time

## Value

Another moment - position tibble with extra columns `direction`, `speed`
and `ttt` corresponding to time_next

## See also

Other step_functions:
[`step_direct()`](https://visionlabels.github.io/motrack/reference/step_direct.md),
[`step_grid()`](https://visionlabels.github.io/motrack/reference/step_grid.md),
[`step_vonmises()`](https://visionlabels.github.io/motrack/reference/step_vonmises.md),
[`step_vortex()`](https://visionlabels.github.io/motrack/reference/step_vortex.md),
[`step_waitandmove()`](https://visionlabels.github.io/motrack/reference/step_waitandmove.md)
