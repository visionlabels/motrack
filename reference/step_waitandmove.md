# Wait and move step function

Step function for wait-and-move movement pattern

## Usage

``` r
step_waitandmove(
  moment,
  time_next,
  settings,
  move_time = c(0.5, 1.5),
  wait_time = c(0.2, 0.5),
  wait_prob = 0.1,
  syncstart = FALSE
)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`

- time_next:

  Next time step to predict

- settings:

  List with basic properties

- move_time:

  Range of move phase duration

- wait_time:

  Range of wait phase duration

- wait_prob:

  Probability of waiting

- syncstart:

  Logical, if time-to-change should be synchronized when it is generated
  for the first time

## Value

Another moment - position tibble with extra columns `direction`,
`speed`, `speed_scale`, `ttc` and `mov_state`

## Details

Using this step function, objects changes between two states - moving
and waiting. After each state, new state is sampled with given
probability.

When probability of waiting is set to zero, objects move in a same way
as in
[`step_zigzag()`](https://visionlabels.github.io/motrack/reference/step_zigzag.md)
step function

## See also

Other step_functions:
[`step_direct()`](https://visionlabels.github.io/motrack/reference/step_direct.md),
[`step_grid()`](https://visionlabels.github.io/motrack/reference/step_grid.md),
[`step_vonmises()`](https://visionlabels.github.io/motrack/reference/step_vonmises.md),
[`step_vortex()`](https://visionlabels.github.io/motrack/reference/step_vortex.md),
[`step_zigzag()`](https://visionlabels.github.io/motrack/reference/step_zigzag.md)
