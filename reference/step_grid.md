# Grid trajectory step function

Step function for objects moving on a grid. In each step, every object
picks one of four directions (east, north, west or south) at random and
moves by `speed * (time_next - time)`. The directions of all objects are
resampled until the result is valid, that is:

## Usage

``` r
step_grid(moment, time_next, settings)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`.

- time_next:

  Next time step to predict.

- settings:

  List with basic properties - namely `xlim` and `ylim`.

## Value

Another moment - position tibble with extra columns `direction` and
`speed` corresponding to `time_next`.

## Details

- no two objects are closer than 0.5 to each other, and

- all objects lie strictly inside the arena (`xlim`, `ylim`).

Directions follow the clock analogy used elsewhere in the package: 0 = 3
o'clock, pi/2 = 12 o'clock, pi = 9 o'clock, 3\*pi/2 = 6 o'clock.

Because the whole configuration is resampled, the function can loop for
a long time (or forever) when no valid move exists, e.g., when the grid
is densely occupied.

## See also

Other step_functions:
[`step_direct()`](https://visionlabels.github.io/motrack/reference/step_direct.md),
[`step_vonmises()`](https://visionlabels.github.io/motrack/reference/step_vonmises.md),
[`step_vortex()`](https://visionlabels.github.io/motrack/reference/step_vortex.md),
[`step_waitandmove()`](https://visionlabels.github.io/motrack/reference/step_waitandmove.md),
[`step_zigzag()`](https://visionlabels.github.io/motrack/reference/step_zigzag.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)

# A 4 x 4 arena; grid nodes sit at 0.0, 1.0, 2.0, 3.0
settings <-
  new_settings(xlim = c(0, 3), ylim = c(0, 3), speed = 1, r = 0.25)
# extra space for viewing
settingsx <-
  new_settings(xlim = c(-1, 4), ylim = c(-1, 4), speed = 1, r = 0.25)

# Start with 4 randomly chosen nodes
start <- generate_positions_grid(4, step = 1, offset = 0.5, settings = settings)
plot_position(start)

# 5 steps of length 1: each object moves one node east, north, west or south
trajectory <- make_random_trajectory(
   start,
   timescale = 0:5,
   settings = settings,
   step_function = step_grid
)
plot_trajectory(trajectory, settings = settingsx)
} # }
```
