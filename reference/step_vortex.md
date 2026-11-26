# Vortex trajectory step function

Step function for objects moving on an integer grid. The grid is divided
into small 2x2 fields ("vortices") and the objects in a vortex rotate
clockwise or counter-clockwise by one grid step per call. The vortex
grid is randomly shifted in each step. Objects in vortices that are not
fully within the arena limits move only along the allowed axis or stay
put.

## Usage

``` r
step_vortex(moment, time_next, settings)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`, with
  integer `x` and `y` coordinates.

- time_next:

  Next time step to predict.

- settings:

  List with basic properties - namely `xlim`, `ylim` and `speed`.

## Value

Another moment - position tibble with extra columns `pos` (position
within the vortex), `direction` and `speed` corresponding to
`time_next`.

## Details

Directions follow the clock analogy used elsewhere in the package: 0 = 3
o'clock, pi/2 = 12 o'clock, pi = 9 o'clock, 3\*pi/2 = 6 o'clock.

## See also

Other step_functions:
[`step_direct()`](https://visionlabels.github.io/motrack/reference/step_direct.md),
[`step_grid()`](https://visionlabels.github.io/motrack/reference/step_grid.md),
[`step_vonmises()`](https://visionlabels.github.io/motrack/reference/step_vonmises.md),
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

# All 16 grid nodes (n = NULL returns every position)
nodes <- generate_positions_grid(
    NULL,
    step = 1,
    settings = settings
)
plot_position(nodes, settings = settingsx)
trajectory <- make_random_trajectory(
    nodes,
    timescale = 0:5,
    settings = settings,
    step_function = step_vortex
)
plot_trajectory(trajectory, settings = settingsx)
render_trajectory_video("demo.mp4", trajectory, settings = settingsx)
} # }
```
