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
