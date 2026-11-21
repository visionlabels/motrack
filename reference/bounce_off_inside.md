# Check for bouncing off the inner circle of a donut arena

Counterpart of
[`bounce_off_circle()`](https://visionlabels.github.io/motrack/reference/bounce_off_circle.md)
for the inner border of a donut-shaped arena (`arena_shape = "donut"`).
The function extrapolates the objects to the next time point. If an
object would end up closer to the arena centre than
`settings$arena_inside_radius`, its direction is changed to point
directly away from the centre, with optional random jitter.

## Usage

``` r
bounce_off_inside(moment, time_next, settings)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`

- time_next:

  Next timepoint

- settings:

  List with basic properties. Besides `xlim` and `ylim`, it must contain
  `arena_inside_radius` (radius of the inner border, which is an
  optional setting without a default) and uses `circle_bounce_jitter`,
  the amount of uniform angular jitter (in radians) added to the new
  direction.

## Value

Like `moment`, but with `direction` adjusted for objects which would
cross the inner border in the next time point.

## Details

The arena centre is the midpoint of `xlim` and `ylim`, so the arena must
be circular, i.e., `xlim` and `ylim` must have the same extent.
Otherwise the function stops with an error.

## See also

[`bounce_off_circle()`](https://visionlabels.github.io/motrack/reference/bounce_off_circle.md)
for the outer border,
[`default_settings()`](https://visionlabels.github.io/motrack/reference/default_settings.md)
for setting definitions
