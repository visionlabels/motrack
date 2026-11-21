# Check direction changes for single object trajectory

Check direction changes for single object trajectory

## Usage

``` r
validate_object_direction_change(
  trajectory1,
  change_angle = 30,
  min_interval = 0.2
)
```

## Arguments

- trajectory1:

  Trajectory featuring only one object

- change_angle:

  Angle value in degrees. Direction changes beyond this value are
  considered large.

- min_interval:

  Time in seconds, which should pass between too large direction
  changes.

## Value

Logical. `TRUE` if each pair of consecutive bounces is separated by at
least `min_interval` seconds.
