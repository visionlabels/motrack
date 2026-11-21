# Sanity check for trajectory

Sometimes, we want have extra requirements for trajectories. The idea is
to have automated check about possible errors or artifacts which could
appear in trajectory data.

## Usage

``` r
validate_trajectory(
  trajectory,
  change_angle = 30,
  min_interval = 0.2,
  min_distance = 1,
  verbose = FALSE
)
```

## Arguments

- trajectory:

  Tibble object with trajectory

- change_angle:

  Angle value in degrees. Direction changes beyond this value are
  considered large.

- min_interval:

  Time in seconds, which should pass between too large direction
  changes.

- min_distance:

  Minimum inter-object spacing, which should be complied in every frame.

- verbose:

  If `TRUE`, results for each subtest are printed on console. Default
  `FALSE`.

## Value

Logical. `TRUE` if all conditions are passed.

## Details

We test for following conditions:

- *Sudden motion changes* - Sometimes objects bounce too frequently,
  which may attract attention. For each object, we test what is the
  minimal time interval between two consecutive large direction changes.

- *Minimum interobject distance* - The minimum distance is checked when
  generating positions and also in each step of generating trajectories.
  Here, we make final check if the minimum distance is complied.

## Examples

``` r
validate_trajectory(trajectory8c)
#> [1] FALSE
```
