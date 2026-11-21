# Check for inter-object bouncing and recalculate directions

Check for inter-object bouncing and recalculate directions

## Usage

``` r
bounce_off_others(moment, time_next, settings)
```

## Arguments

- moment:

  Position tibble with extra columns `direction` and `speed`

- time_next:

  Next timepoint

- settings:

  list with basic properties

## Value

Like `moment`, but some directions are adjusted (reversed, swapped), if
there were a collision in next time point (`timestep`)
