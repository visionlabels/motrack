# Save trajectory to file

Save trajectory to file

## Usage

``` r
save_trajectory(trajectory, filename, delim = ",")
```

## Arguments

- trajectory:

  Trajectory object to be exported to file

- filename:

  Name of output file (e.g., .csv)

- delim:

  Single character used to separate fields within a record. Default
  `","`

## Value

Invisibly returns data in wide format (as saved to file)

## Examples

``` r
filename <- "test.csv"
save_trajectory(trajectory8c, filename)
unlink(filename)
```
