# Load trajectory from file

Load trajectory from file

## Usage

``` r
load_trajectory(filename, delim = ",", ...)
```

## Arguments

- filename:

  Name of input file (e.g., .csv)

- delim:

  Single character used to separate fields within a record. Default
  `","`

- ...:

  Parameters passed to
  [`readr::read_delim`](https://readr.tidyverse.org/reference/read_delim.html)

## Value

Tibble trajectory object with loaded data
