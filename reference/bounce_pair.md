# Calculate collision vectors for two objects

Calculate collision vectors for two objects

## Usage

``` r
bounce_pair(v1, v2, x1, x2)
```

## Arguments

- v1:

  Velocity vector of object 1. Two-dimensional numeric vector.

- v2:

  Velocity vector of object 2. Two-dimensional numeric vector.

- x1:

  Center of object 1 in the time collision. Two-dimensional numeric
  vector.

- x2:

  Center of object 1 in the time collision. Two-dimensional numeric
  vector.

## Value

list of the two two-dimension vectors - new speed for object 1 and 2

## Examples

``` r
# direct collision, speeds swap
bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(0, 1))
#> [[1]]
#> [1]  0 -1
#> 
#> [[2]]
#> [1] 0 1
#> 

# 45deg contact angle, makes 90deg turns
bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(1, 1))
#> [[1]]
#> [1] -1  0
#> 
#> [[2]]
#> [1] 1 0
#> 
bounce_pair(c(0, 1), c(0, -1), c(0, 1), c(1, 0))
#> [[1]]
#> [1] 1 0
#> 
#> [[2]]
#> [1] -1  0
#> 

# distance is not relevant, direct or 45 deg contact
bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(0, 2))
#> [[1]]
#> [1]  0 -1
#> 
#> [[2]]
#> [1] 0 1
#> 
bounce_pair(c(0, 1), c(0, -1), c(0, 0), c(2, 2))
#> [[1]]
#> [1] -1  0
#> 
#> [[2]]
#> [1] 1 0
#> 

# Works also for different velocities
bounce_pair(c(0, 1), c(0, -2), c(0, 0), c(0, 1))
#> [[1]]
#> [1]  0 -2
#> 
#> [[2]]
#> [1] 0 1
#> 
bounce_pair(c(0, 1), c(0, -2), c(0, 0), c(1, 1))
#> [[1]]
#> [1] -1.5 -0.5
#> 
#> [[2]]
#> [1]  1.5 -0.5
#> 
```
