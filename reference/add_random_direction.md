# Add random direction

Simple fuction to assign random direction to each object in position
object. Random means uniformly distributed value from 0 to 2 \* pi (0 to
360 deg). The orientation guide using clock analogy: 0 = 3 o'clock, pi/2
= 12 o'clock, pi = 9 o'clock, 3\*pi/2 = 6 o'clock

## Usage

``` r
add_random_direction(position)
```

## Arguments

- position:

  tibble

## Value

same tibble with extra direction column

## Examples

``` r
position8c %>% add_random_direction()
#> # A tibble: 8 × 4
#>   object      x      y direction
#>    <int>  <dbl>  <dbl>     <dbl>
#> 1      1 -1.21  -1.84     0.507 
#> 2      2 -3.74  -0.477    5.24  
#> 3      3 -3.28   5.01     3.77  
#> 4      4 -6.36  -0.809    0.988 
#> 5      5  4.18  -5.29     0.0465
#> 6      6  0.853 -4.11     2.93  
#> 7      7 -5.21   3.55     3.13  
#> 8      8  5.53  -1.76     1.82  
```
