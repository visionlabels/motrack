# Calculate small vortex positions

Vortex is a small 2x2 field, where objects rotate. The function takes
coordinates of the points and potential offset, and calculates new
columns:

## Usage

``` r
coords_to_vortex_coords(moment, offset, settings)
```

## Arguments

- moment:

  Position tibble with integer `x` and `y` columns.

- offset:

  Vector of two numbers (`x` and `y`) by which the vortex grid is
  shifted.

- settings:

  List with basic properties. Currently not used.

## Value

`moment` with extra columns `vx`, `vy` and `pos`.

## Details

- `vx`, `vy`: coordinates of the vortex the object belongs to

- `pos`: position of the object within the vortex, one of `"SW"`,
  `"SE"`, `"NW"`, `"NE"`

NOTE: object coordinates (and vortex coordinates `vy`) assume the y-axis
has positive values upwards (like a chart, not like a screen).
