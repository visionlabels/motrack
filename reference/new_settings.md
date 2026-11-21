# Creates a custom copy of settings

Creates a custom copy of settings

## Usage

``` r
new_settings(.from = NULL, ...)
```

## Arguments

- .from:

  optional parent from which defaults are inherited

- ...:

  named list of properties to override default settings.

## Value

Updated list of settings

## See also

[`default_settings()`](https://visionlabels.github.io/motrack/reference/default_settings.md)

## Examples

``` r
s1 <- new_settings(xlim = c(0, 10), ylim = c(0, 10))
s2 <- new_settings(.from = s1, show_labels = TRUE)
```
