# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this is

`motrack` is an R package (v0.3.0) for generating and visualizing
trajectories for Multiple Object Tracking (MOT) experiments. It produces
object positions and trajectories as tibbles and exports them as
delimited text. No compiled code.

## Commands

Run from the repo root. Check which R is on the path (`which Rscript`);
the location differs between machines.

``` sh
# Install dependencies
Rscript -e 'devtools::install_deps(dependencies = TRUE)'

# Load package for interactive work (no install needed)
Rscript -e 'devtools::load_all()'

# Run all tests
Rscript -e 'devtools::test()'

# Run one test file (filter matches the name after "test-")
Rscript -e 'devtools::test(filter = "bounce-pair")'

# Regenerate NAMESPACE and man/ from roxygen comments (RoxygenNote 7.3.3)
Rscript -e 'devtools::document()'

# Full R CMD check (CI runs it on macOS, Windows, Ubuntu devel/release/oldrel)
Rscript -e 'devtools::check()'

# Build the pkgdown site (CI: .github/workflows/pkgdown.yaml)
Rscript -e 'pkgdown::build_site()'
```

- There is no flag for a single `test_that()` block. Use
  [`testthat::test_file()`](https://testthat.r-lib.org/reference/test_file.html)
  after `load_all()`, or temporarily comment out the others.
- `devtools::load_all()` and `test()` abort if any `Imports` package is
  missing (`circular`, `ggforce` and `animation` are the usual ones).
  Install them. Do not work around this by editing `DESCRIPTION`.
- Edit roxygen comments in `R/*.R`, then run `devtools::document()`. Do
  not hand-edit `NAMESPACE` or `man/*.Rd`. Running a roxygen2 version
  other than the one in `RoxygenNote` rewrites `DESCRIPTION`, so check
  `git diff DESCRIPTION` afterwards.
- There is no lint configuration.

## Architecture

The package is a single simulation pipeline. Each function lives in its
own file named after it, so find code with `R/<function_name>.R`.

**Data model.** Everything is a tibble with an `object` column (object
IDs): - Position: `object, x, y` plus optional `direction` (radians; 0 =
3 o’clock, counter-clockwise) and `speed`. Validated by
[`is_valid_position()`](https://visionlabels.github.io/motrack/reference/is_valid_position.md)
(requires all three columns, finite numeric `x`/`y`, at least one row,
unique non-`NA` `object`). - Trajectory: `time, object, x, y` plus
`direction`, `speed`, and step-specific state columns (`ttt` for zigzag;
`ttc`, `mov_state`, `speed_scale` for wait-and-move). Validated by
[`is_valid_trajectory()`](https://visionlabels.github.io/motrack/reference/is_valid_trajectory.md). -
`position8c` and `trajectory8c` (in `data/`, built by
`data-raw/trajectory8c.R`) are example datasets.

**Settings.** Settings are a plain named list.
[`default_settings()`](https://visionlabels.github.io/motrack/reference/default_settings.md)
defines the valid keys;
[`new_settings()`](https://visionlabels.github.io/motrack/reference/new_settings.md)
overrides them and rejects unknown names. It also accepts two optional
keys, `speed` and `arena_inside_radius`, which are not in the defaults.

**Speed.** `settings$speed` (or a `speed` column in the start position)
is either a number or a list of functions `function(time, moment)`
(write `c(f)` to wrap a single function).
[`cur_speed()`](https://visionlabels.github.io/motrack/reference/cur_speed.md)
evaluates it, and it is called only from
[`extrapolate_moment()`](https://visionlabels.github.io/motrack/reference/extrapolate_moment.md),
so every step function that moves objects through
[`extrapolate_moment()`](https://visionlabels.github.io/motrack/reference/extrapolate_moment.md)
supports function speeds. `step_vortex` reads `settings$speed` as a
plain number and does not. `step_waitandmove` scales movement through
its `speed_scale` column instead of changing `speed`.

**Generation pipeline.** 1. `generate_positions_random(n, settings)` (or
[`generate_positions_grid()`](https://visionlabels.github.io/motrack/reference/generate_positions_grid.md))
places objects in a square/circle/donut arena (`xlim`/`ylim`,
`arena_shape`), optionally enforcing `min_distance`. 2.
`make_random_trajectory(start, timescale, settings, step_function, ...)`
iterates over `timescale`. Each step first applies the enabled bounce
functions (`bounce_off_square`, `bounce_off_circle`,
`bounce_off_inside`, `bounce_off_others`) to the current moment, then
calls `step_function(moment, time_next, settings, ...)`. The result is a
list-column of per-time moments that is unnested at the end. 3. Step
functions (`step_direct`, `step_zigzag`, `step_waitandmove`,
`step_vonmises`) return the next moment. They carry per-object state in
extra columns, initialized on the first call (the
`if (!"ttt" %in% names(moment))` pattern). Custom steps should follow
the same signature and use
[`extrapolate_moment()`](https://visionlabels.github.io/motrack/reference/extrapolate_moment.md)
to move objects and
[`cur_speed()`](https://visionlabels.github.io/motrack/reference/cur_speed.md)
for speed.

Bouncing works on direction vectors and the next-step positions.
`bounce_off_others` uses single-linkage clustering (`hclust`/`cutree`)
on next-step distances to find collisions. `bounce_pair` does the
two-body case.

**Post-processing and output.** -
[`simplify_trajectory()`](https://visionlabels.github.io/motrack/reference/simplify_trajectory.md)
reduces the per-frame data to critical points (first, last and direction
changes, using wrapped angle differences). With `snapshot = TRUE` it
keeps all objects at every critical time. With `FALSE` it keeps only
each object’s own critical rows, with the original columns untouched. -
`estimate_position_for_time()` (internal) linearly interpolates `x`/`y`
between the two neighbouring frames. It is used for rendering simplified
trajectories. All other columns, including `time`, come from the earlier
frame. -
[`plot_position()`](https://visionlabels.github.io/motrack/reference/plot_position.md),
[`plot_trajectory()`](https://visionlabels.github.io/motrack/reference/plot_trajectory.md),
[`annotate_image()`](https://visionlabels.github.io/motrack/reference/annotate_image.md),
[`annotate_aperture()`](https://visionlabels.github.io/motrack/reference/annotate_aperture.md)
draw with ggplot2 and grid/png. -
[`render_trajectory_video()`](https://visionlabels.github.io/motrack/reference/render_trajectory_video.md)
uses the `animation` package and needs an external `ffmpeg` (path set
via
[`animation::ani.options`](https://rdrr.io/pkg/animation/man/ani.options.html)). -
[`save_trajectory()`](https://visionlabels.github.io/motrack/reference/save_trajectory.md)
/
[`load_trajectory()`](https://visionlabels.github.io/motrack/reference/load_trajectory.md)
read and write delimited text (`delim` argument) as
`time, x1, y1, x2, y2, ...` with no header. -
[`validate_trajectory()`](https://visionlabels.github.io/motrack/reference/validate_trajectory.md)
and
[`validate_object_direction_change()`](https://visionlabels.github.io/motrack/reference/validate_object_direction_change.md)
check output.

**Tidy evaluation.** Code uses `dplyr` verbs with `.data$col` pronouns.
The package uses the native `|>` pipe internally.

**Grid / vortex steps (experimental).** `step_vortex`, `step_grid` and
`calculate_incomplete_vortex` are documented but not exported
(`step_vortex` and `step_grid` are absent from `NAMESPACE`;
`coords_to_vortex_coords` is exported). `step_vortex` does not yet
respect arena limits fully, `step_grid` resamples directions with an
unbounded `while (TRUE)` loop, and they round positions to the grid.
Check them before relying on them.

Internal helpers marked `@noRd` (no `.Rd` page): `distances()`,
`estimate_position_for_time()`, `calculate_incomplete_vortex()`.

## Tests

Tests live in `tests/testthat/` (one `test-<name>.R` per function or
topic; testthat edition 3, based on the `Config/testthat/edition` in
`DESCRIPTION`). Randomized tests call
[`set.seed()`](https://rdrr.io/r/base/Random.html). Internal functions
can be called directly because tests run inside the package namespace.
