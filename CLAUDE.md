# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this is

`motrack` is an R package (v0.3.0) for generating and visualizing
trajectories for Multiple Object Tracking (MOT) experiments. It produces
object positions and trajectories as tibbles and exports them as
delimited text. No compiled code.

## Commands

Run from the repo root. R is at `/usr/local/bin/R`; `devtools`,
`testthat`, `roxygen2`, `circular` and `ggforce` are installed locally.

``` sh
# Install dependencies
Rscript -e 'devtools::install_deps(dependencies = TRUE)'

# Load package for interactive work (no install needed)
Rscript -e 'devtools::load_all()'

# Run all tests (currently: 75 pass, 0 fail, 5 warnings from tidyselect's .data usage)
Rscript -e 'devtools::test()'

# Run one test file (filter matches the name after "test-")
Rscript -e 'devtools::test(filter = "bounce-pair")'

# Run a single test_that() block: no CLI flag; load_all() first, then
# testthat::test_file() on the file, or temporarily comment out others

# Regenerate NAMESPACE and man/ from roxygen comments (RoxygenNote 7.1.1)
Rscript -e 'devtools::document()'

# Full R CMD check (what CI runs on macOS, Windows, Ubuntu)
Rscript -e 'devtools::check()'

# Build the pkgdown site (CI: .github/workflows/pkgdown.yaml)
Rscript -e 'pkgdown::build_site()'
```

Edit roxygen comments in `R/*.R`, then run `devtools::document()`. Do
not hand-edit `NAMESPACE` or `man/*.Rd`.

## Architecture

The package is a single simulation pipeline. Understanding it requires
reading `R/trajectory.R` and `R/position.R` together.

**Data model.** Everything is a tibble with an `object` column (object
IDs): - Position: `object, x, y` plus optional `direction` (radians; 0 =
3 o’clock, counter-clockwise) and `speed`. Validated by
[`is_valid_position()`](https://visionlabels.github.io/motrack/reference/is_valid_position.md). -
Trajectory: `time, object, x, y` plus `direction`, `speed`, and
step-specific state columns (`ttt` for zigzag; `ttc`, `mov_state`,
`speed_scale` for wait-and-move). Validated by
[`is_valid_trajectory()`](https://visionlabels.github.io/motrack/reference/is_valid_trajectory.md). -
`position8c` and `trajectory8c` (in `data/`) are example datasets.

**Settings.** Settings are a plain named list.
[`default_settings()`](https://visionlabels.github.io/motrack/reference/default_settings.md)
(`R/position.R`) defines the valid keys;
[`new_settings()`](https://visionlabels.github.io/motrack/reference/new_settings.md)
overrides them and rejects unknown names. It also accepts two optional
keys, `speed` and `arena_inside_radius`, which are not in the defaults.
`speed` can be a number or a list of functions `function(time, moment)`;
[`cur_speed()`](https://visionlabels.github.io/motrack/reference/cur_speed.md)
evaluates them.

**Generation pipeline.** 1. `generate_positions_random(n, settings)`
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

Bouncing (`bounce_pair`, `bounce_off_*` in `R/trajectory.R`) works on
direction vectors and the next-step positions. `bounce_off_others` uses
single-linkage clustering (`hclust`/`cutree`) on next-step distances to
find collisions.

**Post-processing and output.** -
[`simplify_trajectory()`](https://visionlabels.github.io/motrack/reference/simplify_trajectory.md)
reduces the per-frame data to critical points. It can keep only
snapshots, which is useful for export. - `estimate_position_for_time()`
interpolates positions at arbitrary times. -
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
read and write delimited text (`delim` argument). -
[`validate_trajectory()`](https://visionlabels.github.io/motrack/reference/validate_trajectory.md)
and
[`validate_object_direction_change()`](https://visionlabels.github.io/motrack/reference/validate_object_direction_change.md)
check output.

**Tidy evaluation.** Code uses `dplyr` verbs with `.data$col` pronouns.
`R/utils-tidy-eval.R` re-exports rlang symbols; `R/utils-pipe.R`
re-exports `%>%`. Follow these patterns rather than adding new imports.

**Work in progress: `R/discrete.R`.** This file (grid positions,
`step_vortex`, `step_grid`) is not exported (see `NAMESPACE`) and is not
yet documented. It uses `|>` and unqualified dplyr verbs (`left_join`,
`join_by`, `mutate`) that are not imported, and the recent commit notes
the vortex step does not yet respect arena limits. Treat it as
experimental. Check it before relying on it.

## Tests

Tests live in `tests/testthat/` and cover settings validation, bounce
calculations, position generation, trajectory generation, and saving.
Randomized tests call [`set.seed()`](https://rdrr.io/r/base/Random.html)
(e.g., `test-generate-positions.R`, `test-generate-trajectories.R`).
There is no lint configuration.
