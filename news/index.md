# Changelog

## motrack 0.4.0

### New features

- New
  [`generate_positions_grid()`](https://visionlabels.github.io/motrack/reference/generate_positions_grid.md)
  places objects on a regular grid, with `step` and `offset` arguments.
- New
  [`step_vortex()`](https://visionlabels.github.io/motrack/reference/step_vortex.md)
  and
  [`step_grid()`](https://visionlabels.github.io/motrack/reference/step_grid.md)
  for discrete grid and vortex trajectories.

### Improvements

- Trajectory generation, plotting, rendering and validation code in
  `R/trajectory.R` and `R/position.R` was substantially reworked. This
  included
  [`make_random_trajectory()`](https://visionlabels.github.io/motrack/reference/make_random_trajectory.md),
  [`step_zigzag()`](https://visionlabels.github.io/motrack/reference/step_zigzag.md),
  [`plot_position()`](https://visionlabels.github.io/motrack/reference/plot_position.md),
  [`plot_trajectory()`](https://visionlabels.github.io/motrack/reference/plot_trajectory.md),
  [`annotate_aperture()`](https://visionlabels.github.io/motrack/reference/annotate_aperture.md),
  [`render_trajectory_video()`](https://visionlabels.github.io/motrack/reference/render_trajectory_video.md),
  [`validate_trajectory()`](https://visionlabels.github.io/motrack/reference/validate_trajectory.md)
  and
  [`validate_object_direction_change()`](https://visionlabels.github.io/motrack/reference/validate_object_direction_change.md).
- Added an example for a variable-speed trial.
- Added tests for trajectory generation
  (`test-generate-trajectories.R`).

## motrack 0.3.0

### New features

- New step function
  [`step_waitandmove()`](https://visionlabels.github.io/motrack/reference/step_waitandmove.md):
  objects alternate between moving and waiting phases, with configurable
  `move_time`, `wait_time`, `wait_prob` and optional synchronized start
  (`syncstart`).
- Speed can now be a function of time (e.g., acceleration), not just a
  number. Speed functions have the signature `function(time, moment)`.
  They work with every step function that moves objects through
  [`extrapolate_moment()`](https://visionlabels.github.io/motrack/reference/extrapolate_moment.md).
- New exported helpers
  [`cur_speed()`](https://visionlabels.github.io/motrack/reference/cur_speed.md)
  and
  [`extrapolate_moment()`](https://visionlabels.github.io/motrack/reference/extrapolate_moment.md)
  for writing custom step functions.

### Infrastructure

- Added a pkgdown website (<https://visionlabels.github.io/motrack/>), a
  logo and favicons. The site is built by GitHub Actions.
- Replaced Travis CI with GitHub Actions (`R-CMD-check`, `pkgdown`) and
  updated the README badges.
- The repository moved to the `visionlabels` organization.
- Regenerated documentation with roxygen2 7.1.1.

## motrack 0.2.0

- Added trajectory simplification. You can generate trajectory with 100
  frames per second and later remove datapoints, which can be
  interpolated.
- Updated versioning. From now, we will start to use more minor/patch
  numbering.

## motrack 0.1.0.9000

- Added a `NEWS.md` file to track changes to the package.
