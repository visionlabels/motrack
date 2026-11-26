# motrack 0.4.0

## New features

* New `generate_positions_grid()` places objects on a regular grid, with `step` and `offset` arguments.
* New `step_vortex()` and `step_grid()` for discrete grid and vortex trajectories. 

## Improvements

* Trajectory generation, plotting, rendering and validation code in `R/trajectory.R` and `R/position.R` was substantially reworked. This included `make_random_trajectory()`, `step_zigzag()`, `plot_position()`, `plot_trajectory()`, `annotate_aperture()`, `render_trajectory_video()`, `validate_trajectory()` and `validate_object_direction_change()`.
* Added an example for a variable-speed trial.
* Added tests for trajectory generation (`test-generate-trajectories.R`).

# motrack 0.3.0

## New features

* New step function `step_waitandmove()`: objects alternate between moving and waiting phases, with configurable `move_time`, `wait_time`, `wait_prob` and optional synchronized start (`syncstart`).
* Speed can now be a function of time (e.g., acceleration), not just a number. Speed functions have the signature `function(time, moment)`. They work with every step function that moves objects through `extrapolate_moment()`.
* New exported helpers `cur_speed()` and `extrapolate_moment()` for writing custom step functions.

## Infrastructure

* Added a pkgdown website (https://visionlabels.github.io/motrack/), a logo and favicons. The site is built by GitHub Actions.
* Replaced Travis CI with GitHub Actions (`R-CMD-check`, `pkgdown`) and updated the README badges.
* The repository moved to the `visionlabels` organization.
* Regenerated documentation with roxygen2 7.1.1.

# motrack 0.2.0

* Added trajectory simplification. You can generate trajectory with 100 frames per second and later remove datapoints, which can be interpolated.
* Updated versioning. From now, we will start to use more minor/patch numbering.

# motrack 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.
