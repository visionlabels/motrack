# motrack - tools for Multiple Object Tracking

<!-- badges: start -->
[![R-CMD-check](https://github.com/visionlabels/motrack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/visionlabels/motrack/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[Multiple Object Tracking](http://www.scholarpedia.org/article/Multiple_object_tracking) 
is a popular experimental paradigm used in visual attention research.
Your goal is to remember several objects and 
keep track of the them while they move ([demo](https://youtu.be/lAQM4QJRYV8)).

This package allows researcher to generate object trajectories, 
visualize them and export them for experiments.
The trajectory and position data are in `tibble` format for easy further manipulation. 

## Installation

The package is in active development. 
You can install the current version from GitHub with following code:

``` r
# Install devtools package if necessary
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

# Install the stable verion from GitHub
devtools::install_github("visionlabels/motrack")
```

## Example

Let us create a trajectory or two:

``` r
library(motrack) #load the package

# we want objects starting 
# more further from the arena border and further from each other
sett_generate <-
  new_settings(xlim = c(-7, 7), ylim = c(-7, 7), min_distance = 2,
               arena_shape = "circle")
# they can move in a circular arena, bounce off borders and other objects
sett_move <-
  new_settings(speed = 5, xlim = c(-9, 9), ylim = c(-9, 9),
               bounce_off_square = F,
               bounce_off_circle = T, circle_bounce_jitter = pi / 6)
# here you can separately adjust settings for visualization
sett_show <-
  new_settings(show_labels = T)

# set seed for replicability
set.seed(1001)

position <- generate_positions_random(8, sett_generate)

# our starting positions
plot_position(position, sett_move)

# Make objects move for 8 seconds (0 to 8), adjust direction every 100 ms
timescale <- seq(0, 8, by = 0.1)

# Objects only bounce
trajectory_d <- 
  make_random_trajectory(position, timescale, sett_move, 
    step_direct)

# Object change direction randomly every 0.5-1.5 seconds
trajectory_z <- 
  make_random_trajectory(position, timescale, sett_move, 
    step_zigzag, ttt = c(.5, 1.5), syncstart = F)

# Object change direction smoothly
trajectory_v <- 
  make_random_trajectory(position, timescale, sett_move, 
    step_vonmises, kappa = 10)

plot_trajectory(trajectory_d, sett_show)
plot_trajectory(trajectory_z, sett_show)
plot_trajectory(trajectory_v, sett_show)

# It is possible to implement complex motion patterns:
# Object move for 0.5-1.5 seconds or wait for 0.2-0.5 seconds. 
# After movement/waiting, it waits with probability 0.1, 
# otherwise it changes direction randomly
trajectory_wm <- 
  make_random_trajectory(
    position, timescale, sett_move, 
    step_waitandmove, move_time = c(.5, 1.5), 
    wait_time = c(.2,.5), wait_prob = 0.1, syncstart = F)

# It is possible to vary speed during the trial based on the custom function
f1 <- function(time, moment) {
  5 + cos(time + moment$object) # different offset for each object
}
sett_move$speed <- c(f1)

trajectory_s <- 
  make_random_trajectory(
    position, timescale, sett_move, step_direct)
```

If you have `ffmpeg` installed, you can use `animation` package to render videos.

``` r
animation::ani.options(ffmpeg = "/usr/bin/ffmpeg")   # update to your path

render_trajectory_video("trajectory_d.mp4", trajectory_d, 
  new_settings(show_labels = T), targets = 1:4
)
render_trajectory_video("trajectory_z.mp4", trajectory_z, 
  new_settings(show_labels = T), targets = 1:4
)
render_trajectory_video("trajectory_v.mp4", trajectory_v, 
  new_settings(show_labels = T), targets = 1:4
)

# variable speed example
render_trajectory_video("trajectory_s.mp4", trajectory_s, 
  new_settings(show_labels = T), targets = 1:4
)
```
