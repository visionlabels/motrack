context("Generate trajectories")

spd <- 5
xlims_move <- c(-9,9)
ylims_move <- c(-9,9)
sett_generate <-
  new_settings(xlim = c(-7, 7), ylim = c(-7, 7), min_distance = 2,
               arena_shape = "circle")
# they can move in a circular arena, bounce off borders and other objects
sett_move <-
  new_settings(xlim = xlims_move, ylim = ylims_move,
               bounce_off_square = F,
               bounce_off_circle = T, circle_bounce_jitter = pi / 6,speed = spd)
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

# Object move for 0.5-1.5 seconds or wait for 0.2-0.5 seconds. After movement/waiting, it waits with probability 0.1, otherwise it changes direction randomly
trajectory_wm <-
  make_random_trajectory(position, timescale, sett_move,
                         step_waitandmove, move_time = c(.5, 1.5),
                         wait_time = c(.2,.5), wait_prob = 0.1, syncstart = F)

test_that("All speeds are equal to generated value", {
  expect_true(all(trajectory_wm$speed==spd))
  expect_true(all(trajectory_d$speed==spd))
  expect_true(all(trajectory_z$speed==spd))
  expect_true(all(trajectory_v$speed==spd))
})

dist_wm <- sqrt((trajectory_wm$x^2+trajectory_wm$y^2))
dist_z <- sqrt((trajectory_z$x^2+trajectory_z$y^2))
dist_v <- sqrt((trajectory_v$x^2+trajectory_v$y^2))
dist_d <- sqrt((trajectory_d$x^2+trajectory_d$y^2))
test_that("Objects do not move outside the circular arena", {
  #expect_true(max(dist_z) < max(xlims_move))
  #expect_true(max(dist_v) < max(xlims_move))
  #expect_true(max(dist_d) < max(xlims_move))

  expect_true(max(trajectory_wm$x) <= max(xlims_move))
  expect_true(min(trajectory_wm$x) >= min(xlims_move))
  expect_true(max(trajectory_wm$y) <= max(ylims_move))
  expect_true(min(trajectory_wm$y) >= min(ylims_move))

  expect_true(max(trajectory_d$x) <= max(xlims_move))
  expect_true(min(trajectory_d$x) >= min(xlims_move))
  expect_true(max(trajectory_d$y) <= max(ylims_move))
  expect_true(min(trajectory_d$y) >= min(ylims_move))

  expect_true(max(trajectory_z$x) <= max(xlims_move))
  expect_true(min(trajectory_z$x) >= min(xlims_move))
  expect_true(max(trajectory_z$y) <= max(ylims_move))
  expect_true(min(trajectory_z$y) >= min(ylims_move))

  expect_true(max(trajectory_v$x) <= max(xlims_move))
  expect_true(min(trajectory_v$x) >= min(xlims_move))
  expect_true(max(trajectory_v$y) <= max(ylims_move))
  expect_true(min(trajectory_v$y) >= min(ylims_move))
})

time_diff_d <- trajectory_d %>% group_by(object) %>% mutate(past_time = lag(time),time_diff = time-past_time) %>% pull(time_diff)
time_diff_z <- trajectory_z %>% group_by(object) %>% mutate(past_time = lag(time),time_diff = time-past_time) %>% pull(time_diff)
time_diff_v <- trajectory_v %>% group_by(object) %>% mutate(past_time = lag(time),time_diff = time-past_time) %>% pull(time_diff)
time_diff_wm <- trajectory_wm %>% group_by(object) %>% mutate(past_time = lag(time),time_diff = time-past_time) %>% pull(time_diff)

test_that("Time coordinates are set properly", {
  expect_true(all(time_diff_d[!is.na(time_diff_d)]-0.1 <=0.000001))
  expect_true(all(time_diff_z[!is.na(time_diff_z)]-0.1 <=0.000001))
  expect_true(all(time_diff_v[!is.na(time_diff_v)]-0.1 <=0.000001))
  expect_true(all(time_diff_wm[!is.na(time_diff_wm)]-0.1 <=0.000001))
})

test_that("Wait-and-move uses specific parameters, test that they are set correct", {
  expect_true(all(trajectory_wm$speed_scale[trajectory_wm$mov_state=="m"&!is.na(trajectory_wm$mov_state=="m")] == 1))
  expect_true(all(trajectory_wm$speed_scale[trajectory_wm$mov_state=="w"&!is.na(trajectory_wm$mov_state)] == 0))
})
