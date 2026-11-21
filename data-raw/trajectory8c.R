## code to prepare `position8c` and `trajectory8c`

set.seed(123)
sett_generate <-
  new_settings(
    xlim = c(-7, 7),
    ylim = c(-7, 7),
    min_distance = 2,
    arena_shape = "circle"
  )
sett_move <-
  new_settings(
    speed = 5,
    xlim = c(-9, 9),
    ylim = c(-9, 9),
    bounce_off_square = FALSE,
    bounce_off_circle = TRUE,
    circle_bounce_jitter = pi / 6
  )
sett_show <-
  new_settings(show_labels = TRUE)
position8c <- generate_positions_random(8, sett_generate)
# plot_position(position8c, sett_move)
trajectory8c <- make_random_trajectory(
  position8c,
  seq(0, 8, by = 0.1),
  sett_move,
  step_direct
)

# plot_trajectory(trajectory8c, sett_show)
usethis::use_data(position8c, overwrite = TRUE, compress = "gzip")
usethis::use_data(trajectory8c, overwrite = TRUE, compress = "gzip")
