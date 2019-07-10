context("Generate positions")

xlims <- c(-12, 12)
ylims <- c(-12, 12)
xlims2 <- xlims + c(2, 2) # must be same, otherwise not square
ylims2 <- ylims + c(-3, -3)
border_gap <- 1
object_gap <- 1
radius_out <- 12 # manually, but corresponds to xlims and ylims
radius_in <- 3

set.seed(1)

# square
ss <- new_settings(xlim = xlims, ylim = ylims, arena_shape = "square", min_distance = object_gap)
ps1 <- generate_positions_random(8, ss, check_distance = T, border_distance = 0)
ps2 <- generate_positions_random(8, ss, check_distance = T, border_distance = border_gap)

# circle
sc <- new_settings(xlim = xlims, ylim = ylims, arena_shape = "circle", min_distance = object_gap)
pc1 <- generate_positions_random(8, sc, check_distance = T, border_distance = 0) %>%
  dplyr::mutate(d = sqrt((x - mean(xlims))^2 + (y - mean(ylims))^2))
pc2 <- generate_positions_random(8, sc, check_distance = T, border_distance = border_gap) %>%
  dplyr::mutate(d = sqrt((x - mean(xlims))^2 + (y - mean(ylims))^2))

# donut
sd <- new_settings(
  xlim = xlims, ylim = ylims, arena_shape = "donut", min_distance = object_gap,
  arena_inside_radius = radius_in
)
pd1 <- generate_positions_random(8, sd, check_distance = T, border_distance = 0) %>%
  dplyr::mutate(d = sqrt((x - mean(xlims))^2 + (y - mean(ylims))^2))
pd2 <- generate_positions_random(8, sd, check_distance = T, border_distance = border_gap) %>%
  dplyr::mutate(d = sqrt((x - mean(xlims))^2 + (y - mean(ylims))^2))

# shifted border - square, circle, donut
ps3 <- generate_positions_random(
  8, new_settings(.from = ss, xlim = xlims2, ylim = ylims2),
  check_distance = T, border_distance = 0
)
pc3 <- generate_positions_random(
  8, new_settings(.from = sc, xlim = xlims2, ylim = ylims2),
  check_distance = T, border_distance = 0
) %>%
  dplyr::mutate(d = sqrt((x - mean(xlims2))^2 + (y - mean(ylims2))^2))
pd3 <- generate_positions_random(
  8, new_settings(.from = sd, xlim = xlims2, ylim = ylims2),
  check_distance = T, border_distance = 0
) %>%
  dplyr::mutate(d = sqrt((x - mean(xlims2))^2 + (y - mean(ylims2))^2))

test_that("Nothing exceeds outer limits: square", {
  expect_true(all(ps1$x < max(xlims)))
  expect_true(all(ps1$x > min(xlims)))
  expect_true(all(ps1$y < max(ylims)))
  expect_true(all(ps1$y > min(ylims)))
})

test_that("Nothing exceeds outer limits: circle", {
  expect_true(all(pc1$d < radius_out))
})

test_that("Nothing exceeds outer limits: donut", {
  expect_true(all(pd1$d < radius_out))
})

test_that("Nothing exceeds outer limits with gap: square", {
  expect_true(all(ps2$x < (max(xlims) - border_gap)))
  expect_true(all(ps2$x > (min(xlims) + border_gap)))
  expect_true(all(ps2$y < (max(ylims) - border_gap)))
  expect_true(all(ps2$y > (min(ylims) + border_gap)))
})

test_that("Nothing exceeds outer limits with gap: circle", {
  expect_true(all(pc2$d < (radius_out - border_gap)))
})

test_that("Nothing exceeds outer limits with gap: donut", {
  expect_true(all(pd2$d < (radius_out - border_gap)))
})

test_that("Nothing exceeds inner limits: donut", {
  expect_true(all(pd1$d > radius_in))
})

test_that("Nothing exceeds inner limits with gap: donut", {
  expect_true(all(pd2$d > (radius_in + border_gap)))
})

test_that("Minimum distances are met", {
  expect_true(is_distance_at_least(ps1, object_gap))
  expect_true(is_distance_at_least(ps2, object_gap))
  expect_true(is_distance_at_least(pc1, object_gap))
  expect_true(is_distance_at_least(pc2, object_gap))
  expect_true(is_distance_at_least(pd1, object_gap))
  expect_true(is_distance_at_least(pd2, object_gap))
})

test_that("Nothing exceeds outer limits when off center: square", {
  expect_true(all(ps3$x < max(xlims2)))
  expect_true(all(ps3$x > min(xlims2)))
  expect_true(all(ps3$y < max(ylims2)))
  expect_true(all(ps3$y > min(ylims2)))
})

test_that("Nothing exceeds outer limits when off center: circle", {
  expect_true(all(pc3$d < radius_out))
})

test_that("Nothing exceeds outer limits when off center: donut", {
  expect_true(all(pd3$d < radius_out))
})
