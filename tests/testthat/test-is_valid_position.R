valid_pos <- tibble::tibble(object = 1:4, x = c(0, 1, 2, 3), y = c(3, 2, 1, 0))

test_that("A well-formed position is valid", {
  expect_true(is_valid_position(valid_pos))
})

test_that("Bundled example position is valid", {
  expect_true(is_valid_position(position8c))
})

test_that("Single object is valid", {
  expect_true(is_valid_position(valid_pos[1, ]))
})

test_that("Object identifiers need not be numeric", {
  p <- valid_pos
  p$object <- letters[1:4]
  expect_true(is_valid_position(p))
})

test_that("Extra columns are allowed", {
  p <- valid_pos
  p$direction <- 0
  p$speed <- 5
  expect_true(is_valid_position(p))
})

test_that("Duplicated object identifiers are invalid", {
  p <- valid_pos
  p$object[2] <- 1
  expect_false(is_valid_position(p))
})

test_that("Missing x or y values are invalid", {
  p <- valid_pos
  p$x[2] <- NA
  expect_false(is_valid_position(p))
  p <- valid_pos
  p$y[3] <- NA
  expect_false(is_valid_position(p))
})

test_that("Non-numeric x or y are invalid", {
  p <- valid_pos
  p$x <- as.character(p$x)
  expect_false(is_valid_position(p))
  p <- valid_pos
  p$y <- as.character(p$y)
  expect_false(is_valid_position(p))
})

test_that("Integer coordinates are valid", {
  p <- valid_pos
  p$x <- as.integer(p$x)
  p$y <- as.integer(p$y)
  expect_true(is_valid_position(p))
})

test_that("Missing x or y column is invalid", {
  # tibble warns about the unknown column; the result is what matters here
  suppressWarnings({
    expect_false(is_valid_position(valid_pos[, c("object", "y")]))
    expect_false(is_valid_position(valid_pos[, c("object", "x")]))
  })
})
