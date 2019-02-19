context("File input/output")

filename <- tempfile(fileext = ".csv")

save_trajectory(trajectory8c, filename)
trajload <- load_trajectory(filename)

unlink(filename)

test_that("Length does not change", {
  expect_equal(nrow(trajload), nrow(trajectory8c))
})

test_that("Loaded values did not change", {
  expect_equal(trajload$time, trajectory8c$time)
  expect_equal(trajload$object, trajectory8c$object)
  expect_equal(trajload$x, trajectory8c$x)
  expect_equal(trajload$y, trajectory8c$y)
})