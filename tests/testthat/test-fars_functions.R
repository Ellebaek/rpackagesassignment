test_that("make_filename_correct", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

test_that("make_filename_type", {
  expect_type(make_filename(2013), "character")
})
