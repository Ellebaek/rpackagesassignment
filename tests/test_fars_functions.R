test_that("make_filename_correct", {
  expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
})

test_that("make_filename_type", {
  expect_that(make_filename(2013), is_a("character"))
})
