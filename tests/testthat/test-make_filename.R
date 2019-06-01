test_that("File name constructs normally", {
  expect_identical(make_filename(2013), "accident_2013.csv.bz2")
  expect_identical(make_filename(2020), "accident_2020.csv.bz2")
  expect_identical(make_filename("ab"), "accident_NA.csv.bz2")
})
