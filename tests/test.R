require(testthat)

test_that("File name exists", {
  file_name <<- make_filename("2014")
  expect_that(file_name, is_a("character"))
  expect_that(fars_read(), throws_error())
})

