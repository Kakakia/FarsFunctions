test_that("filename",{
  filename <- make_filename(year = 2013)
  expect_that(filename, equals("accident_2013.csv.bz2"))
})

test_that("read file",{
  data_2013 <- fars_read(filename = "accident_2013.csv.bz2")
  expect_that(data_2013, is_a("dataframe"))
})

