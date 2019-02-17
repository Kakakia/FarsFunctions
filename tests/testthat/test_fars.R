library(testthat)
f<-file(system.file("R", "fars_functions.R", package = "farsfunctions"), open = "r")
source(f)
close(f)

expect_equal(make_filename(2013), "accident_2013.csv.bz2")
expect_equal(dim(fars_summarize_years(c(2013:2015))), c(12, 4))
expect_equal(length(fars_read_years(c(2013:2015))), 3)

