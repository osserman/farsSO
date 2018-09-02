library(testthat)

test_that("summarize years", {
  my_results <- fars_summarize_years(list('2015','2013'))
  expect_that(my_results, is_a("tbl_df"))
  expect_that(colnames(my_results),equals(c("MONTH","2013","2015")))
})

