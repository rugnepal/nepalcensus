test_that("search_census() returns same result as catalog(search = ...)", {
  expect_equal(
    search_census("migration"),
    catalog(search = "migration")
  )
})

test_that("search_census() errors on missing keyword", {
  expect_error(search_census(), "'keyword'")
})

test_that("search_census() errors on empty string keyword", {
  expect_error(search_census(""), "'keyword'")
})

test_that("search_census() type argument passes through", {
  res_all  <- search_census("disability")
  res_indv <- search_census("disability", type = "individual")
  expect_true(nrow(res_all) >= nrow(res_indv))
})

test_that("search_census() returns data frame with expected columns", {
  result <- search_census("water")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("table_no", "title", "dataset", "href") %in% names(result)))
})

test_that("search_census() with no match returns 0-row data frame", {
  result <- search_census("zzz_no_match_xyzabc")
  expect_equal(nrow(result), 0L)
})
