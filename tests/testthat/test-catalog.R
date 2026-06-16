test_that("catalog() returns a data frame with expected columns", {
  result <- catalog()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("table_no", "title", "dataset", "href") %in% names(result)))
})

test_that("catalog() returns 89 rows by default", {
  result <- catalog()
  expect_equal(nrow(result), 89L)
})

test_that("catalog() type='household' returns only Household tables", {
  result <- catalog(type = "household")
  expect_true(all(grepl("^Household", result$table_no)))
  expect_equal(nrow(result), 23L)
})

test_that("catalog() type='individual' returns only Individual tables", {
  result <- catalog(type = "individual")
  expect_true(all(grepl("^Individual", result$table_no)))
  expect_equal(nrow(result), 66L)
})

test_that("catalog() search filters titles case-insensitively", {
  res_lower <- catalog(search = "education")
  res_upper <- catalog(search = "EDUCATION")
  expect_equal(nrow(res_lower), nrow(res_upper))
  expect_true(nrow(res_lower) > 0L)
  expect_true(all(grepl("education", res_lower$title, ignore.case = TRUE)))
})

test_that("catalog() search + type combination works", {
  result <- catalog(search = "water", type = "household")
  expect_true(nrow(result) > 0L)
  expect_true(all(grepl("^Household", result$table_no)))
  expect_true(all(grepl("water", result$title, ignore.case = TRUE)))
})

test_that("catalog() search with no match returns 0-row data frame", {
  result <- catalog(search = "zzz_no_match_xyzabc")
  expect_equal(nrow(result), 0L)
})

test_that("catalog() type argument uses partial matching", {
  expect_error(catalog(type = "xyz"), "arg")
})

test_that("catalog() dataset column matches actual loaded datasets", {
  cat <- catalog()
  expect_true("Hhld01" %in% cat$dataset)
  expect_true("Indv01" %in% cat$dataset)
  expect_true("Indv71" %in% cat$dataset)
})

test_that("catalog() href column contains URLs", {
  cat <- catalog()
  expect_true(all(grepl("^http", cat$href)))
})
