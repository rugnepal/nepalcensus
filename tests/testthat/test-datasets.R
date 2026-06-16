CORE_COLS <- c("prov", "dist", "gapa", "provname", "dname", "gapaname",
               "category", "number")

test_that("datacatalog has 89 rows and 4 columns", {
  data(datacatalog, envir = environment())
  expect_equal(nrow(datacatalog), 89L)
  expect_equal(ncol(datacatalog), 4L)
  expect_named(datacatalog, c("table_no", "title", "dataset", "href"))
})

test_that("datacatalog dataset names are loadable", {
  data(datacatalog, envir = environment())
  for (nm in datacatalog$dataset) {
    expect_true(
      exists(nm, where = asNamespace("nepalcensus"), inherits = FALSE),
      info = paste("dataset not found:", nm)
    )
  }
})

test_that("Hhld01 has expected structure", {
  data(Hhld01, envir = environment())
  expect_s3_class(Hhld01, "data.frame")
  expect_true(all(CORE_COLS %in% names(Hhld01)))
  expect_gt(nrow(Hhld01), 0L)
  expect_true(is.character(Hhld01$number))
})

test_that("Hhld01 province code 0 gives Nepal total", {
  data(Hhld01, envir = environment())
  nepal_rows <- Hhld01[Hhld01$prov == "0" & Hhld01$dist == "0" & Hhld01$gapa == "0", ]
  expect_gt(nrow(nepal_rows), 0L)
})

test_that("Indv01 has expected structure including n_hhld", {
  data(Indv01, envir = environment())
  expect_s3_class(Indv01, "data.frame")
  expect_true("n_hhld" %in% names(Indv01))
  expect_true(all(c("prov", "dist", "gapa", "category", "number") %in% names(Indv01)))
  expect_gt(nrow(Indv01), 0L)
})

test_that("Indv03 has age group columns", {
  data(Indv03, envir = environment())
  expect_true(all(c("agegrp", "agegrpname") %in% names(Indv03)))
})

test_that("Indv16 has sex and age group columns (disability table)", {
  data(Indv16, envir = environment())
  expect_true(all(c("sex", "sexname", "agegrp", "agegrpname") %in% names(Indv16)))
})

test_that("Hhld23 (absent population) has sex and age group columns", {
  data(Hhld23, envir = environment())
  expect_true(all(c("sex", "sexname", "agegrp", "agegrpname") %in% names(Hhld23)))
})

test_that("number column is character across key datasets", {
  for (nm in c("Hhld01", "Hhld10", "Indv01", "Indv04", "Indv17")) {
    env <- new.env()
    data(list = nm, envir = env)
    ds <- get(nm, envir = env)
    expect_true(is.character(ds$number),
                info = paste(nm, ": number should be character"))
  }
})

test_that("prov code 0 exists in Hhld01 (Nepal total rows present)", {
  data(Hhld01, envir = environment())
  expect_true(any(Hhld01$prov == "0"))
})

test_that("all Hhld datasets exist in the package", {
  hhld_names <- paste0("Hhld", sprintf("%02d", 1:23))
  for (nm in hhld_names) {
    expect_true(
      exists(nm, where = asNamespace("nepalcensus"), inherits = FALSE),
      info = paste(nm, "missing from package")
    )
  }
})

test_that("Indv datasets exist (gaps at 08, 12-15 expected)", {
  present <- c(1:7, 9:11, 16:71)
  indv_names <- paste0("Indv", sprintf("%02d", present))
  for (nm in indv_names) {
    expect_true(
      exists(nm, where = asNamespace("nepalcensus"), inherits = FALSE),
      info = paste(nm, "missing from package")
    )
  }
})

test_that("Indv datasets do NOT include known gaps (08, 12-15)", {
  gaps <- c("Indv08", "Indv12", "Indv13", "Indv14", "Indv15")
  for (nm in gaps) {
    expect_false(
      exists(nm, where = asNamespace("nepalcensus"), inherits = FALSE),
      info = paste(nm, "should not exist (known gap)")
    )
  }
})
