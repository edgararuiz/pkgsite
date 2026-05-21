test_that("rd_compare_titles matches for a simple Rd file", {
  example_pkg <- system.file("example", package = "pkgsite")
  result <- rd_compare_titles("rd_to_list.Rd", project = example_pkg)
  expect_setequal(result$html, result$list)
  expect_length(result$in_html_not_list, 0)
  expect_length(result$in_list_not_html, 0)
})

test_that("rd_compare_titles handles custom sections and Author(s)", {
  db <- tools::Rd_db("stats")
  lm_rd <- db[["lm.Rd"]]
  result <- rd_compare_titles(lm_rd)
  expect_setequal(result$html, result$list)
  expect_length(result$in_html_not_list, 0)
  expect_length(result$in_list_not_html, 0)
})

test_that("rd_compare_pkg_titles prints results and returns invisible", {
  example_pkg <- system.file("example", package = "pkgsite")
  expect_invisible(suppressMessages(rd_compare_pkg_titles(project = example_pkg)))
  expect_snapshot(rd_compare_pkg_titles(project = example_pkg))
})

test_that("rd_normalize_header lowercases and strips non-alpha chars", {
  expect_equal(rd_normalize_header("See Also"), "seealso")
  expect_equal(rd_normalize_header("Author(s)"), "author")
  expect_equal(rd_normalize_header("Using time series"), "usingtimeseries")
})
