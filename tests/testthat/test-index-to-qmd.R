test_that("Index parsing works", {
  skip_on_os("windows")
  yaml_file <- read_quarto(test_path("test-pkg2"))
  index <- yaml_file[["reference"]][["index"]]
  expect_snapshot(
    reference_index_convert(
      pkg = path_abs(test_path("test-pkg2")),
      index = index
    )
  )
})

test_that("Index parsing works", {
  skip_on_os("windows")
  expect_snapshot(
    index_to_qmd(
      pkg = path_abs(test_path("test-pkg2"))
    )
  )
})

test_that("Index lists S3 methods under their generic", {
  usage <- c(
    "tidy(x, ...)",
    "",
    "## S3 method for class 'foo'",
    "tidy(x, ...)",
    "`%+%`(e1, e2)"
  )
  expect_equal(usage_functions(usage), c("tidy", "`%+%`"))
})
