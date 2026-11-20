test_that("lm() to list works", {
  db <- tools::Rd_db("stats")
  lm_rd <- db[["lm.Rd"]]
  expect_snapshot(names(rd_to_list(lm_rd)))
})

test_that("Internal functions return NULL", {
  db <- tools::Rd_db("purrr")
  map_rd <- db[["map_raw.Rd"]]
  expect_null(rd_to_list(map_rd))
})

test_that("Source reference is present", {
  rd <- rd_to_list(test_path("test-pkg", "man", "llm_classify.Rd"))
  expect_equal(rd$source, "R/llm-classify.R")
})

test_that("Source reference is present", {
  rd <- rd_to_list(test_path("test-pkg", "man", "llm_use.Rd"))
  expect_in("...", map_chr(rd$arguments, \(x) x$argument))
})

test_that("Extract returns NULL on error", {
  expect_null(rd_extract_text2(notexists))
})

test_that("examplesIf wrapper becomes a condition", {
  rd <- tools::parse_Rd(textConnection(
    r"(\name{a}\title{a}
\examples{
\dontshow{if (rlang::is_installed("x")) withAutoprint(\{ # examplesIf}
f(1)
\dontshow{\}) # examplesIf}
})"
  ))
  expect_equal(
    rd_to_list(rd)$examples,
    list(code_run = "f(1)", eval = "rlang::is_installed(\"x\")")
  )
})

test_that("Arguments keep block content", {
  rd <- tools::parse_Rd(textConnection(
    r"(\name{a}\title{a}
\arguments{\item{x}{Either: \itemize{\item a \item b}}})"
  ))
  expect_equal(
    rd_to_list(rd)$arguments,
    list(list(argument = "x", description = c("Either:", "", "- a", "- b")))
  )
})
