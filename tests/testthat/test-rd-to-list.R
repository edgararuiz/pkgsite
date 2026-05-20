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
  rd <- rd_to_list("llm_classify.Rd", test_path("test-pkg"))
  expect_equal(rd$source, "R/llm-classify.R")
})

test_that("Source reference is present", {
  rd <- rd_to_list("llm_use.Rd", test_path("test-pkg"))
  expect_true(any(map_chr(rd$arguments, \(x) x$argument) == "..."))
})

test_that("Extract returns NULL on error", {
  expect_null(rd_extract_text2(notexists))
})
