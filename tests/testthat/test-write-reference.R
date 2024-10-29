test_that("write-reference works", {
  temp_reference <- tempfile("/reference")
  expect_message(
    write_reference(test_path("test-pkg"), folder = temp_reference)
    )
  expect_snapshot(
    dir(temp_reference)
  )
  expect_snapshot(
    readLines(fs::path(temp_reference, "index.qmd"))
  )
  expect_snapshot(
    readLines(fs::path(temp_reference, "llm_classify.qmd"))
  )  
})
