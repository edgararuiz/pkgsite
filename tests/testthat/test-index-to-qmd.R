test_that("Index parsing works", {
  yaml_file <- read_quarto(test_path("test-pkg2"))
  index <- yaml_file[["reference"]][["index"]]
  expect_snapshot(
    reference_index_convert(
      pkg = path_abs(test_path("test-pkg2")), 
      project = "", 
      index = index
    )
  )
})

