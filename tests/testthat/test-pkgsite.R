test_that("Quarto file exceptions", {
  expect_error(read_quarto("/does/not/exists/_quarto.yml", fail = TRUE))
})
