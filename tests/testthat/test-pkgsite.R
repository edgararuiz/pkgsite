test_that("Quarto file exceptions", {
  expect_snapshot(
    read_quarto("/does/not/exists/_quarto.yml", fail = TRUE),
    error = TRUE
  )
})
