test_that("lm() to list works", {
  db <- tools::Rd_db("stats")
  lm_rd <- db[["lm.Rd"]]
  expect_snapshot(names(rd_to_list(lm_rd)))
})
