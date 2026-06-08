## Resubmission

- This is a new package

- The `\dontrun{}` flagged in `index_to_qmd.Rd`, `rd_to_list.Rd`, and
  `rd_to_qmd.Rd` may have originated from a fixture R file used in tests
  (`tests/testthat/test-pkg/R/llm-extract.R`). Replaced `\dontrun{}` with
  `\donttest{}` in that file.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
