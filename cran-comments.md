## Resubmission

- This is a new package.

- Examples for unexported functions: removed `\examples{}` blocks from all
  `.Rd` fixture files in `tests/testthat/` that referenced unexported functions.

- `\dontrun{}` replaced with `\donttest{}` in all three exported functions.

- Writing to user's home filespace: `target_folder` now defaults to `NULL`
  and resolves at runtime from `_quarto.yml` or falls back to `"reference/"`.
  All examples write to `tempdir()`.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
