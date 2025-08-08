#' @importFrom rlang env empty_env env_bind
#' @importFrom utils capture.output
#' @import purrr
#' @import cli
#' @import fs
#' @import yaml

read_quarto <- function(pkg = ".", fail = FALSE) {
  if (inherits(pkg, "pkgdown")) {
    folder <- pkg$src_path
  } else {
    folder <- pkg
  }
  quarto_file <- path(folder, "_quarto.yml")
  if (!file_exists(quarto_file) && fail) {
    cli_abort("'_quarto.yml' file not found")
  }

  suppressWarnings(
    quarto <- try(read_yaml(quarto_file), silent = TRUE)
  )
  if (inherits(quarto, "try-error")) {
    if (fail) {
      cli_abort("'_quarto.yml' file could not be read")
    }
    out <- NULL
  } else {
    out <- quarto$pkgsite
  }
  out
}
