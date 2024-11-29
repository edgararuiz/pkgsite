#' @importFrom rlang env empty_env env_bind
#' @importFrom utils capture.output
#' @import purrr
#' @import cli
#' @import fs
#' @import yaml

read_quarto <- function(pkg = ".") {
  if (inherits(pkg, "pkgdown")) {
    folder <- pkg$src_path
  } else {
    folder <- pkg
  }
  suppressWarnings(
    quarto <- try(read_yaml(path(folder, "_quarto.yml")), silent = TRUE)
  )
  if (inherits(quarto, "try-error")) {
    out <- NULL
  } else {
    out <- quarto$pkgsite
  }
  out
}
