#' @importFrom rlang env empty_env env_bind
#' @import pkgdown
#' @import purrr
#' @import cli
#' @import fs
#' @import yaml

read_quarto <- function() {
  suppressWarnings(
    quarto <- try(read_yaml("_quarto.yml"), silent = TRUE)  
  )
  if(inherits(quarto, "try-error")) {
    out <- NULL
  } else {
    out <- quarto$pkgsite  
  }
  out
}
