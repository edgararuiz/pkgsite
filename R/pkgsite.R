#' @importFrom rlang env empty_env env_bind
#' @import pkgdown
#' @import purrr
#' @import cli
#' @import fs
#' @import yaml

read_quarto <- function() {
  quarto <- read_yaml("_quarto.yml")
  quarto$pkgsite
}
