#' Writes the reference pages and index as Quarto files
#'
#' @param pkg The path to the package or a `pkgdown` package object
#' @param folder The target folder to save the new Quarto files to
#' @param examples Flag that sets the examples code chuck to be evaluated when
#' the Quarto document is rendered
#' @param not_run_examples Flag that sets the "do not run" examples code chuck
#' to be evaluated when the Quarto document is rendered
#' @param template The path to a Quarto file that can be used as the template
#' for all of the resulting reference files. If left NULL, `pkgsite` will use
#' its default template.
#' @param index_file The name assigned to the resulting index Quarto file
#' @param index_template The path to a Quarto file that can be used as the
#' template for the index Quarto file
#'
#' @export
write_reference <- function(
    pkg = NULL,
    folder = NULL,
    examples = TRUE,
    not_run_examples = NULL,
    template = NULL,
    index_file = NULL,
    index_template = NULL) {
  pkg_site <- read_quarto(pkg)
  pkg <- pkg %||% pkg_site[["dir"]] %||% "."
  folder <- folder %||% pkg_site[["reference"]][["dir"]] %||% "reference"
  not_run_examples <- not_run_examples %||%
    pkg_site[["reference"]][["not_run_examples"]] %||%
    FALSE
  index_file <- index_file %||%
    pkg_site[["reference"]][["index"]][["file"]] %||%
    "index.qmd"
  write_reference_index(
    pkg = pkg,
    folder = folder,
    index_file = index_file,
    index_template = index_template
  )
  write_reference_pages(
    pkg = pkg,
    folder = folder,
    examples = examples,
    not_run_examples = not_run_examples,
    template = template
  )
}

#' Writes the index of the reference pages into a Quarto file
#' @inheritParams write_reference
#' @export
write_reference_index <- function(
    pkg = ".",
    folder = "reference",
    index_file = "index.qmd",
    index_template = NULL) {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  try(dir_create(folder), silent = TRUE)
  ref <- path(folder, index_file)
  try(file_delete(ref), silent = TRUE)
  writeLines(index_to_qmd(pkg, index_template), ref)
  cli_inform(col_green(ref))
}

#' Converts the 'Rd' file into Quarto, and writes the file to a specified folder
#' @inheritParams write_reference
#' @export
write_reference_pages <- function(
    pkg = ".",
    folder = "reference",
    examples = TRUE,
    not_run_examples = FALSE,
    template = NULL) {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  walk(
    pkg$topics$file_in,
    \(x) {
      ref <- paste0(folder, "/", path_ext_remove(x), ".qmd")
      qmd <- rd_to_qmd(x, pkg, examples, not_run_examples, template)
      try(file_delete(ref), silent = TRUE)
      writeLines(qmd, ref)
      cli_inform(col_green(ref))
    }
  )
}
