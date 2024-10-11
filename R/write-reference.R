#' @export
write_reference_index <- function(
    pkg = ".",
    folder = "reference",
    file = "index.qmd",
    template = system.file("templates/_index.qmd", package = "pkgsite")) {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  try(dir_create(folder))
  ref <- path(folder, file)
  try(file_delete(ref))
  writeLines(reference_index(pkg, template), ref)
  cli_inform(col_green(ref))
}

#' @export
write_reference <- function(
    pkg = ".",
    folder = "reference",
    examples = TRUE,
    not_run_examples = FALSE,
    template = system.file("templates/_reference.qmd", package = "pkgsite")) {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  walk(
    pkg$topics$file_in,
    \(x) {
      ref <- paste0(folder, "/", path_ext_remove(x), ".qmd")
      qmd <- rd_to_qmd(x, pkg, examples, not_run_examples, template)
      try(file_delete(ref))
      writeLines(qmd, ref)
      cli_inform(col_green(ref))
    }
  )
}
