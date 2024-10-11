#' @export
write_reference <- function(
    pkg = ".",
    folder = "reference",
    examples = TRUE,
    not_run_examples = FALSE,
    template = system.file("templates/_reference.qmd", package = "pkgsite"),
    index_file = "index.qmd",
    index_template = system.file("templates/_index.qmd", package = "pkgsite")) {
  write_reference_index(
    pkg = pkg,
    folder = folder,
    file = index_file,
    template = index_template
  )
  write_reference_pages(
    pkg = pkg,
    folder = folder,
    examples = examples,
    not_run_examples = not_run_examples,
    template = template
  )
}

#' @export
write_reference_index <- function(
    pkg = ".",
    folder = "reference",
    file = "index.qmd",
    template = system.file("templates/_index.qmd", package = "pkgsite")) {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  try(dir_create(folder), silent = TRUE)
  ref <- path(folder, file)
  try(file_delete(ref), silent = TRUE)
  writeLines(rd_to_index(pkg, template), ref)
  cli_inform(col_green(ref))
}

#' @export
write_reference_pages <- function(
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
      try(file_delete(ref), silent = TRUE)
      writeLines(qmd, ref)
      cli_inform(col_green(ref))
    }
  )
}
