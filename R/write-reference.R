#' @export
write_reference_index <- function(
    pkg = ".",
    folder = "reference",
    file = "index.qmd") {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  try(dir_create(folder))
  ref <- path(folder, file)
  try(file_delete(ref))
  writeLines(reference_index(pkg), ref)
  cli_inform(col_green(ref))
}

#' @export
write_reference <- function(pkg = ".", folder = "reference") {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  walk(
    pkg$topics$file_in,
    \(x) {
      ref <- paste0(folder, "/", path_ext_remove(x), ".qmd")
      qmd <- rd_to_qmd(x, pkg)
      try(file_delete(ref))
      writeLines(qmd, ref)
      cli_inform(col_green(ref))
    }
  )
}
