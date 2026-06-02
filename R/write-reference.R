#' Writes the reference pages and index as Quarto files
#'
#' @param pkg Path to the root of the R package. Defaults to `"."`.
#' @param target_folder Path to the folder where the Quarto files will be
#' written. Defaults to `NULL`, which resolves to `"reference/"` (or the
#' `dir` value under `pkgsite > reference` in `_quarto.yml` if set).
#' @param examples Flag that sets the examples code chunk to be evaluated when
#' the Quarto document is rendered
#' @param not_run_examples Flag that sets the "do not run" examples code chunk
#' to be evaluated when the Quarto document is rendered
#' @param template The path to a Quarto file that can be used as the template
#' for all of the resulting reference files. If left NULL, `pkgsite` will use
#' its default template.
#' @param index_file The name assigned to the resulting index Quarto file
#' @param index_template The path to a Quarto file that can be used as the
#' template for the index Quarto file
#' @param quarto_file_path Path to the folder containing `_quarto.yml`. Use
#' when the Quarto project root differs from the package root (e.g. the
#' package lives in a sub-folder of a larger Quarto project). Must be a
#' directory path. Defaults to `NULL`, in which case `pkg` is used.
#' @returns A set of Quarto files written to the specified path based on the
#' number of Rd files in the package, plus an additional one that is the index.
#' @family Quarto file creation
#' @examples
#' \donttest{
#' library(pkgsite)
#' example_pkg <- system.file("example", package = "pkgsite")
#' write_reference(pkg = example_pkg, target_folder = tempdir())
#' }
#' @export
write_reference <- function(
  pkg = ".",
  target_folder = NULL,
  examples = TRUE,
  not_run_examples = NULL,
  template = NULL,
  index_file = NULL,
  index_template = NULL,
  quarto_file_path = NULL
) {
  check_quarto_file_path(quarto_file_path)
  quarto_root <- quarto_file_path %||% pkg
  pkg_site <- read_quarto(quarto_root)
  target_folder <- reference_folder(target_folder, quarto_root)
  examples <- examples %||% pkg_site[["run_examples"]] %||% TRUE
  not_run_examples <- not_run_examples %||%
    pkg_site[["reference"]][["not_run_examples"]] %||%
    FALSE
  ref_tmpl <- pkg_site[["reference"]][["template"]]
  template <- template %||%
    (if (!is.null(ref_tmpl)) path(quarto_root, ref_tmpl) else NULL)
  idx_tmpl <- pkg_site[["reference"]][["index"]][["template"]]
  index_template <- index_template %||%
    (if (!is.null(idx_tmpl)) path(quarto_root, idx_tmpl) else NULL)
  index_file <- index_file %||%
    pkg_site[["reference"]][["index"]][["file"]] %||%
    "index.qmd"
  write_reference_index(
    pkg = pkg,
    target_folder = target_folder,
    index_file = index_file,
    index_template = index_template,
    quarto_file_path = quarto_file_path
  )
  write_reference_pages(
    pkg = pkg,
    target_folder = target_folder,
    examples = examples,
    not_run_examples = not_run_examples,
    template = template,
    quarto_file_path = quarto_file_path
  )
}

#' Writes the index of the reference pages into a Quarto file
#' @inheritParams write_reference
#' @returns A Quarto file that links to the individual Quarto documentation
#' files
#' @family Quarto file creation
#' @examples
#' \donttest{
#' library(pkgsite)
#' example_pkg <- system.file("example", package = "pkgsite")
#' write_reference_index(pkg = example_pkg, target_folder = tempdir())
#' }
#' @export
write_reference_index <- function(
  pkg = ".",
  target_folder = NULL,
  index_file = "index.qmd",
  index_template = NULL,
  quarto_file_path = NULL
) {
  check_quarto_file_path(quarto_file_path)
  target_folder <- reference_folder(target_folder, quarto_file_path %||% pkg)
  try(dir_create(target_folder), silent = TRUE)
  ref <- path(target_folder, index_file)
  try(file_delete(ref), silent = TRUE)
  writeLines(index_to_qmd(pkg, index_template, quarto_file_path), ref)
  cli_h3("{.pkg pkgsite}")
  cli_inform("{.emph Creating index file:}")
  cli_bullets(c(" " = "{.code {as.character(ref)}}"))
}

#' Converts the 'Rd' file into Quarto, and writes the file to a specified folder
#' @inheritParams write_reference
#' @returns A set of Quarto files written to the specified path based on the
#' number of Rd files in the package.
#' @family Quarto file creation
#' @examples
#' \donttest{
#' library(pkgsite)
#' example_pkg <- system.file("example", package = "pkgsite")
#' write_reference_pages(pkg = example_pkg, target_folder = tempdir())
#' }
#' @export
write_reference_pages <- function(
  pkg = ".",
  target_folder = NULL,
  examples = TRUE,
  not_run_examples = FALSE,
  template = NULL,
  quarto_file_path = NULL
) {
  check_quarto_file_path(quarto_file_path)
  target_folder <- reference_folder(target_folder, quarto_file_path %||% pkg)
  man_folder <- path(pkg, "man")
  cli_inform("{.emph Converting .Rd to .qmd:}")
  if (l10n_info()$`UTF-8` && !is_latex_output()) {
    arrow <- "\u2192"
  } else {
    arrow <- "->"
  }
  walk(
    path_file(dir_ls(man_folder, glob = "*.Rd")),
    function(x) {
      ref <- paste0(target_folder, "/", path_ext_remove(x), ".qmd")
      qmd <- rd_to_qmd(
        path(man_folder, x),
        pkg = pkg,
        examples = examples,
        not_run_examples = not_run_examples,
        template = template,
        quarto_file_path = quarto_file_path
      )
      if (is.null(qmd)) {
        cli_bullets(c(
          " " = "{.code {as.character(path(man_folder, x))}} {arrow} {.emph Skipped - Internal}"
        ))
      } else {
        try(file_delete(ref), silent = TRUE)
        writeLines(qmd, ref)
        cli_bullets(c(
          " " = "{.code {as.character(path(man_folder, x))}} {arrow} {.code {ref}}"
        ))
      }
    }
  )
}

reference_folder <- function(target_folder, quarto_root) {
  target_folder %||%
    read_quarto(quarto_root)[["reference"]][["dir"]] %||%
    "reference"
}

check_quarto_file_path <- function(quarto_file_path) {
  if (!is.null(quarto_file_path) && !is_dir(quarto_file_path)) {
    cli_abort(
      "{.arg quarto_file_path} must be a directory path, not a file path."
    )
  }
}

# Copied from `r-lib/cli`
is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
}
