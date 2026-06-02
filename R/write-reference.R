#' Writes the reference pages and index as Quarto files
#'
#' @param project The path to the root folder of the project.
#' @param pkg The path inside the project folder. Use only if the R package
#' itself is in a sub-folder within the project.
#' @param folder The target folder to save the new Quarto files to
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
#' @returns A set of Quarto files written to the specified path based on the
#' number of Rd files in the package, plus an additional one that is the index.
#' @family Quarto file creation
#' @examples
#' \donttest{
#' library(pkgsite)
#' example_pkg <- system.file("example", package = "pkgsite")
#' write_reference(project = example_pkg, folder = tempdir())
#' }
#' @export
write_reference <- function(
  project = ".",
  pkg = NULL,
  folder = NULL,
  examples = TRUE,
  not_run_examples = NULL,
  template = NULL,
  index_file = NULL,
  index_template = NULL
) {
  pkg_site <- read_quarto(project)
  pkg <- pkg %||% pkg_site[["dir"]]
  folder <- reference_folder(folder, project)
  examples <- examples %||% pkg_site[["run_examples"]] %||% TRUE
  not_run_examples <- not_run_examples %||%
    pkg_site[["reference"]][["not_run_examples"]] %||%
    FALSE
  template <- template %||% pkg_site[["reference"]][["template"]]
  index_template <- index_template %||%
    pkg_site[["reference"]][["index"]][["template"]]
  index_file <- index_file %||%
    pkg_site[["reference"]][["index"]][["file"]] %||%
    "index.qmd"
  write_reference_index(
    project = project,
    pkg = pkg,
    folder = folder,
    index_file = index_file,
    index_template = index_template
  )
  write_reference_pages(
    project = project,
    pkg = pkg,
    folder = folder,
    examples = examples,
    not_run_examples = not_run_examples,
    template = template
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
#' write_reference_index(project = example_pkg, folder = tempdir())
#' }
#' @export
write_reference_index <- function(
  project = ".",
  pkg = NULL,
  folder = NULL,
  index_file = "index.qmd",
  index_template = NULL
) {
  folder <- reference_folder(folder, project)
  try(dir_create(folder), silent = TRUE)
  ref <- path(folder, index_file)
  try(file_delete(ref), silent = TRUE)
  writeLines(index_to_qmd(project, pkg, index_template), ref)
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
#' write_reference_pages(project = example_pkg, folder = tempdir())
#' }
#' @export
write_reference_pages <- function(
  project = ".",
  pkg = NULL,
  folder = NULL,
  examples = TRUE,
  not_run_examples = FALSE,
  template = NULL
) {
  folder <- reference_folder(folder, project)
  pkg <- pkg %||% ""
  man_folder <- path(project, pkg, "man")
  cli_inform("{.emph Converting .Rd to .qmd:}")
  if (l10n_info()$`UTF-8` && !is_latex_output()) {
    arrow <- "\u2192"
  } else {
    arrow <- "->"
  }
  walk(
    path_file(dir_ls(man_folder, glob = "*.Rd")),
    function(x) {
      ref <- paste0(folder, "/", path_ext_remove(x), ".qmd")
      qmd <- rd_to_qmd(x, project, pkg, examples, not_run_examples, template)
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

reference_folder <- function(folder, project) {
  folder %||% read_quarto(project)[["reference"]][["dir"]] %||% "reference"
}

# Copied from `r-lib/cli`
is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
}
