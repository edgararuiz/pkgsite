#' Writes the reference pages and index as Quarto files
#'
#' @param project The path to the root folder of the project.
#' @param pkg The path inside the project folder. Use only if the R package
#' itself is in a sub-folder within the project.
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
  folder <- folder %||% pkg_site[["reference"]][["dir"]] %||% "reference"
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
#' @export
write_reference_index <- function(
  project = ".",
  pkg = NULL,
  folder = "reference",
  index_file = "index.qmd",
  index_template = NULL
) {
  try(dir_create(folder), silent = TRUE)
  ref <- path(folder, index_file)
  try(file_delete(ref), silent = TRUE)
  writeLines(index_to_qmd(project, pkg, index_template), ref)
  cli_h3("{.pkg pkgsite}")
  cli_inform("{.emph Creating index file:}")
  cli_bullets(c(" " = "{.file {ref}}"))
}

#' Converts the 'Rd' file into Quarto, and writes the file to a specified folder
#' @inheritParams write_reference
#' @export
write_reference_pages <- function(
  project = ".",
  pkg = NULL,
  folder = "reference",
  examples = TRUE,
  not_run_examples = FALSE,
  template = NULL
) {
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
    \(x) {
      ref <- paste0(folder, "/", path_ext_remove(x), ".qmd")
      qmd <- rd_to_qmd(x, project, pkg, examples, not_run_examples, template)
      if (is.null(qmd)) {
        cli_bullets(c(" " = "{.file {path(man_folder, x)}} {arrow} {.emph Skipped - Internal}"))
      } else {
        try(file_delete(ref), silent = TRUE)
        writeLines(qmd, ref)
        cli_bullets(c(" " = "{.file {path(man_folder, x)}} {arrow} {.file {ref}}"))
      }
    }
  )
}

# Copied from `r-lib/cli`
is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
}
