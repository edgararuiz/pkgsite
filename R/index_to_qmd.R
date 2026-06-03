#' Create a Quarto file that lists the available reference pages
#' @inheritParams rd_to_qmd
#' @returns A character vector with the contents of a document that links to
#' the converted Quarto documents.
#' @family Conversion functions
#' @examples
#' library(pkgsite)
#' example_pkg <- system.file("example", package = "pkgsite")
#' index_to_qmd(pkg = example_pkg)
#' @export
index_to_qmd <- function(
  pkg = ".",
  template = NULL,
  quarto_file_path = NULL
) {
  check_quarto_file_path(quarto_file_path)
  quarto_root <- quarto_file_path %||% pkg
  pkg_site <- read_quarto(quarto_root)
  index <- pkg_site[["reference"]][["index"]]
  pkg_template <- system.file("templates/_index.qmd", package = "pkgsite")
  yaml_template <- if (!is.null(index$template)) {
    path(quarto_root, index$template)
  } else {
    NULL
  }
  template <- template %||% yaml_template %||% pkg_template
  out <- reference_index_convert(pkg, index)
  template |>
    template_parse(out) |>
    reduce(c)
}

reference_index_convert <- function(pkg = ".", index = NULL) {
  # This section creates reads the Rd files and extracts their name & description
  man_folder <- path(pkg, "man")
  rd_paths <- dir_ls(man_folder, glob = "*.Rd")
  rd_names <- path_file(rd_paths)
  qmd_names <- path(path_ext_remove(rd_names), ext = "qmd")
  rd_list <- rd_paths |>
    map(rd_to_list_internal) |>
    set_names(qmd_names) |>
    map(
      function(x) {
        seealso <- x[["seealso"]]
        if (!is.null(seealso)) {
          fn_family <- unlist(strsplit(seealso, "\\:"))
          x$family <- fn_family[[1]]
          x
        } else {
          x
        }
      }
    )
  # For Rd's that return NULL because they are internal
  null_rds <- map_lgl(rd_list, is.null)
  rd_list <- rd_list[!null_rds]
  ref_lines <- imap(rd_list, function(x, y) {
    alias <- as.character(x[names(x) == "alias"])
    paren <- "()"
    if (all(alias == "")) {
      # This is in case of non-aliased functions, such as infixes
      alias <- as.character(x[names(x) == "name"])
      paren <- ""
    }
    alias_links <- paste0("[", alias, paren, "](", y, ")")
    c(
      paste0(alias_links, collapse = " "),
      "",
      "",
      paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", x$title),
      ""
    )
  })
  # If there is a custom index in the '_quarto.yml' file, it will match up,
  # to the order in the yaml spec, as well as to add the title for a given
  # section, if one is provided
  ref_names <- path_ext_remove(names(ref_lines))
  if (!is.null(index[["contents"]])) {
    ref_lines <- map(
      index[["contents"]],
      function(x) {
        refs <- map(
          x[["contents"]],
          \(y) ref_lines[ref_names == path_ext_remove(y)]
        )
        c(list(list(title = paste("###", x[["section"]]), "")), refs)
      }
    ) |>
      list_flatten() |>
      reduce(c)
  } else {
    families <- as.character(map(rd_list, function(x) x[["concept"]]))
    families <- families[families != "NULL"]
    tab_families <- table(families)
    unique_fams <- names(tab_families[tab_families > 1])
    if (!is.null(unique_fams)) {
      ref_list <- list()
      for (i in seq_along(rd_list)) {
        rds <- rd_list[[i]]
        rds_family <- rds[["concept"]]
        if (!is.null(rds_family) && rds_family %in% unique_fams) {
          ref_list[[rds_family]] <- c(ref_list[[rds_family]], ref_lines[[i]])
        } else {
          ref_list[["<none>"]] <- c(ref_list[["<none>"]], ref_lines[[i]])
        }
      }
      ref_list <- ref_list[order(names(ref_list))]
      ref_lines <- ref_list |>
        imap(function(x, y) {
          if (y == "<none>") {
            x
          } else {
            c(paste("###", y), "", x)
          }
        })
    }
  }
  ref_lines <- reduce(ref_lines, c)
  list("reference" = ref_lines)
}
