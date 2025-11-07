#' Create a Quarto file that lists the available reference pages
#' @inheritParams rd_to_qmd
#' @returns A character vector with the resulting contents creating document
#' that links to the converted Quarto documents.
#' @family Conversion functions
#' @export
index_to_qmd <- function(
  project = ".",
  pkg = NULL,
  template = NULL
) {
  pkg_site <- read_quarto(project)
  index <- NULL
  yaml_template <- NULL
  index <- pkg_site[["reference"]][["index"]]
  yaml_template <- index$template
  pkg_template <- system.file("templates/_index.qmd", package = "pkgsite")
  template <- template %||% yaml_template %||% pkg_template
  out <- reference_index_convert(project, pkg, index)
  out <- template_parse(template, out)
  reduce(out, c)
}

reference_index_convert <- function(project, pkg = NULL, index = NULL) {
  # This section creates reads the Rd files and extracts their name & description
  pkg <- pkg %||% ""
  rd_names <- path_file(dir_ls(path(project, pkg, "man"), glob = "*.Rd"))
  rd_list <- map(rd_names, rd_to_list, project, pkg)
  qmd_names <- path(path_ext_remove(rd_names), ext = "qmd")
  rd_list <- set_names(rd_list, qmd_names)
  rd_list <- map(
    rd_list,
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
  ref_lines <- imap(rd_list, \(x, y) {
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
      "", "",
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
      \(x) {
        refs <- map(
          x[["contents"]],
          \(y) {
            ref_lines[ref_names == path_ext_remove(y)]
          }
        )
        c(list(list(title = paste("###", x[["section"]]), "")), refs)
      }
    )
    ref_lines <- list_flatten(ref_lines)
    ref_lines <- reduce(ref_lines, c)
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
      ref_lines <- imap(ref_list, \(x, y) {
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
