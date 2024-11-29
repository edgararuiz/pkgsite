#' Create a Quarto file that lists the available reference pages
#' @inheritParams rd_to_qmd
#' @export
index_to_qmd <- function(
    pkg = ".",
    template = NULL) {
  pkg_site <- read_quarto(pkg)
  index <- NULL
  yaml_template <- NULL
  if (!is.null(pkg_site)) {
    index <- pkg_site[["reference"]][["index"]]
    yaml_template <- index$template
  }
  pkg_template <- system.file("templates/_index.qmd", package = "pkgsite")
  template <- template %||% yaml_template %||% pkg_template
  out <- reference_index_convert(pkg, index)
  out <- template_parse(template, out)
  reduce(out, c)
}

reference_index_convert <- function(pkg, index = NULL) {
  rd_names <- path_file(dir_ls(path(pkg, "man"), glob = "*.Rd"))
  qmd_names <- path(path_ext_remove(rd_names), ext = "qmd")
  rd_list <- map(rd_names, rd_to_list, pkg)
  rd_list <- set_names(rd_list, qmd_names)

  ref_lines <- imap(rd_list, \(x, y) {
    alias <- as.character(x[names(x) == "alias"])
    alias_links <- paste0("[", alias, "()](", y, ")")
    c(
      paste0(alias_links, collapse = " "),
      "", "",
      paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", x$title),
      ""
    )
  })
  if (!is.null(index)) {
    contents <- index[["contents"]]
    ref_lines <- map(contents, \(x) {
      if (!is.null(x[["section"]])) {
        refs <- map(x[["contents"]], \(y) ref_lines[names(ref_lines) == y])
        out <- c(list(list(title = paste("##", x$section), "")), refs)
      }
    })
    ref_lines <- list_flatten(ref_lines)
    ref_lines <- reduce(ref_lines, c)
  }
  ref_lines <- reduce(ref_lines, c)
  list("reference" = ref_lines)
}
