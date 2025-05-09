#' Create a Quarto file that lists the available reference pages
#' @inheritParams rd_to_qmd
#' @export
index_to_qmd <- function(
    project = ".",
    pkg = NULL,
    template = NULL) {
  pkg_site <- read_quarto(project)
  index <- NULL
  yaml_template <- NULL
  #TODO: This code does not look right. Not sure why we're checking
  #again
  if (!is.null(pkg_site)) {
    index <- pkg_site[["reference"]][["index"]]
    yaml_template <- index$template
  }
  # ---------------------------------
  pkg_template <- system.file("templates/_index.qmd", package = "pkgsite")
  template <- template %||% yaml_template %||% pkg_template
  out <- reference_index_convert(project, pkg, index)
  out <- template_parse(template, out)
  reduce(out, c)
}

reference_index_convert <- function(project, pkg, index = NULL) {
  rd_names <- path_file(dir_ls(path(project, pkg, "man"), glob = "*.Rd"))
  qmd_names <- path(path_ext_remove(rd_names), ext = "qmd")
  rd_list <- map(rd_names, rd_to_list, project, pkg)
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
    contents <- index[["contents"]][["sections"]]
    ref_lines <- map(contents, \(x) {
      refs <- map(x[["contents"]], \(y) {
        ref_names <- path_ext_remove(names(ref_lines))
        ref_lines[ref_names == y]
      })
      out <- c(list(list(title = paste("##", x[["title"]]), "")), refs)
    })
    ref_lines <- list_flatten(ref_lines)
    ref_lines <- reduce(ref_lines, c)
  }
  ref_lines <- reduce(ref_lines, c)
  list("reference" = ref_lines)
}
