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
  rd_list <- map(rd_names, rd_to_list, pkg)
  rd_list <- set_names(rd_list, rd_names)
  
  ref_lines <- imap(rd_list, \(x, y) {
    qmd_name <- path(path_ext_remove(y), ext = "qmd")
    alias <- as.character(x[names(x) == "alias"])
    alias_links <- paste0("[", alias, "()](", qmd_name,")")
    c(
      paste0(alias_links, collapse = " "),
      "", "", 
      paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", x$title),
      ""
    )
  })
  ref_names <- path_ext_remove(names(ref_lines))
  ref_lines <- set_names(ref_lines, ref_names)
  
  if(!is.null(index)) {
    sections <- index[["contents"]][["sections"]]
    if(!is.null(sections)) {
      ref_lines <- map(contents, \(x) {
        refs <- map(x$contents, \(y) ref_lines[names(ref_lines) == y])
        c(list(list(title = paste0("##", x$title), "")), refs)
      })
      ref_lines <- list_flatten(ref_lines)
      ref_lines <- reduce(ref_lines, c)
    }
  }
  ref_lines <- reduce(ref_lines, c)
  list("reference" = ref_lines)
}
