#' Create a Quarto file that lists the available reference pages
#' @inheritParams rd_to_qmd
#' @export
index_to_qmd <- function(
    pkg = ".",
    template = NULL) {
  pkg_site <- read_quarto()
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
  out <- NULL
  fun_line <- function(x) {
    c(
      x$links,
      "", "",
      paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", x$description),
      ""
    )
  }
  if (is.character(pkg)) pkg <- as_pkgdown(pkg)
  topics <- transpose(pkg$topics)
  ref <- map(topics, reference_links)
  if (!is.null(index)) {
    contents <- index[["contents"]]
    if (is.null(names(contents)[[1]])) {
      ref <- map(contents, \(x) ref[path(x, ext = "Rd")])
      ref <- list_flatten(ref)
    } else {
      out <- map(
        contents$sections,
        \(x){
          ref <- ref[path(x$contents, ext = "Rd")]
          ref <- map(ref, fun_line)
          ref <- reduce(ref, c)
          c("", "", paste0("## ", x$title), "", "", ref)
        }
      )
      out <- list_flatten(out)
      out <- as.character(reduce(out, c))
    }
  }
  if (is.null(out)) {
    out <- map(ref, fun_line)
    out <- as.character(reduce(out, c))
  }
  list("reference" = out)
}

reference_links <- function(x) {
  # Manual fixes of special characters in funs variable
  funcs <- x$funs
  file_out <- path(x$file_out)
  if (length(funcs) == 0) funcs <- x$alias
  funcs <- gsub("&lt;", "<", funcs)
  funcs <- gsub("&gt;", ">", funcs)
  funcs <- paste0("[", funcs, "](", file_out, ")")
  funcs <- paste0(funcs, collapse = " ")
  desc <- x$title
  list(
    links = funcs,
    description = desc
  )
}
