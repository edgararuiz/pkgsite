#' @export
rd_to_index <- function(
    pkg = ".",
    template = system.file("templates/_index.qmd", package = "pkgsite")) {
  if (is.character(pkg)) pkg <- as_pkgdown(pkg)
  topics <- transpose(pkg$topics)
  res <- reference_index_convert(topics)
  res <- list("reference" = res)
  res <- template_parse(template, res)
  reduce(res, c)
}

reference_index_convert <- function(index_list) {
  pkg_site <- read_quarto()
  ref <- map(index_list, reference_links)
  if(!is.null(pkg_site)) {
    reference_index <- pkg_site[["reference-index"]]
    if(is.null(names(reference_index)[[1]])) {
      ref <- map(reference_index, \(x) ref[path(x, ext = "Rd")])
      ref <- list_flatten(ref)
      ref <- set_names(ref, reference_index)
    }
  }
  out <- map(
    ref,
    \(x) {
      c(
        x$links,
        "", "",
        paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", x$description),
        ""
      )
    }
  )
  as.character(reduce(out, c))
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
