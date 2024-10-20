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
  out <- map(index_list, reference_links)
  out <- map(
    out,
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
