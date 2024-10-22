#' @export
rd_to_index <- function(
    pkg = ".",
    template = system.file("templates/_index.qmd", package = "pkgsite")) {
  res <- reference_index_convert(pkg)
  res <- list("reference" = res)
  res <- template_parse(template, res)
  reduce(res, c)
}

reference_index_convert <- function(pkg) {
  out <- NULL
  fun_line <- function(x) {
    c(
      x$links,
      "", "",
      paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", x$description),
      ""
    )
  }
  pkg_site <- read_quarto()
  if (is.character(pkg)) pkg <- as_pkgdown(pkg)
  topics <- transpose(pkg$topics)
  ref <- map(topics, reference_links)
  if (!is.null(pkg_site)) {
    reference_index <- pkg_site[["reference"]][["index"]][["contents"]]
    if (is.null(names(reference_index)[[1]])) {
      ref <- map(reference_index, \(x) ref[path(x, ext = "Rd")])
      ref <- list_flatten(ref)
    } else {
      out <- map(
        reference_index$sections,
        \(x){
          ref <- ref[path(x$contents, ext = "Rd")]
          ref <- map(ref, fun_line)
          ref <- reduce(ref, c)
          c("", "", paste0("## ", x$title), ref)
        }
      )
      out <- list_flatten(out)
      out <- as.character(reduce(out, c))
    }
  } else {
    ref_list <- path_ext_remove(names(ref))
    cli_bullets(paste("-", ref_list))
  }
  if (is.null(out)) {
    out <- map(ref, fun_line)
    out <- as.character(reduce(out, c))
  }
  out
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
