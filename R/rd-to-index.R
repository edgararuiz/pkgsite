#' @export
rd_to_index <- function(
    pkg = ".",
    template = system.file("templates/_index.qmd", package = "pkgsite")) {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  ref_list <- reference_to_list_index(pkg)
  ref_convert <- reference_index_convert(ref_list)
  res <- imap(
    ref_convert,
    \(.x, .y) {
      if (.y == 1) {
        .x
      } else {
        c(" ", paste("##", .y), " ", .x)
      }
    }
  )
  res <- reduce(res, c)
  res <- list("reference" = res)
  template_parse(template, res)
}

reference_index_convert <- function(index_list) {
  out <- map(index_list, \(.x) map(.x, reference_links))
  map(
    out,
    \(.x) {
      c(
        map_chr(
          .x,
          \(.x) paste0(
            .x$links,
            "\n\n&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
            .x$description,
            "\n\n"
          )
        )
      )
    }
  )
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

reference_to_list_index <- function(pkg) {
  if (is.character(pkg)) pkg <- as_pkgdown(pkg)
  pkg_ref <- pkg$meta$reference
  pkg_topics <- pkg$topics
  pkg_topics <- pkg_topics[!pkg_topics$internal, ]
  topics_env <- match_env(pkg_topics)
  if (is.null(pkg_ref)) {
    x <- list(data.frame(contents = pkg_topics$name))
  } else {
    x <- pkg_ref
  }
  sections_list <- map(
    seq_along(x),
    \(.x) {
      ref <- x[[.x]]
      topic_list <- map(
        ref$contents,
        ~ {
          item_numbers <- NULL
          try(
            item_numbers <- eval(parse(text = paste0("`", .x, "`")), topics_env),
            silent = TRUE
          )
          if (is.null(item_numbers)) {
            item_numbers <- eval(parse(text = .x), topics_env)
          }
          item_numbers
        }
      )
      topic_ids <- as.numeric(list_c(topic_list))
      transpose(pkg_topics[topic_ids, ])
    }
  )
  if (!is.null(pkg_ref)) {
    sections_title <- map_chr(pkg_ref, \(.x) .x$title)
    names(sections_list) <- sections_title
  }
  sections_list
}
