#' Converts 'Rd' to Quarto files
#' @param file_in The name of the source Rd file
#' @inheritParams write_reference
#' @export
rd_to_qmd <- function(
    file_in,
    pkg = ".",
    examples = TRUE,
    not_run_examples = FALSE,
    template = NULL) {
  if (is.character(pkg)) pkg <- pkgdown::as_pkgdown(pkg)
  pkg_site <- read_quarto()
  yaml_template <- NULL
  if (!is.null(pkg_site)) {
    reference <- pkg_site[["reference"]]
    yaml_template <- reference$template
  }
  pkg_template <- system.file("templates/_reference.qmd", package = "pkgsite")
  template <- template %||% yaml_template %||% pkg_template
  parsed <- rd_to_list(file_in, pkg)
  con <- reference_convert(parsed, examples, not_run_examples)
  out <- template_parse(template, con)
  out <- discard(out, is.null)
  out <- list_flatten(out)
  out <- list_c(out)
  as.character(out)
}

template_parse <- function(template, con) {
  read_template <- readLines(template)
  map(read_template, parse_line_tag, con)
}

parse_line_tag <- function(line, con) {
  start_tag <- "\\{\\{\\{\\{"
  end_tag <- "\\}\\}\\}\\}"

  tr <- c(
    "seealso" = "See Also",
    "author" = "Author(s)"
  )

  if (grepl(start_tag, line)) {
    start_half <- strsplit(line, start_tag)[[1]]

    parsed <- list_c(strsplit(start_half, end_tag))

    pm <- map(parsed, ~ {
      yes_title <- substr(.x, 1, 6) == "title."
      yes_notitle <- substr(.x, 1, 8) == "notitle."
      if (yes_title | yes_notitle) {
        start_sub <- ifelse(yes_title, 7, 9)
        tag <- substr(.x, start_sub, nchar(.x))
        tag <- trimws(tag)
        x <- con[[tag]]
      } else {
        x <- .x
        tag <- NULL
      }
      list(
        content = x,
        title = yes_title,
        no_title = yes_notitle,
        no_lines = length(x),
        tag = tag
      )
    })
    pm <- transpose(pm)

    if (all(map_lgl(pm$content, is.null))) {
      tag_content <- NULL
    } else {
      if (any(pm$no_lines > 1)) {
        tag_content <- reduce(pm$content, c)
      } else {
        tag_content <- paste0(pm$content, collapse = "")
      }

      yes_notitle <- any(as.logical(pm$no_title))
      yes_title <- any(as.logical(pm$title))

      if (yes_title && !yes_notitle) {
        tag_names <- as.character(pm$tag)
        tag_names <- tag_names[!is.null(tag_names)]
        if (length(tag_names > 0)) {
          tag_name <- tag_names[[1]]
          tag_name_label <- paste0(
            toupper(substr(tag_name, 1, 1)),
            substr(tag_name, 2, nchar(tag_name))
          )

          tag_match <- names(tr) == tag_name

          if (any(tag_match)) {
            tag_name_label <- tr[tag_match]
          }

          if (!is.null(tag_content)) {
            tag_content <- c(paste0("## ", tag_name_label), tag_content)
          }
        }
      }
    }
    tag_content
  } else {
    line
  }
}

reference_entry <- function(x, title = NULL) {
  out <- NULL
  if (!is.null(title)) title <- paste("##", title)
  if (!is.null(x)) out <- c(title, "", x, "")
  out
}

reference_convert <- function(x,
                              examples = TRUE,
                              not_run_examples = FALSE) {
  res <- list()
  for (i in seq_along(x)) {
    curr <- x[[i]]
    curr_name <- names(x[i])
    out <- NULL

    if (curr_name == "examples") {
      run_examples <- FALSE
      out <- list()
      ex_run <- "```r"
      ex_not <- "```r"
      if (examples) {
        ex_run <- "```{r}"
      }
      if (not_run_examples) {
        ex_not <- "```{r}"
      }
      if (!is.null(curr$code_run)) {
        out <- c(out, ex_run, curr$code_run, "```")
      }
      if (!is.null(curr$code_dont_run)) {
        out <- c(out, ex_not, curr$code_dont_run, "```")
      }
    }

    if (curr_name == "usage") {
      out <- reference_qmd_example(curr, FALSE)
    }

    if (curr_name == "arguments") out <- reference_arguments(curr)

    if (curr_name == "section") {
      out <- map(curr, ~ c(paste("##", .x$title), .x$contents))
      out <- list_c(out)
      out <- reduce(out, function(x, y) c(x, "", y), .init = NULL)
    }

    if (is.null(out)) {
      out <- curr
      if (is.list(out)) out <- list_c(out)
      if (length(out) > 1) out <- reduce(out, function(x, y) c(x, "", y), .init = NULL)
    }

    out <- list(out)
    names(out) <- curr_name

    res <- c(res, out)
  }

  res
}

reference_arguments <- function(x) {
  lines <- map_chr(x, ~ paste0(.x[[1]], " | ", paste0(.x[[2]], collapse = "<br>")))
  rows <- paste0("| ", lines, " |")
  c("|Arguments|Description|", "|---|---|", rows)
}

reference_qmd_example <- function(x, run = FALSE) {
  # x <- x[x != "\n"]
  if (run) {
    out <- c("```{r}", x, "```")
  } else {
    out <- c("```r", x, "```")
  }
}
