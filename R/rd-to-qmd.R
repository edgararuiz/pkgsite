#' Converts 'Rd' to Quarto files
#' @param path The path to the source Rd file
#' @inheritParams write_reference
#' @returns A character vector with the resulting contents of converting the
#' Rd file format into a Quarto file format.
#' @family Conversion functions
#' @examples
#' library(pkgsite)
#' example_pkg <- system.file("example", package = "pkgsite")
#' rd_to_qmd(file.path(example_pkg, "man", "rd_to_qmd.Rd"), pkg = example_pkg)
#' @export
rd_to_qmd <- function(
  path,
  pkg = ".",
  examples = TRUE,
  not_run_examples = FALSE,
  template = NULL,
  quarto_file_path = NULL
) {
  check_quarto_file_path(quarto_file_path)
  quarto_root <- quarto_file_path %||% pkg
  pkg_site <- read_quarto(quarto_root)
  yaml_template <- NULL
  if (!is.null(pkg_site)) {
    ref_template <- pkg_site[["reference"]][["template"]]
    yaml_template <- if (!is.null(ref_template)) {
      path(quarto_root, ref_template)
    } else {
      NULL
    }
  }
  pkg_template <- system.file("templates/_reference.qmd", package = "pkgsite")
  template <- template %||% yaml_template %||% pkg_template
  parsed <- rd_to_list_internal(path, internal = FALSE)
  if (is.null(parsed)) {
    return(NULL)
  }
  con <- reference_convert(parsed, examples, not_run_examples)
  template |>
    template_parse(con) |>
    discard(is.null) |>
    list_flatten() |>
    list_c() |>
    as.character()
}

template_parse <- function(template, con) {
  template |>
    readLines() |>
    map(parse_line_tag, con)
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

    parsed <- start_half |>
      strsplit(end_tag) |>
      list_c()

    pm <- parsed |>
      map(
        function(.x) {
          yes_title <- substr(.x, 1, 6) == "title."
          yes_notitle <- substr(.x, 1, 8) == "notitle."
          if (yes_title | yes_notitle) {
            start_sub <- ifelse(yes_title, 7, 9)
            tag <- .x |>
              substr(start_sub, nchar(.x)) |>
              trimws()
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
        }
      ) |>
      transpose()

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
            if (tag_name_label != "Section") {
              tag_content <- c(paste0("## ", tag_name_label), "", tag_content)
            }
          }
        }
      }
    }
    tag_content
  } else {
    line
  }
}

reference_convert <- function(x, examples = TRUE, not_run_examples = FALSE) {
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
      if (examples && !is.null(curr$eval)) {
        ex_run <- c(ex_run, paste0("#| eval: !expr ", curr$eval))
      }
      if (!is.null(curr$code_run)) {
        out <- c(out, ex_run, curr$code_run, "```")
      }
      if (!is.null(curr$code_dont_run)) {
        out <- c(out, ex_not, curr$code_dont_run, "```")
      }
    }

    if (curr_name == "usage") {
      out <- c("```r", curr, "```")
    }

    if (curr_name == "arguments") {
      out <- reference_arguments(curr)
    }

    if (curr_name == "section") {
      out <- c(out, paste("##", curr$title), "", curr$contents, "")
    }

    if (is.null(out)) {
      out <- curr
      if (is.list(out)) {
        out <- list_c(out)
      }
    }

    out <- list(out)
    names(out) <- curr_name

    res <- c(res, out)
  }
  # An Rd file can have more than one custom section
  sections <- names(res) == "section"
  if (sum(sections) > 1) {
    merged <- list(section = list_c(res[sections]))
    res <- c(res[!sections], merged)
  }
  res
}

# Written as a grid table, because its cells can hold lists and code blocks
reference_arguments <- function(x, width = 60) {
  args <- map(x, \(.x) strwrap(.x[[1]], width = 20))
  descs <- map(x, \(.x) md_wrap_lines(.x[[2]], width))
  w_arg <- max(nchar(c("Arguments", list_c(args)), type = "width"))
  w_desc <- max(nchar(c("Description", list_c(descs)), type = "width"))
  pad <- function(x, w) {
    paste0(x, strrep(" ", w - nchar(x, type = "width")))
  }
  row <- function(a, d) {
    n <- max(length(a), length(d), 1)
    a <- c(a, rep("", n - length(a)))
    d <- c(d, rep("", n - length(d)))
    paste0("| ", pad(a, w_arg), " | ", pad(d, w_desc), " |")
  }
  line <- function(ch) {
    paste0("+", strrep(ch, w_arg + 2), "+", strrep(ch, w_desc + 2), "+")
  }
  body <- map2(args, descs, \(a, d) c(row(a, d), line("-")))
  c(line("-"), row("Arguments", "Description"), line("="), list_c(body))
}

# Wraps the paragraphs and list items of Markdown lines. Code blocks and
# tables are left as they are.
md_wrap_lines <- function(lines, width) {
  if (length(lines) == 0) {
    return("")
  }
  in_code <- FALSE
  out <- list()
  for (l in lines) {
    fence <- grepl("^\\s*```", l)
    if (in_code || fence || grepl("^\\s*\\|", l) || l == "") {
      out <- c(out, list(l))
    } else {
      prefix <- regmatches(l, regexpr("^\\s*([-:]|[0-9]+\\.)?\\s*", l))
      indent <- strrep(" ", nchar(prefix))
      wrapped <- strwrap(substring(l, nchar(prefix) + 1), width - nchar(prefix))
      out <- c(
        out,
        list(paste0(c(prefix, rep(indent, length(wrapped) - 1)), wrapped))
      )
    }
    if (fence) {
      in_code <- !in_code
    }
  }
  list_c(out)
}
