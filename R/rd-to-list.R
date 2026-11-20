#' Converts a given 'Rd' file into a list object
#' @description This function is meant to be used as an intermediate object
#' that could be used as an easy way to convert the information inside the 'Rd'
#' into other formats or outputs.
#' @param path The path to the source Rd file
#' @returns A list object that contains the contents of the Rd file
#' @family Conversion functions
#' @examples
#' library(pkgsite)
#' example_pkg <- system.file("example", package = "pkgsite")
#' rd_to_list(file.path(example_pkg, "man", "rd_to_list.Rd"))
#' @export
rd_to_list <- function(path) {
  rd_to_list_internal(
    path = path,
    internal = FALSE
  )
}

rd_to_list_internal <- function(path, internal = TRUE) {
  if (inherits(path, "Rd")) {
    rd_content <- path
  } else {
    rd_content <- tools::parse_Rd(path)
  }
  out <- rd_content |>
    map(rd_tag_process) |>
    keep(\(x) !is.null(x)) |>
    list_flatten()
  if (any(as.character(out["keyword"]) == "internal") && !internal) {
    out <- NULL
  }
  out
}

rd_tag_process <- function(x) {
  out <- list()
  tag_name <- attr(x, "Rd_tag")
  x_str <- as.character(x)[[1]]
  source_prefix <- "% Please edit documentation in "
  if (grepl(source_prefix, x_str)) {
    x_str <- substr(x_str, nchar(source_prefix), nchar(x_str))
    out <- list(source = trimws(x_str))
  }
  if (grepl("\\\\", tag_name)) {
    tag_name <- substr(tag_name, 2, nchar(tag_name))
    if (tag_name == "arguments") {
      tag_text <- list(rd_args_process(x))
    } else if (tag_name == "usage") {
      tag_text <- list(rd_extract_text2(x, FALSE, "tab"))
    } else if (tag_name %in% c("alias", "name", "keyword", "concept")) {
      tag_text <- list(trimws(rd_plain(x)))
    } else if (tag_name == "title") {
      tag_text <- list(rd_md_inline(x, escape = FALSE))
    } else if (tag_name == "examples") {
      tag_text <- list(rd_examples_process(x))
    } else if (tag_name == "section") {
      tag_text <- list(list(
        title = rd_md_inline(x[[1]], escape = FALSE),
        contents = rd_md(x[[2]])
      ))
    } else {
      tag_text <- list(rd_md(x))
    }

    out <- set_names(tag_text, tag_name)
  }
  out
}

rd_args_process <- function(x) {
  x |>
    keep(\(x) identical(attr(x, "Rd_tag"), "\\item") && length(x) == 2) |>
    map(
      function(x) {
        list(
          argument = rd_md_inline(x[[1]]),
          description = rd_md(x[[2]])
        )
      }
    ) |>
    discard(\(x) x$argument == "")
}

# roxygen2 wraps `@examplesIf` code in `\dontshow{}` lines. These are removed,
# and the condition is returned in `eval`, so it can be used as a chunk option.
# If there is more than one condition, all of them must be met.
rd_examples_process <- function(x) {
  rd_tags <- map_chr(x, \(x) attr(x, "Rd_tag") %||% "")
  run <- "code_run"
  if ("\\donttest" %in% rd_tags | "\\dontrun" %in% rd_tags) {
    run <- "code_dont_run"
  }
  code <- x |>
    map(as.character) |>
    reduce(c) |>
    paste0(collapse = "")
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  open <- "^\\s*if \\((.*)\\) withAutoprint\\(\\{ # examplesIf\\s*$"
  close <- "^\\s*\\}\\) # examplesIf\\s*$"
  conditions <- unique(sub(open, "\\1", lines[grepl(open, lines)]))
  lines <- lines[!grepl(open, lines) & !grepl(close, lines)]
  out <- list(trimws(paste0(lines, collapse = "\n")))
  names(out) <- run
  if (length(conditions) > 0) {
    out$eval <- paste0(conditions, collapse = " && ")
  }
  out
}

rd_extract_text2 <- function(x, collapse = TRUE, trim = "full") {
  old_opt <- options(useFancyQuotes = TRUE)
  on.exit(options(old_opt))
  rd_txt <- try(
    capture.output(tools::Rd2txt(list(x), fragment = TRUE)),
    silent = TRUE
  )
  if (inherits(rd_txt, "try-error")) {
    return(NULL)
  }
  if (length(rd_txt) == 0) {
    return(as.character(x))
  }
  rd_txt <- gsub("\U2018", "`", rd_txt)
  rd_txt <- gsub("\U2019", "`", rd_txt)
  if (trim == "tab") {
    tabbed <- substr(rd_txt, 1, 5) == "     "
    rd_txt[tabbed] <- substr(rd_txt[tabbed], 6, nchar(rd_txt[tabbed]))
  }
  if (trim == "full") {
    rd_txt <- trimws(rd_txt)
  }
  rd_txt <- rd_txt[2:length(rd_txt)]
  if (rd_txt[[1]] == "") {
    rd_txt <- rd_txt[2:length(rd_txt)]
  }
  if (rd_txt[[length(rd_txt)]] == "") {
    rd_txt <- rd_txt[1:length(rd_txt) - 1]
  }
  if (collapse) {
    rd_txt[rd_txt == ""] <- "xxxx"
    rd_txt <- paste(rd_txt, collapse = " ")
    rd_txt <- unlist(strsplit(rd_txt, "xx"))
  }
  rd_txt
}
