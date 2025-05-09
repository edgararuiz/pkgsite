#' Converts a given 'Rd' file into a list object
#' @description This function is meant to be used as a intermediate object
#' that could be used as an easy way to convert the information inside the 'Rd'
#' into other formats or outputs
#' @inheritParams rd_to_qmd
#' @export
rd_to_list <- function(rd_file, project = ".", pkg = NULL) {
  rd_content <- tools::parse_Rd(fs::path(project, pkg, "man", rd_file))
  out <- map(rd_content, rd_tag_process)
  out <- keep(out, \(x) !is.null(x))
  list_flatten(out)
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
      usage <- as.character(x)
      usage <- gsub("\n", "", usage)
      tag_text <- list(usage)
    } else if (tag_name == "examples") {
      rd_tags <- map_chr(x, attr, "Rd_tag")
      run <- "code_run"
      if ("\\donttest" %in% rd_tags | "\\dontrun" %in% rd_tags) {
        run <- "code_dont_run"
      }
      tag_text <- map(x, as.character)
      tag_text <- reduce(tag_text, c)
      tag_text <- list(paste0(tag_text, collapse = ""))
      tag_text <- set_names(tag_text, run)
      tag_text <- list(tag_text)
    } else if (tag_name == "section") {
      tag_text <- list(list(
        title = rd_extract_text(x[[1]]),
        contents = rd_extract_text(x[[2]])
      ))
    } else {
      tag_text <- list(trimws(rd_extract_text(x)))
    }

    out <- set_names(tag_text, tag_name)
  }
  out
}

rd_args_process <- function(x) {
  out <- map(x, \(x) {
    name <- rd_extract_text(x[1])
    val <- rd_extract_text(x[2])
    if (name != "") {
      out <- list(argument = name, description = val)
    } else {
      out <- NULL
    }
    out
  })
  keep(out, \(x) !is.null(x))
}

rd_extract_text <- function(x, collapse = TRUE) {
  attributes(x) <- NULL
  class(x) <- "Rd"
  rd_text <- as.character(x)
  if (collapse) {
    rd_text <- paste0(as.character(x), collapse = "")
  }
  temp_rd <- tempfile(fileext = ".Rd")
  writeLines(rd_text, temp_rd)
  suppressWarnings(
    rd_txt <- capture.output(tools::Rd2txt(temp_rd, fragment = TRUE))
  )
  if (collapse) {
    rd_txt <- paste0(rd_txt, collapse = " ")
  }
  rd_txt <- gsub("\U2018", "`", rd_txt)
  rd_txt <- gsub("\U2019", "`", rd_txt)
  rd_txt
}
