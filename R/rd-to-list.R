#' Converts a given 'Rd' file into a list object
#' @description This function is meant to be used as a intermediate object
#' that could be used as an easy way to convert the information inside the 'Rd'
#' into other formats or outputs.
#' @inheritParams rd_to_qmd
#' @returns A list object that contains the contents of the Rd file
#' @family Conversion functions
#' @export
rd_to_list <- function(rd_file, project = ".", pkg = NULL) {
  pkg <- pkg %||% ""
  if (inherits(rd_file, "Rd")) {
    rd_content <- rd_file
  } else {
    rd_content <- tools::parse_Rd(fs::path(project, pkg, "man", rd_file))
  }
  out <- map(rd_content, rd_tag_process)
  out <- keep(out, \(x) !is.null(x))
  out <- list_flatten(out)
  if (any(as.character(out["keyword"]) == "internal")) {
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
    } else if (tag_name == "alias") {
      tag_text <- list(rd_extract_text(x))
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
        title = as.character(x[[1]]),
        contents = rd_extract_text2(x)
      ))
    } else {
      tag_text <- list(rd_extract_text2(x))
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
  rd_tag <- attr(x, "Rd_tag") %||% ""
  if (rd_tag == "\\dots") {
    return(" ...")
  }
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

rd_extract_text2 <- function(x, collapse = TRUE, trim = "full") {
  rd_txt <- try(capture.output(tools::Rd2txt(list(x), fragment = TRUE)), silent = TRUE)
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
  if (rd_txt[[1]] == "") rd_txt <- rd_txt[2:length(rd_txt)]
  if (rd_txt[[length(rd_txt)]] == "") rd_txt <- rd_txt[1:length(rd_txt) - 1]
  if (collapse) {
    rd_txt[rd_txt == ""] <- "xxxx"
    rd_txt <- paste(rd_txt, collapse = " ")
    rd_txt <- unlist(strsplit(rd_txt, "xx"))
  }
  rd_txt
}
