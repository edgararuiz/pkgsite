#' Converts a given 'Rd' file into a list object
#' @description This function is meant to be used as a intermediate object
#' that could be used as an easy way to convert the information inside the 'Rd'
#' into other formats or outputs
#' @inheritParams rd_to_qmd
#' @export
rd_to_list <- function(rd_file, pkg = ".") {
  rd_content <- tools::parse_Rd(fs::path(pkg, "man", rd_file))
  out <- map(rd_content, rd_tag_process) 
  out <- keep(out, \(x) !is.null(x)) 
  list_flatten(out) 
}

rd_tag_process <- function(x) {
  out <- list()
  tag_name <- attr(x, "Rd_tag")
  x_str <- as.character(x)[[1]]
  source_prefix <- "% Please edit documentation in "
  if(grepl(source_prefix, x_str)) {
    x_str <- substr(x_str, nchar(source_prefix), nchar(x_str))
    out <- list(source = trimws(x_str))
  }
  if(grepl("\\\\", tag_name)) {
    tag_name <- substr(tag_name, 2, nchar(tag_name))
    if(tag_name == "arguments") {
      tag_text <- list(rd_args_process(x))
    } else if(tag_name == "usage") {
      usage <- as.character(x)
      usage <- gsub("\n", "", usage)
      tag_text <- list(usage)
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
    if(name != "") {
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
  rd_txt <- gsub("‘", "`", rd_txt)
  rd_txt <- gsub("’", "`", rd_txt)
  rd_txt
}
