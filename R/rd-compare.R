rd_compare_titles <- function(rd_file, project = ".", pkg = NULL) {
  rd_input <- if (inherits(rd_file, "Rd")) {
    rd_file
  } else {
    fs::path(project, pkg %||% "", "man", rd_file)
  }

  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html))
  suppressWarnings(tools::Rd2HTML(rd_input, out = tmp_html))
  html_text <- paste(readLines(tmp_html), collapse = "\n")

  h3 <- regmatches(
    html_text,
    gregexpr("(?<=<h3>)[^<]+(?=</h3>)", html_text, perl = TRUE)
  )[[1]]
  html_names <- rd_normalize_header(h3)

  rd_list <- rd_to_list_internal(rd_file, project, pkg)

  section_idx <- which(names(rd_list) == "section")
  section_names <- if (length(section_idx) > 0) {
    vapply(
      rd_list[section_idx],
      \(x) rd_normalize_header(x$title),
      character(1)
    )
  } else {
    character(0)
  }

  meta_keys <- c("title", "name", "alias", "keyword", "concept", "source")
  content_names <- names(rd_list)[!names(rd_list) %in% c(meta_keys, "section")]
  list_names <- c(content_names, section_names)

  list(
    html = html_names,
    list = list_names,
    in_html_not_list = setdiff(html_names, list_names),
    in_list_not_html = setdiff(list_names, html_names)
  )
}

rd_compare_pkg_titles <- function(project = ".", pkg = NULL) {
  pkg <- pkg %||% ""
  rd_names <- fs::path_file(fs::dir_ls(
    fs::path(project, pkg, "man"),
    glob = "*.Rd"
  ))
  purrr::walk(rd_names, function(x) {
    result <- rd_compare_titles(x, project, pkg)
    missing <- result$in_html_not_list
    if (length(missing) == 0) {
      cli::cli_inform(c("v" = "{x}"))
    } else {
      cli::cli_inform(c("x" = "{x}: missing {.val {missing}}"))
    }
  })
  invisible()
}

rd_normalize_header <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z]", "", x)
  x <- gsub("^authors$", "author", x)
  x
}
