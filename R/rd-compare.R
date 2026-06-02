rd_compare_titles <- function(path, project = ".", pkg = NULL) {
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html))
  suppressWarnings(tools::Rd2HTML(path, out = tmp_html))
  html_text <- paste(readLines(tmp_html), collapse = "\n")

  h3 <- regmatches(
    html_text,
    gregexpr("(?<=<h3>)[^<]+(?=</h3>)", html_text, perl = TRUE)
  )[[1]]
  html_names <- rd_normalize_header(h3)

  rd_list <- rd_to_list_internal(path)

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

rd_compare_pkg_titles <- function(pkg = ".") {
  man_folder <- fs::path(pkg, "man")
  rd_paths <- fs::dir_ls(man_folder, glob = "*.Rd")
  purrr::walk(rd_paths, function(x) {
    result <- rd_compare_titles(x)
    missing <- result$in_html_not_list
    name <- fs::path_file(x)
    if (length(missing) == 0) {
      cli::cli_inform(c("v" = "{name}"))
    } else {
      cli::cli_inform(c("x" = "{name}: missing {.val {missing}}"))
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
