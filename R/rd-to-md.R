# Converts parsed Rd content into Quarto Markdown.
#
# `rd_md()` returns a character vector of lines, with "" between blocks.
# `rd_md_inline()` returns a single line, for table cells and titles. Lists
# become lines separated by `<br>`.
#
# Blocks (lists, tables, code, headings) are rendered on their own, stored in
# `ctx$blocks`, and replaced in the text by a placeholder. This lets the
# surrounding text be re-flowed into paragraphs without breaking the blocks.

rd_md <- function(x, level = 3) {
  ctx <- md_context(level = level)
  md_finish(md_nodes(x, ctx), ctx)
}

rd_md_inline <- function(x, escape = TRUE) {
  ctx <- md_context(oneline = TRUE, escape = escape)
  md_finish_oneline(md_nodes(x, ctx))
}

md_context <- function(level = 3, oneline = FALSE, escape = TRUE) {
  ctx <- new.env(parent = emptyenv())
  ctx$blocks <- list()
  ctx$level <- level
  ctx$oneline <- oneline
  ctx$escape <- escape
  ctx$lang <- NULL
  ctx
}

md_nodes <- function(x, ctx) {
  if (is.character(x)) {
    x <- list(x)
  }
  paste0(map_chr(x, md_node, ctx), collapse = "")
}

md_node <- function(x, ctx) {
  tag <- attr(x, "Rd_tag") %||% ""
  switch(
    tag,
    "TEXT" = ,
    "RCODE" = ,
    "VERB" = md_escape(paste0(x, collapse = ""), ctx),
    "COMMENT" = ,
    "USERMACRO" = ,
    "\\figure" = ,
    "\\newcommand" = ,
    "\\renewcommand" = "",
    "\\code" = ,
    "\\verb" = ,
    "\\samp" = ,
    "\\kbd" = ,
    "\\env" = ,
    "\\option" = ,
    "\\command" = ,
    "\\file" = md_code(rd_plain(x)),
    "\\emph" = ,
    "\\var" = ,
    "\\dfn" = md_wrap(md_nodes(x, ctx), "*"),
    "\\bold" = ,
    "\\strong" = md_wrap(md_nodes(x, ctx), "**"),
    "\\sQuote" = paste0("\u2018", md_nodes(x, ctx), "\u2019"),
    "\\dQuote" = paste0("\u201c", md_nodes(x, ctx), "\u201d"),
    "\\dots" = ,
    "\\ldots" = "...",
    "\\R" = "R",
    "\\cr" = "<br>",
    "\\tab" = " ",
    "\\href" = paste0(
      "[",
      md_nodes(x[[2]], ctx),
      "](",
      trimws(rd_plain(x[[1]])),
      ")"
    ),
    "\\url" = ,
    "\\email" = paste0("<", trimws(rd_plain(x)), ">"),
    "\\enc" = md_nodes(x[[1]], ctx),
    "\\method" = ,
    "\\S3method" = ,
    "\\S4method" = md_code(rd_plain(x[[1]])),
    "\\eqn" = paste0("$", md_squish(rd_plain(x[[1]])), "$"),
    "\\deqn" = md_deqn(x, ctx),
    "\\if" = md_if(x, ctx),
    "\\ifelse" = md_ifelse(x, ctx),
    "\\out" = rd_plain(x),
    "\\Sexpr" = md_sexpr(x, ctx),
    "\\preformatted" = md_preformatted(x, ctx),
    "\\itemize" = md_list(x, ctx, ordered = FALSE),
    "\\enumerate" = md_list(x, ctx, ordered = TRUE),
    "\\describe" = md_describe(x, ctx),
    "\\item" = md_describe(list(x), ctx),
    "\\tabular" = md_tabular(x, ctx),
    "\\subsection" = md_subsection(x, ctx),
    if (is.list(x)) md_nodes(x, ctx) else md_escape(x, ctx)
  )
}

# Text of a node and its children, with no Markdown
rd_plain <- function(x) {
  tag <- attr(x, "Rd_tag") %||% ""
  if (tag %in% c("\\dots", "\\ldots")) {
    return("...")
  }
  if (tag == "\\R") {
    return("R")
  }
  if (tag == "COMMENT") {
    return("")
  }
  if (is.list(x)) {
    paste0(map_chr(x, rd_plain), collapse = "")
  } else {
    paste0(as.character(x), collapse = "")
  }
}

md_escape <- function(x, ctx) {
  if (!ctx$escape) {
    return(x)
  }
  gsub("([\\\\`*$@^~<])", "\\\\\\1", x)
}

md_squish <- function(x) {
  trimws(gsub("\\s+", " ", x))
}

md_code <- function(x) {
  x <- md_squish(x)
  if (x == "") {
    return("")
  }
  ticks <- regmatches(x, gregexpr("`+", x))[[1]]
  n <- if (length(ticks)) max(nchar(ticks)) + 1 else 1
  fence <- strrep("`", n)
  pad <- if (grepl("^`|`$", x)) " " else ""
  paste0(fence, pad, x, pad, fence)
}

md_wrap <- function(x, with) {
  x <- md_squish(x)
  if (x == "") {
    return("")
  }
  paste0(with, x, with)
}

md_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

md_block <- function(text, ctx) {
  ctx$blocks <- c(ctx$blocks, list(text))
  paste0("\n\n\u001a", length(ctx$blocks), "\u001a\n\n")
}

# Turns the rendered text into lines. Text between blocks is joined into
# paragraphs, and the placeholders are swapped for their blocks.
md_finish <- function(txt, ctx) {
  txt <- gsub("[ \t\r]*\n[ \t\r]*\n\\s*", "\u001e", txt, perl = TRUE)
  paras <- strsplit(txt, "\u001e", fixed = TRUE)[[1]]
  paras <- md_squish(gsub("\\s+", " ", paras))
  paras <- paras[paras != ""]
  out <- map_chr(paras, function(p) {
    if (grepl("^\u001a[0-9]+\u001a$", p)) {
      ctx$blocks[[as.integer(gsub("\u001a", "", p))]]
    } else {
      md_escape_start(p)
    }
  })
  if (length(out) == 0) {
    return(character())
  }
  strsplit(paste0(out, collapse = "\n\n"), "\n", fixed = TRUE)[[1]]
}

md_finish_oneline <- function(txt) {
  txt <- gsub("[ \t\r]*\n[ \t\r]*\n\\s*", "<br><br>", txt, perl = TRUE)
  txt <- md_squish(txt)
  txt <- gsub("(<br>\\s*){3,}", "<br><br>", txt)
  txt <- gsub("^(<br>\\s*)+|(\\s*<br>)+$", "", txt)
  txt
}

# Keeps a paragraph from being read as a heading, list or block quote
md_escape_start <- function(x) {
  x <- sub("^([#>:+-])", "\\\\\\1", x)
  sub("^([0-9]+)([.)])( |$)", "\\1\\\\\\2\\3", x)
}

md_indent <- function(lines, first, rest) {
  if (length(lines) == 0) {
    return(first)
  }
  out <- paste0(c(first, rep(rest, length(lines) - 1)), lines)
  sub("^\\s+$", "", out)
}

md_list <- function(x, ctx, ordered) {
  x <- unclass(x)
  is_item <- map_lgl(x, \(n) identical(attr(n, "Rd_tag"), "\\item"))
  items <- split(x, cumsum(is_item))
  items <- items[as.character(unique(cumsum(is_item)[is_item]))]
  items <- map(items, \(i) i[-1])
  # Pandoc drops block HTML, such as <ul>, inside table cells
  if (ctx$oneline) {
    marker <- if (ordered) paste0(seq_along(items), ". ") else "\u2022 "
    li <- paste0("<br>", marker, map_chr(items, rd_md_inline))
    return(paste0(li, collapse = ""))
  }
  marker <- if (ordered) "1. " else "- "
  lines <- items |>
    map(\(i) rd_md(i, level = ctx$level)) |>
    map(\(l) md_indent(l, marker, strrep(" ", nchar(marker)))) |>
    list_c()
  md_block(paste0(lines, collapse = "\n"), ctx)
}

md_describe <- function(x, ctx) {
  items <- keep(
    x,
    \(n) identical(attr(n, "Rd_tag"), "\\item") && length(n) == 2
  )
  if (ctx$oneline) {
    dl <- map_chr(items, function(i) {
      paste0("<br>**", rd_md_inline(i[[1]]), "**: ", rd_md_inline(i[[2]]))
    })
    return(paste0(dl, collapse = ""))
  }
  entries <- map_chr(items, function(i) {
    term <- rd_md_inline(i[[1]])
    desc <- md_indent(rd_md(i[[2]], level = ctx$level), ":   ", "    ")
    paste0(c(term, desc), collapse = "\n")
  })
  md_block(paste0(entries, collapse = "\n\n"), ctx)
}

md_tabular <- function(x, ctx) {
  align <- strsplit(gsub("[^lcr]", "", rd_plain(x[[1]])), "")[[1]]
  rows <- list()
  row <- character()
  cell <- list()
  end_cell <- function() {
    row <<- c(row, gsub("|", "\\|", rd_md_inline(cell), fixed = TRUE))
    cell <<- list()
  }
  for (n in x[[2]]) {
    tag <- attr(n, "Rd_tag") %||% ""
    if (tag == "\\tab") {
      end_cell()
    } else if (tag == "\\cr") {
      end_cell()
      rows <- c(rows, list(row))
      row <- character()
    } else {
      cell <- c(cell, list(n))
    }
  }
  end_cell()
  if (any(row != "")) {
    rows <- c(rows, list(row))
  }
  if (length(rows) == 0) {
    return("")
  }
  ncol <- max(length(align), lengths(rows))
  rows <- map(rows, \(r) c(r, rep("", ncol - length(r))))
  if (ctx$oneline) {
    return(paste0(
      map_chr(rows, \(r) paste0(r, collapse = " ")),
      collapse = "<br>"
    ))
  }
  align <- c(align, rep("l", ncol - length(align)))[seq_len(ncol)]
  sep <- c(l = ":---", c = ":---:", r = "---:")[align]
  lines <- c(
    paste0("| ", paste0(rows[[1]], collapse = " | "), " |"),
    paste0("|", paste0(sep, collapse = "|"), "|"),
    map_chr(rows[-1], \(r) paste0("| ", paste0(r, collapse = " | "), " |"))
  )
  md_block(paste0(lines, collapse = "\n"), ctx)
}

md_subsection <- function(x, ctx) {
  title <- rd_md_inline(x[[1]])
  if (ctx$oneline) {
    return(paste0("<br><br>**", title, "**<br><br>", md_nodes(x[[2]], ctx)))
  }
  heading <- md_block(paste(strrep("#", ctx$level), title), ctx)
  ctx$level <- ctx$level + 1
  on.exit(ctx$level <- ctx$level - 1)
  paste0(heading, md_nodes(x[[2]], ctx), "\n\n")
}

md_preformatted <- function(x, ctx) {
  code <- sub("^\n", "", sub("\n+$", "", rd_plain(x)))
  lang <- ctx$lang %||% ""
  ctx$lang <- NULL
  if (ctx$oneline) {
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    return(paste0(map_chr(lines, md_code), collapse = "<br>"))
  }
  ticks <- regmatches(code, gregexpr("`{3,}", code))[[1]]
  n <- if (length(ticks)) max(nchar(ticks)) + 1 else 3
  fence <- strrep("`", n)
  md_block(paste0(fence, lang, "\n", code, "\n", fence), ctx)
}

md_deqn <- function(x, ctx) {
  eqn <- trimws(rd_plain(x[[1]]))
  if (ctx$oneline) {
    return(paste0("$$", md_squish(eqn), "$$"))
  }
  md_block(paste0("$$\n", eqn, "\n$$"), ctx)
}

md_is_html <- function(format) {
  any(trimws(strsplit(rd_plain(format), ",")[[1]]) == "html")
}

md_if <- function(x, ctx) {
  if (!md_is_html(x[[1]])) {
    return("")
  }
  # roxygen2 wraps code blocks in a div that names the language
  content <- trimws(rd_plain(x[[2]]))
  div <- "^<div class=\"sourceCode ?([^\"]*)\">$"
  if (grepl(div, content)) {
    ctx$lang <- sub(div, "\\1", content)
    return("")
  }
  if (content == "</div>") {
    return("")
  }
  md_nodes(x[[2]], ctx)
}

md_ifelse <- function(x, ctx) {
  has_figure <- function(n) {
    if (identical(attr(n, "Rd_tag"), "\\figure")) {
      return(TRUE)
    }
    is.list(n) && any(map_lgl(n, has_figure))
  }
  # Use the HTML branch, unless it is only an image, such as a lifecycle badge
  if (md_is_html(x[[1]]) && !has_figure(x[[2]])) {
    md_nodes(x[[2]], ctx)
  } else {
    md_nodes(x[[3]], ctx)
  }
}

md_sexpr <- function(x, ctx) {
  code <- rd_plain(x)
  opts <- attr(x, "Rd_option") %||% ""
  opts <- trimws(strsplit(rd_plain(opts), ",")[[1]])
  results <- sub("^results=", "", opts[grepl("^results=", opts)])
  results <- if (length(results)) results[[1]] else "text"
  if (results == "hide") {
    return("")
  }
  out <- tryCatch(
    eval(parse(text = code), envir = new.env(parent = globalenv())),
    error = function(e) NULL
  )
  if (is.null(out)) {
    return(md_code(code))
  }
  out <- paste0(as.character(out), collapse = "\n")
  if (results == "rd") {
    rd <- tryCatch(
      tools::parse_Rd(textConnection(out), fragment = TRUE),
      error = function(e) NULL
    )
    if (is.null(rd)) {
      return(md_escape(out, ctx))
    }
    return(sub("\\s+$", "", md_nodes(rd, ctx)))
  }
  if (results == "verbatim") {
    return(md_code(out))
  }
  md_escape(out, ctx)
}
