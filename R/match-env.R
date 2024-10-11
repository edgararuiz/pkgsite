match_env <- function(topics) {
  fns <- env(empty_env(),
    "-" = function(x) -x,
    "c" = function(...) c(...)
  )
  out <- env(fns)

  topic_index <- seq_along(topics$name)

  # Each \alias{} is matched to its position
  topics$alias <- lapply(topics$alias, unique)
  aliases <- set_names(
    rep(topic_index, lengths(topics$alias)),
    unlist(topics$alias)
  )
  env_bind(out, !!!aliases)

  # As is each \name{} - we bind these second so that if \name{x} and \alias{x}
  # are in different files, \name{x} wins. This doesn't usually matter, but
  # \name{} needs to win so that the default_reference_index() matches the
  # correct files
  env_bind(out, !!!set_names(topic_index, topics$name))

  # dplyr-like matching functions

  any_alias <- function(f, ..., .internal = FALSE) {
    f <- as_function(f)
    alias_match <- purrr::map_lgl(unname(topics$alias), function(x) {
      any(f(x, ...))
    })
    name_match <- purrr::map_lgl(topics$name, f, ...)

    which((alias_match | name_match) & is_public(.internal))
  }

  is_public <- function(internal) {
    if (!internal) !topics$internal else rep(TRUE, nrow(topics))
  }
  fns$starts_with <- function(x, internal = FALSE) {
    check_string(x)
    check_bool(internal)

    any_alias(~ grepl(paste0("^", x), .), .internal = internal)
  }
  fns$ends_with <- function(x, internal = FALSE) {
    check_string(x)
    check_bool(internal)

    any_alias(~ grepl(paste0(x, "$"), .), .internal = internal)
  }
  fns$matches <- function(x, internal = FALSE) {
    check_string(x)
    check_bool(internal)

    any_alias(~ grepl(x, .), .internal = internal)
  }
  fns$contains <- function(x, internal = FALSE) {
    check_string(x)
    check_bool(internal)

    any_alias(~ grepl(x, ., fixed = TRUE), .internal = internal)
  }
  fns$has_keyword <- function(x) {
    check_character(x)
    which(purrr::map_lgl(topics$keywords, ~ any(. %in% x)))
  }
  fns$has_lifecycle <- function(x) {
    check_string(x)
    which(purrr::map_lgl(topics$lifecycle, ~ any(. %in% x)))
  }
  fns$has_concept <- function(x, internal = FALSE) {
    check_string(x)
    check_bool(internal)

    match <- purrr::map_lgl(topics$concepts, ~ any(str_trim(.) == x))
    which(match & is_public(internal))
  }
  fns$lacks_concepts <- function(x, internal = FALSE) {
    check_character(x)
    check_bool(internal)

    match <- purrr::map_lgl(topics$concepts, ~ any(str_trim(.) == x))
    which(!match & is_public(internal))
  }
  fns$lacks_concept <- fns$lacks_concepts
  out
}
