options(
  downlit.local_packages = list(pkgsite = "https://edgararuiz.github.io/pkgsite"),
  downlit.attached = "pkgsite"
)

output_dir <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", unset = "docs")

output_files_env <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES", unset = "")

if (nzchar(output_files_env)) {
  html_files <- strsplit(output_files_env, "\n")[[1]]
  html_files <- html_files[grepl("\\.html$", html_files)]
} else {
  html_files <- list.files(
    output_dir,
    pattern = "\\.html$",
    recursive = TRUE,
    full.names = TRUE
  )
}

for (f in html_files) {
  downlit::downlit_html_path(f, f)
}
