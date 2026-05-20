# Replace "pkgsite" with your package name, and update the URL to your site
options(
  # Tells downlit where to send links for your package's functions
  downlit.local_packages = list(
    pkgsite = "https://edgararuiz.github.io/pkgsite"
  ),
  # Tells downlit to search your package when resolving bare function names
  downlit.attached = "pkgsite"
)

# Quarto passes the list of rendered files via this environment variable
output_files_env <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES", unset = "")

if (nzchar(output_files_env)) {
  # Incremental render: only process the files Quarto just built
  html_files <- strsplit(output_files_env, "\n")[[1]]
  html_files <- html_files[grepl("\\.html$", html_files)]
} else {
  # Full render or manual run: process every HTML file in the output directory
  output_dir <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", unset = "docs")
  html_files <- list.files(
    output_dir,
    pattern = "\\.html$",
    recursive = TRUE,
    full.names = TRUE
  )
}

# Apply downlit to each file in-place, adding links to function documentation
for (f in html_files) {
  downlit::downlit_html_path(f, f)
}
