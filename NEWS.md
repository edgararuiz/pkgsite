# pkgsite (development version)

* Reference pages now keep the structure of the Rd files. Subsections become headings, and lists, tables, code blocks, math (`\eqn{}` and `\deqn{}`), links, and `\Sexpr{}` results, such as DOIs, are converted to Markdown. Apostrophes and quotes are no longer changed into backticks.
* The arguments are now written as a grid table, so descriptions can contain lists and code blocks, and the description column is wider.
* Examples from `@examplesIf` no longer show the `withAutoprint()` wrapper. The condition is used as the chunk's `eval` option instead.
* Reference pages with more than one custom section now include all of them.
* `index_to_qmd()` lists the functions from the usage section, so S3 methods are listed once, under the name of the generic. Descriptions are wrapped in a `.ref-desc` span instead of being indented with `&nbsp;`.
* `rd_to_list()` now returns Markdown, as a character vector of lines, for the description, details, value, and other text sections.

# pkgsite 0.1.0

* Initial CRAN release.
