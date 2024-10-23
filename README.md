
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgsite

<!-- badges: start -->
<!-- badges: end -->

The goal of `pkgsite` is to make it easier to create a package website
using Quarto. It attempts to do for R what
[Quartodoc](https://machow.github.io/quartodoc/get-started/overview.html)
does for Python package documentation.

### When should I use `pkgsite`?

In the vast majority of cases,
[`pkgdown`](https://pkgdown.r-lib.org/index.html) is enough to create a
well refined package website. `pkgdown` is a well developed mature
package that covers all sorts of special needs. In fact, `pkgsite`
actually uses `pkgdown` to perform several operations.

So apart for wanting a Quarto site for your R package, a couple of
reasons to use `pkgsite` to create your reference Quarto pages are:

- **You want to use the output
  [“freeze”](https://quarto.org/docs/projects/code-execution.html#freeze)
  capabilities in Quarto** - You would like the examples in the help
  pages to run, but, the ability to run such examples depend on specific
  technologies to be accessible to where the site is being published,
  which is usually GitHub. Examples of those technologies could be
  databases, Spark and Large Language Models. The idea would be to
  render the website locally, where you would have access to the needed
  technologies, and make commit the resulting “\_freeze” folder to the
  repository. This will allow any re-building of your website by GitHub
  to have the necessary code output to create the HTML files.

- **You want to build a unified site** - Your packages from your project
  may be available in multiple languages, and wish to make a unified
  website. The idea here is if you have output from Quartodoc from the
  Python side of your project, that needs to be available in the website
  along with the references from the R side of your project.

An example of having both reasons to use `pkgsite`, is the
[`mall`](https://github.com/mlverse/mall) package. This project has a
Python, and an R package. And the examples in both packages need an LLM
in order to work, here is the resulting [reference
pages](https://mlverse.github.io/mall/reference/).

## Installation

You can install the development version of pkgsite from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/pkgsite")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pkgsite)

write_reference()
#> reference/index.qmd
#> reference/index_to_qmd.qmd
#> reference/rd_to_list.qmd
#> reference/rd_to_qmd.qmd
#> reference/write_reference.qmd
#> reference/write_reference_index.qmd
#> reference/write_reference_pages.qmd
```
