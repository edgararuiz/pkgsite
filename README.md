
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgsite

<!-- badges: start -->

[![R-CMD-check](https://github.com/edgararuiz/pkgsite/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/edgararuiz/pkgsite/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/edgararuiz/pkgsite/branch/main/graph/badge.svg)](https://app.codecov.io/gh/edgararuiz/pkgsite?branch=main)
<!-- badges: end -->

## Intro

The goal of `pkgsite` is to make it easier to create a package website
using Quarto. It attempts to do for R what
[Quartodoc](https://machow.github.io/quartodoc/get-started/overview.html)
does for Python package documentation.

## When should I use `pkgsite`?

In the vast majority of cases
[`pkgdown`](https://pkgdown.r-lib.org/index.html) is enough to create
top tier package website. `pkgdown` is a well developed mature package
that covers all sorts of special needs. In fact, `pkgsite` uses
`pkgdown` to perform several operations.

So apart of simply wanting a Quarto site for your R package, a couple of
reasons to use `pkgsite` are:

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

## Usage

In it’s current state, `pkgsite`’s functions build on top of each other,
in order to create the reference pages and the index page. The top tier
function is `write_reference()`.

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

The function uses sensible defaults, so even by calling it without
changing any arguments should work without errors. Internally, the
function applies such sensible values if specific arguments are left
`NULL`.

## Configure via the ’\_quarto.yml’ file

Just as with
[quartodoc](https://machow.github.io/quartodoc/get-started/overview.html#basic-use),
`pkgsite` also supports configuration via the ’\_quarto.yml’ file. You
just need to add a ‘pkgsite’ section at the top level of the yaml file.
Here is an example of the values that are available for use:

``` yaml
pkgsite: 
    dir: "." # location of the package
    reference:
      dir: reference # target folder for qmd files
      template: inst/templates/_reference.qmd # reference template location
      not_run_examples: true # Flag to run the 'Not Run' examples
      index:
        file: index.qmd # name of the index file to use
        template: inst/templates/_index.qmd # template for index qmd file
```

Via the YAML file, it is also possible to create custom grouping
sections for your functions in the index file. Here is an example of how
to accomplish what is setup for this package’s website:

``` yaml
pkgsite: 
    dir: "."
    reference:
      dir: reference
      template: inst/templates/_reference.qmd
      not_run_examples: true
      index:
        file: index.qmd
        template: inst/templates/_index.qmd
        contents: # add 'sections' levels
          - section: Quarto file creation
            contents: 
            - write_reference.qmd
            - write_reference_index.qmd
            - write_reference_pages.qmd
          - section: Conversion functions
            contents:
            - index_to_qmd.qmd
            - rd_to_qmd.qmd
            - rd_to_list.qmd
```

The `mall` package has another example of how to configure the YAML
file:
[mall/\_quarto.yml](https://github.com/mlverse/mall/blob/main/_quarto.yml#L67-L90)
