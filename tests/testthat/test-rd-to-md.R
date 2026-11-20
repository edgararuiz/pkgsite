test_that("Paragraphs are joined and text is escaped", {
  rd <- rd_fragment("R\u2019s \\code{x}\ncosts $5 * 2\n\nNew @para")
  expect_equal(rd_md(rd), c("R\u2019s `x` costs \\$5 \\* 2", "", "New \\@para"))
})

test_that("Lists are converted", {
  rd <- rd_fragment(
    r"(\itemize{
\item One
\item Two
\enumerate{\item A}
})"
  )
  expect_equal(rd_md(rd), c("- One", "- Two", "", "  1. A"))
})

test_that("Subsections, code blocks and math are converted", {
  rd <- rd_fragment(
    r"(\subsection{\code{x = 1}}{
Text \eqn{a \le b}{a <= b}.
\if{html}{\out{<div class="sourceCode r">}}\preformatted{f(x)
g(x)
}\if{html}{\out{</div>}}
})"
  )
  expect_equal(
    rd_md(rd),
    c("### `x = 1`", "", "Text $a \\le b$.", "", "```r", "f(x)", "g(x)", "```")
  )
})

test_that("Tables are converted", {
  rd <- rd_fragment(
    r"(\tabular{lr}{
  a \tab \code{b} \cr
  c | d \tab e \cr
})"
  )
  expect_equal(
    rd_md(rd),
    c("| a | `b` |", "|:---|---:|", "| c \\| d | e |")
  )
})

test_that("Definition lists and links are converted", {
  rd <- rd_fragment(r"(\describe{\item{\code{a}}{An \href{https://x.org}{x}}})")
  expect_equal(rd_md(rd), c("`a`", ":   An [x](https://x.org)"))
})

test_that("Sexpr results are evaluated", {
  rd <- rd_fragment(r"(\doi{10.1/abc}.)")
  expect_equal(rd_md(rd), "[doi:10.1/abc](https://doi.org/10.1/abc).")
})

test_that("Inline lists use line breaks", {
  rd <- rd_fragment(r"(Values: \itemize{\item a \item b})")
  expect_equal(rd_md_inline(rd), "Values: <br>\u2022 a<br>\u2022 b")
})
