% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llm-use.R
\name{llm_use}
\alias{llm_use}
\title{Specify the model to use}
\usage{
llm_use(
  backend = NULL,
  model = NULL,
  ...,
  .silent = FALSE,
  .cache = NULL,
  .force = FALSE
)
}
\arguments{
\item{backend}{The name of an supported back-end provider. Currently only
'ollama' is supported.}

\item{model}{The name of model supported by the back-end provider}

\item{...}{Additional arguments that this function will pass down to the
integrating function. In the case of Ollama, it will pass those arguments to
\code{ollamar::chat()}.}

\item{.silent}{Avoids console output}

\item{.cache}{The path to save model results, so they can be re-used if
the same operation is ran again. To turn off, set this argument to an empty
character: \code{""}. It defaults to a temp folder. If this argument is left
\code{NULL} when calling this function, no changes to the path will be made.}

\item{.force}{Flag that tell the function to reset all of the settings in the
R session}
}
\value{
A \code{mall_session} object
}
\description{
Allows us to specify the back-end provider, model to use during the current
R session
}
\examples{
print(mtcars)
}
