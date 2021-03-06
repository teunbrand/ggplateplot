% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplateplot.R
\name{ggplateplot}
\alias{ggplateplot}
\title{Plate plots}
\usage{
ggplateplot(data = NULL, mapping = aes(), ..., environment = parent.frame())
}
\arguments{
\item{data}{Default dataset to use for plot. If not already a data.frame,
will be converted to one by \code{\link[ggplot2:fortify]{fortify()}}. If not specified,
must be supplied in each layer added to the plot.}

\item{mapping}{Default list of aesthetic mappings to use for plot.
If not specified, must be supplied in each layer added to the plot.}

\item{...}{Other arguments passed on to methods. Not currently used.}

\item{environment}{DEPRECATED. Used prior to tidy evaluation.}
}
\value{
A \code{ggplot} object.
}
\description{
This function is a wrapper for \code{ggplot()}, that tries to automatically guess
an appropriate shape for a microwell layout based on the data.
}
\details{
The 'guessing' part happens by looking at the number of observations
if the \code{data} argument is a \code{data.frame}, and the first two dimensions if
the \code{data} argument is a \code{matrix} or \code{array}. If it does not guess the
appropriate shape correctly, it might be easier to use
\code{ggplot() + ... + coord_shape(spec = ...)} directly.

If the \code{data} argument is an \code{array} or \code{matrix}, this is converted to a
long format \code{data.frame} and the \code{x} and \code{y} aesthetics are populated by
the first two dimensions of the \code{data} argument.
}
\examples{
# With long-format data
df <- expand.grid(LETTERS[1:2], 1:3)
df$value <- seq_len(nrow(df))

ggplateplot(df, aes(Var2, Var1, fill = factor(value))) +
  geom_well()

# With matrices/arrays, cell values become the 'value' column
m <- matrix(1:12, nrow = 3, ncol = 4)

ggplateplot(m, aes(fill = factor(value))) +
  geom_well()
}
