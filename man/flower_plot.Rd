% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{flower_plot}
\alias{flower_plot}
\title{plot flower plot}
\usage{
flower_plot(sample, value, start = 90, a = 0.5, b = 2,
  ellipse_col = rgb(135, 206, 235, 150, max = 255), circle_col = rgb(0, 162,
  214, max = 255), circle_text_cex = 1.5)
}
\arguments{
\item{sample}{a vector of sample names}

\item{value}{a vector of values}

\item{start}{init angle}

\item{a}{ellipse x length}

\item{b}{ellipse y length}

\item{ellipse_col}{flower colors}

\item{circle_col}{circle colors}

\item{circle_text_cex}{circle text cex}
}
\description{
This function plot a flower plot
}
\examples{
flower_plot(1:12, 1:12)
}
