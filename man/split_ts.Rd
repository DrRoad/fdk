% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliar.R
\name{split_ts}
\alias{split_ts}
\title{Automatic Time Series Cross-Validation split}
\usage{
split_ts(.data, test_size, lag)
}
\arguments{
\item{.data}{DataFrame, tibble or tsibble structures.}

\item{test_size}{Numeric. How many periods will be use to asses the forecast accuracy.}

\item{lag}{Numeric. How many periods ahead to start the test size.}
}
\value{
Nested data-frames or tibbles.
}
\description{
Time series CV split heuristics should keep temporal dependencies, henceforth, the sample should not 
be "shuffled" into train and test. This function uses a different strategy to define the test and train sets
maintain the order of the data.
}
\examples{
\dontrun{
split_ts()
}
}
