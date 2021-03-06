% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verbose.R
\name{print_if_debug}
\alias{print_if_debug}
\title{Debug printig}
\usage{
print_if_debug(message, object, debug = F)
}
\arguments{
\item{message}{The messsage that should be printed in the console}

\item{object}{The object to be printed to the console}

\item{debug}{Prints to console only if T}
}
\description{
Prints the message to the console if the debug parameter has
  been set to TRUE.
}
\examples{
print_if_verbose("Hello this is a verbose message", TRUE)
}
