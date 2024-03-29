% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spy.R
\name{spy}
\alias{spy}
\alias{spy.data.frame}
\alias{spy.list}
\alias{spy.character}
\alias{spy_top}
\alias{spy_top.data.frame}
\alias{spy_btm}
\alias{spy_btm.data.frame}
\alias{spy_all}
\alias{spy_all.data.frame}
\title{Spy}
\usage{
spy(x, ...)

\method{spy}{data.frame}(x, ..., name = deparse(substitute(x)))

\method{spy}{list}(x, ..., name = deparse(substitute(x)))

\method{spy}{character}(x, ..., name = deparse(substitute(x)))

spy_top(x, ...)

\method{spy_top}{data.frame}(x, ..., n = 20)

spy_btm(x, ...)

\method{spy_btm}{data.frame}(x, ..., n = 20)

spy_all(x, ...)

\method{spy_all}{data.frame}(x, ..., n = getOption("max.print") - 5L)
}
\arguments{
\item{x}{Object to be printed}

\item{...}{Additional arguments, currently discarded}

\item{name}{Name of object to be printed}

\item{n}{For data frames, rows to be printed}
}
\value{
Invisible \code{NULL}
}
\description{
Spy
}
\examples{

spy(cars)
spy_top(cars)
spy_btm(cars)
spy_all(cars)
# spy_all(ggplot2::diamonds)

# in .Rprofile, set
set_register(    spy(), "4") # map to Shift-Cmd-G
set_register(spy_top(), "5") # map to Shift-Cmd-H
set_register(spy_btm(), "6") # map to Shift-Cmd-J
set_register(spy_all(), "7") # map to Shift-Cmd-A

# highlight cars, then Shift-Cmd-(G,H,J,A)
cars

}
