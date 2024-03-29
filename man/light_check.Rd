% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/light_check.R
\name{light_check}
\alias{light_check}
\alias{light_check.default}
\alias{light_check.flashlight}
\alias{light_check.multiflashlight}
\title{Check flashlight}
\usage{
light_check(x, ...)

\method{light_check}{default}(x, ...)

\method{light_check}{flashlight}(x, ...)

\method{light_check}{multiflashlight}(x, ...)
}
\arguments{
\item{x}{An object of class "flashlight" or "multiflashlight".}

\item{...}{Further arguments passed from or to other methods.}
}
\value{
The input \code{x} or an error message.
}
\description{
Checks if an object of class "flashlight" or "multiflashlight"
is consistently defined.
}
\section{Methods (by class)}{
\itemize{
\item \code{light_check(default)}: Default check method not implemented yet.

\item \code{light_check(flashlight)}: Checks if a flashlight object is consistently defined.

\item \code{light_check(multiflashlight)}: Checks if a multiflashlight object is consistently defined.

}}
\examples{
fit <- lm(Sepal.Length ~ ., data = iris)
fit_log <- lm(log(Sepal.Length) ~ ., data = iris)
fl <- flashlight(fit, data = iris, y = "Sepal.Length", label = "ols")
fl_log <- flashlight(fit_log,  y = "Sepal.Length", label = "ols", linkinv = exp)
light_check(fl)
light_check(fl_log)
}
