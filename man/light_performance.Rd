% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/light_performance.R
\name{light_performance}
\alias{light_performance}
\alias{light_performance.default}
\alias{light_performance.flashlight}
\alias{light_performance.multiflashlight}
\title{Model Performance of Flashlight}
\usage{
light_performance(x, ...)

\method{light_performance}{default}(x, ...)

\method{light_performance}{flashlight}(
  x,
  data = x$data,
  by = x$by,
  metrics = x$metrics,
  use_linkinv = FALSE,
  ...
)

\method{light_performance}{multiflashlight}(x, ...)
}
\arguments{
\item{x}{An object of class "flashlight" or "multiflashlight".}

\item{...}{Arguments passed from or to other functions.}

\item{data}{An optional \code{data.frame}.}

\item{by}{An optional vector of column names used to additionally group the results.
Will overwrite \code{x$by}.}

\item{metrics}{An optional named list with metrics. Each metric takes at least
four arguments: actual, predicted, case weights w and \code{...}.}

\item{use_linkinv}{Should retransformation function be applied? Default is \code{FALSE}.}
}
\value{
An object of class "light_performance" with the following elements:
\itemize{
\item \code{data}: A tibble containing the results.
\item \code{by} Same as input \code{by}.
}
}
\description{
Calculates performance of a flashlight with respect to one or more
performance measure.
}
\details{
The minimal required elements in the (multi-) flashlight are "y", "predict_function",
"model", "data" and "metrics". The latter two can also directly be passed to
\code{\link[=light_performance]{light_performance()}}. Note that by default, no retransformation function is applied.
}
\section{Methods (by class)}{
\itemize{
\item \code{light_performance(default)}: Default method not implemented yet.

\item \code{light_performance(flashlight)}: Model performance of flashlight object.

\item \code{light_performance(multiflashlight)}: Model performance of multiflashlight object.

}}
\examples{
fit_part <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
fl_part <- flashlight(
  model = fit_part, label = "part", data = iris, y = "Sepal.Length"
)
plot(light_performance(fl_part, by = "Species"), fill = "chartreuse4")

# Second model
fit_full <- lm(Sepal.Length ~ ., data = iris)
fl_full <- flashlight(
  model = fit_full, label = "full", data = iris, y = "Sepal.Length"
)
fls <- multiflashlight(list(fl_part, fl_full))

plot(light_performance(fls, by = "Species"))
plot(light_performance(fls, by = "Species"), swap_dim = TRUE)
}
\seealso{
\code{\link[=plot.light_performance]{plot.light_performance()}}
}
