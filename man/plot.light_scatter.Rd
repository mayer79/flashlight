% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/light_scatter.R
\name{plot.light_scatter}
\alias{plot.light_scatter}
\title{Scatter Plot}
\usage{
\method{plot}{light_scatter}(x, swap_dim = FALSE, facet_scales = "free_x", rotate_x = FALSE, ...)
}
\arguments{
\item{x}{An object of class "light_scatter".}

\item{swap_dim}{If multiflashlight and one "by" variable, or single flashlight
with two "by" variables, swap the role of color variable and facet variable.
If multiflashlight or one "by" variable, use colors instead of facets.}

\item{facet_scales}{Scales argument passed to \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}.}

\item{rotate_x}{Should x axis labels be rotated by 45 degrees?}

\item{...}{Further arguments passed to \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}. Typical arguments
would be \code{alpha = 0.2} or \code{position = "jitter"} to avoid overplotting.}
}
\value{
An object of class "ggplot".
}
\description{
Values are plotted against a variable. The object returned is of class "ggplot"
and can be further customized. To avoid overplotting, try \code{alpha = 0.2} or
\code{position = "jitter"}.
}
\seealso{
\code{\link[=light_scatter]{light_scatter()}}
}
