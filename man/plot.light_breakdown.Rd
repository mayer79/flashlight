% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_light_breakdown.R
\name{plot.light_breakdown}
\alias{plot.light_breakdown}
\title{Visualize Variable Contribution Breakdown for Single Observation}
\usage{
\method{plot}{light_breakdown}(x, facet_scales = "free", facet_ncol = 1, rotate_x = FALSE, ...)
}
\arguments{
\item{x}{An object of class \code{light_breakdown}.}

\item{facet_scales}{Scales argument passed to \code{facet_wrap}.}

\item{facet_ncol}{\code{ncol} argument passed to \code{facet_wrap}.}

\item{rotate_x}{Should x axis labels be rotated by 45 degrees? Default is FALSE.}

\item{...}{Further arguments passed to \code{geom_label}.}
}
\value{
An object of class \code{ggplot2}.
}
\description{
Minimal visualization of an object of class \code{light_breakdown} as waterfall plot. The object returned is of class \code{ggplot} and can be further customized.
}
\details{
The waterfall plot is to be read from top to bottom. The first line describes the (weighted) average prediction in the query data used to start with. Then, each additional line shows how the prediction changes due to the impact of the corresponding variable. The last line finally shows the original prediction of the selected observation. Multiple flashlights are shown in different facets. Positive and negative impacts are visualized with different colors.
}
\examples{
fit <- lm(Sepal.Length ~ . + Petal.Length:Species, data = iris)
fl <- flashlight(model = fit, label = "lm", data = iris, y = "Sepal.Length")
plot(light_breakdown(fl, new_obs = iris[1, ]))
}
\seealso{
\code{\link{light_importance}}.
}