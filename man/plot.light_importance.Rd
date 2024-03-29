% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/light_importance.R
\name{plot.light_importance}
\alias{plot.light_importance}
\title{Visualize Variable Importance}
\usage{
\method{plot}{light_importance}(
  x,
  top_m = Inf,
  swap_dim = FALSE,
  facet_scales = "fixed",
  rotate_x = FALSE,
  error_bars = TRUE,
  ...
)
}
\arguments{
\item{x}{An object of class "light_importance".}

\item{top_m}{Maximum number of important variables to be returned.}

\item{swap_dim}{If multiflashlight and one "by" variable or single flashlight with
two "by" variables, swap the role of dodge/fill variable and facet variable.
If multiflashlight or one "by" variable, use facets instead of colors.}

\item{facet_scales}{Scales argument passed to \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}.}

\item{rotate_x}{Should x axis labels be rotated by 45 degrees?}

\item{error_bars}{Should error bars be added? Defaults to \code{TRUE}.
Only available if \code{\link[=light_importance]{light_importance()}} was run with multiple permutations
by setting \code{m_repetitions} > 1.}

\item{...}{Further arguments passed to \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar()}}.}
}
\value{
An object of class "ggplot".
}
\description{
Visualization of an object of class "light_importance" via \code{\link[ggplot2:geom_bar]{ggplot2::geom_bar()}}.
If available, standard errors are added by \code{\link[ggplot2:geom_linerange]{ggplot2::geom_errorbar()}}.
The object returned is of class "ggplot" and can be further customized.
}
\details{
The plot is organized as a bar plot with variable names as x-aesthetic.
Up to two additional dimensions (multiflashlight and one "by" variable or single
flashlight with two "by" variables) can be visualized by facetting and dodge/fill.
Set \code{swap_dim = FALSE} to revert the role of these two dimensions.
One single additional dimension is visualized by a facet wrap,
or - if \code{swap_dim = FALSE} - by dodge/fill.
}
\seealso{
\code{\link[=light_importance]{light_importance()}}
}
