% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{residuals.multiflashlight}
\alias{residuals.multiflashlight}
\title{Residuals for multiflashlight}
\usage{
\method{residuals}{multiflashlight}(object, ...)
}
\arguments{
\item{object}{An object of class "multiflashlight".}

\item{...}{Arguments used to update the multiflashlight before
calculating the residuals.}
}
\value{
A named list with residuals per flashlight.
}
\description{
Residuals method for an object of class "multiflashlight".
Pass additional elements to update the multiflashlight before calculation of
residuals.
}
\examples{
fit_part <- lm(Sepal.Length ~ Petal.Length, data = iris)
fit_full <- lm(Sepal.Length ~ ., data = iris)
mod_full <- flashlight(model = fit_full, label = "full")
mod_part <- flashlight(model = fit_part, label = "part")
mods <- multiflashlight(list(mod_full, mod_part), data = iris, y = "Sepal.Length")
residuals(mods, data = head(iris))
}
