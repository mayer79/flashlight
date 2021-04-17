#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

if (FALSE) {
  library(MetricsWeighted)
  library(tidyverse)
  library(cowplot)
  library(rpart)
  library(rpart.plot)
  library(testthat)
  library(withr)
  lapply(list.files("R", full.names = TRUE), source)
  .onLoad()
}

library(usethis)
library(devtools)

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "flashlight")

create_package(
  open = FALSE,
  pkg,
  fields = list(
    Title = "Shed Light on Black Box Machine Learning Models",
    Type = "Package",
    Version = "0.8.0",
    Date = Sys.Date(),
    Description = "Shed light on black box machine learning models by the help of model performance, variable importance, global surrogate models, ICE profiles, partial dependence (Friedman J. H. (2001) <doi:10.1214/aos/1013203451>), accumulated local effects (Apley D. W. (2016) <arXiv:1612.08468>), further effects plots, scatter plots, interaction strength, and variable contribution breakdown (approximate SHAP) for single observations (Gosiewska and Biecek (2019) <arxiv:1903.11420>). All tools are implemented to work with case weights and allow for stratified analysis. Furthermore, multiple flashlights can be combined and analyzed together.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre', 'cph'))",
    URL = "https://github.com/mayer79/flashlight",
    BugReports = "https://github.com/mayer79/flashlight/issues",
    Depends = "R (>= 3.2.0)",
    VignetteBuilder = "knitr",
    License = "GPL(>= 2)",
    LazyData = NULL,
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>")
)

file.copy(file.path(pkg, "DESCRIPTION"), to = getwd(), overwrite = TRUE)
# Use package has no option to look for pkg, so we first copy description from pkg, modify it and move back

# Imports
use_package("cowplot", "Imports")
use_package("dplyr", "Imports", min_version = "1.0.0")
use_package("ggplot2", "Imports")
use_package("MetricsWeighted", "Imports", min_version = "0.3.0")
use_package("rpart", "Imports")
use_package("rpart.plot", "Imports")
use_package("stats", "Imports")
use_package("tidyr", "Imports", min_version = "1.0.0")
use_package("tidyselect", "Imports")
use_package("utils", "Imports")
use_package("withr", "Imports")

# Suggests
use_package("caret", "Suggests")
use_package("knitr", "Suggests")
use_package("mlr3", "Suggests")
use_package("mlr3learners", "Suggests")
use_package("moderndive", "Suggests")
use_package("ranger", "Suggests")
use_package("rmarkdown", "Suggests")
use_package("testthat", "Suggests")
use_package("xgboost", "Suggests")

# Set up other files -------------------------------------------------
# use_readme_md()
# use_news_md()
# use_cran_comments()

# Copy readme etc.
file.copy(c(".Rbuildignore", "NEWS.md", "README.md", "cran-comments.md", "DESCRIPTION"),
          pkg, overwrite = TRUE)

# Copy R scripts and document them
files <- list.files("R", full.names = TRUE)
file.copy(files, file.path(pkg, "R"), overwrite = TRUE)
devtools::document(pkg)

# Tests
if (!dir.exists(file.path(pkg, "tests"))) {
  dir.create(file.path(pkg, "tests"))
}
file.copy("tests", pkg, recursive = TRUE)
test(pkg)

# Copy vignette
if (TRUE) {
  dir.create(file.path(pkg, "vignettes"))
  dir.create(file.path(pkg, "doc"))
  dir.create(file.path(pkg, "Meta"))
  file.copy(list.files("vignettes", full.names = TRUE),
            file.path(pkg, "vignettes"), overwrite = TRUE)
  devtools::build_vignettes(pkg)
}

# Check
check(pkg, vignettes = FALSE)

# Create
build(pkg)
# build(pkg, binary = TRUE)

# Install
install(pkg)

# Load
library(flashlight)

# modify .Rbuildignore in build project to ignore the proj file.

check_win_devel(pkg)

check_rhub(pkg, env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

devtools::release(pkg)
