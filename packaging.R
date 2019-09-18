#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

# library(devtools) # Get latest MetricsWeighted branch
# install_github("mayer79/MetricsWeighted", ref = "cran_0.1.1")
library(MetricsWeighted)
library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(ggpubr)
lapply(list.files("R", full.names = TRUE), source)

library(usethis)
library(devtools)

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "flashlight")

create_package(
  pkg,
  fields = list(
    Title = "Shed Light on Black Box Machine Learning Models",
    Type = "Package",
    Version = "0.2.1",
    Date = Sys.Date(),
    Description = "Shed light on black box machine learning models by the help of model performance, permutation variable importance (Fisher et al. (2018) <arxiv:1801.01489>), ICE profiles, partial dependence (Friedman J. H. (2001) <doi:10.1214/aos/1013203451>), further effects plots, and variable contribution breakdown for single observations (Gosiewska and Biecek (2019) <arxiv:1903.11420>). All tools are implemented to work with case weights and allow for stratified analysis. Furthermore, multiple flashlights can be combined and analyzed together.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre', 'cph'))",
    Depends = "R (>= 3.5.0)",
    VignetteBuilder = "knitr",
    License = "GPL(>= 2)",
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>"))

file.copy(file.path(pkg, "DESCRIPTION"), to = getwd(), overwrite = TRUE)
# Use package has no option to look for pkg, so we first copy description from pkg, modify it and move back

# Imports
use_package("stats", "Imports")
use_package("utils", "Imports")
use_package("dplyr", "Imports")
use_package("tidyr", "Imports")
use_package("rlang", "Imports")
use_package("ggplot2", "Imports")
use_package("ggpubr", "Imports")
use_package("MetricsWeighted", "Imports", min_version = "0.2.0")

# Suggests
use_package("knitr", "Suggests")
use_package("lubridate", "Suggests")
use_package("ranger", "Suggests")
use_package("xgboost", "Suggests")
use_package("caret", "Suggests")
use_package("moderndive", "Suggests")

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

# Copy vignette
dir.create(file.path(pkg, "vignettes"))
dir.create(file.path(pkg, "doc"))
dir.create(file.path(pkg, "Meta"))
file.copy(list.files("vignettes", full.names = TRUE), file.path(pkg, "vignettes"), overwrite = TRUE)
devtools::build_vignettes(pkg)

# Check
check(pkg, manual = TRUE)

# Create
build(pkg)
build(pkg, binary = TRUE)

# Install
install(pkg)

# Load
library(flashlight)

# modify .Rbuildignore in build project to ignore the proj file.

check_win_devel(pkg)

check_rhub(pkg)

# setwd(pkg)

devtools::release(pkg)
