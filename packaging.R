#=============================================================================
# Put together the package
#=============================================================================

# WORKFLOW: UPDATE EXISTING PACKAGE
# 1) Modify package content and documentation.
# 2) Increase package number in "use_description" below.
# 3) Go through this script and carefully answer "no" if a "use_*" function
#    asks to overwrite the existing files. Don't skip that function call.
# devtools::load_all()

library(usethis)

# Sketch of description file
use_description(
  fields = list(
    Title = "Shed Light on Black Box Machine Learning Models",
    Version = "0.8.2",
    Description = "Shed light on black box machine learning models by the help of model performance, variable importance, global surrogate models, ICE profiles, partial dependence (Friedman J. H. (2001) <doi:10.1214/aos/1013203451>), accumulated local effects (Apley D. W. (2016) <arXiv:1612.08468>), further effects plots, scatter plots, interaction strength, and variable contribution breakdown (approximate SHAP) for single observations (Gosiewska and Biecek (2019) <arxiv:1903.11420>). All tools are implemented to work with case weights and allow for stratified analysis. Furthermore, multiple flashlights can be combined and analyzed together.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre', 'cph'))",
    Depends = "R (>= 3.2.0)",
    LazyData = NULL
  ),
  roxygen = TRUE
)

# Imports
use_package("cowplot", "Imports")
use_package("dplyr", "Imports", min_version = "1.1.0")
use_package("ggplot2", "Imports")
use_package("MetricsWeighted", "Imports", min_version = "0.3.0")
use_package("rlang", "Imports", min_version = "0.3.0")
use_package("rpart", "Imports")
use_package("rpart.plot", "Imports")
use_package("stats", "Imports")
use_package("tibble", "Imports")
use_package("tidyr", "Imports", min_version = "1.0.0")
use_package("tidyselect", "Imports")
use_package("utils", "Imports")
use_package("withr", "Imports")

# Suggests
use_package("caret", "Suggests")
use_package("mlr3", "Suggests")
use_package("mlr3learners", "Suggests")
use_package("moderndive", "Suggests")
use_package("ranger", "Suggests")
use_package("xgboost", "Suggests")

use_gpl_license(2)

use_github_links() # use this if this project is on github

# Your files that do not belong to the package itself (others are added by "use_* function")
use_build_ignore(c("^packaging.R$", "[.]Rproj$", "^backlog$",
                   "^cran-comments.md$", "^logo.png$"), escape = FALSE)

# If your code uses the pipe operator %>%
# use_pipe()

# If your package contains data. Google how to document
# use_data()

# Add short docu in Markdown (without running R code)
use_readme_md()

# Longer docu in RMarkdown (with running R code). Often quite similar to readme.
use_vignette("p1_flashlight")
use_vignette("p2_caret")
use_vignette("p3_mlr3")

# If you want to add unit tests
use_testthat()
# use_test("test-eff.R")

# On top of NEWS.md, describe changes made to the package
use_news_md()

# Add logo
use_logo("logo.png")

# If package goes to CRAN: infos (check results etc.) for CRAN
use_cran_comments()


#=============================================================================
# Finish package building (can use fresh session)
#=============================================================================

library(devtools)

document()
test()
check(manual = TRUE, cran = TRUE)
build()
# build(binary = TRUE)
install()

# Run only if package is public(!) and should go to CRAN
if (FALSE) {
  check_win_devel()
  check_rhub(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))

  # Wait until above checks are passed without relevant notes/warnings
  # then submit to CRAN
  release()
}
