# flashlight 0.9.0.9000

## New functionality

- `plot.light_effects()` has gained an argument `recode_labels` to modify the curve labels.

## Bug fixes

- More than one "by" variable would raise an error when creating the "flashlight" object, see #62.

## Deprecated functionality

- `add_shap()`: Deprecated in favor of {kernelshap} or {fastshap}.
- Consequently, `type = "shap"` in `light_profile()`, `light_importance()`, `light_scatter()`, and `light_profile2d()` is deprecated as well.
- `plot_counts()` is deprecated.
- `light_recode()` is deprecated.
- The option `stats = "quartile"`of `light_effects()` and `light_profile()` is deprecated.
- Column names of resulting data objects cannot be set via `options()` anymore.

## Exported -> internal

The following functions are now internal:

- `grouped_center()`
- `grouped_counts()`
- `grouped_stats()`
- `grouped_weighted_mean()`
- `all_identical()`
- `auto_cut()`
- `common_breaks()`
- `cut3()`

## Minor changes

- `most_important()` is not S3 anymore.
- `plot.light_breakdown()` and `plot.light_importance()` do not use flipped coordinates anymore.

## Less dependencies

- {cowplot} and {withr} packages have been removed from "imports".

## Announcements for upcoming version 1.1.0

- The argument "stats" in `light_effects()` and `light_profile()` will be removed. It has no effect anymore.

## Maintenance

- Update code coverage version.

# flashlight 0.9.0

## Announcements for upcoming version 1.0.0

The following breaking changes are intended for version 1.0.0.

### Deprecated functionality

- `add_shap()`: Deprecated in favor of {kernelshap} or {fastshap}.
- Consequently, `type = "shap"` in `light_profile()`, `light_importance()`, `light_scatter()`, and `light_profile2d()` is deprecated as well.
- `plot_counts()`: Deprecated. Might later be replaced by {ggside}.
- Argument `stats = "quartiles` in `light_profile()` and `light_effects()`.

### External -> internal

These functions will become internal in {flashlight} 1.0.0. Most likely you have never used any of them.

- `grouped_center()`
- `grouped_counts()`
- `grouped_stats()`
- `grouped_weighted_mean()`
- `light_check()`
- `light_combine()`
- `light_recode()` -> similar functionality will be added as option in `light_effects()`
- `all_identical()`
- `auto_cut()`
- `cut3()`

### Options and column names

Currently, predefined column names in data objects returned by `light_*()` functions can be changed via `options()`. For instance, the model name is stored in column "label", which can be changed via option `flashlight.label_name = "label"`. 

In {flashlight} 1.0.0, these options will vanish. To avoid clashs with existing feature names, we will use exotic column names such as "label_" instead. As a consequence, data returned by `light_*()` functions will have different column names.

## Maintenance

- Introduced Github actions
- Comply with {dplyr} and {ggplot2} depreciation cycles
- Better help files
- More compact vignette(s)
- Dropped suggested packages caret, mlr3, mlr3learners, moderndive, ranger, xgboost

# flashlight 0.8.0

## New functionality

- Added new explainer `light_profile2d` for 2D plots of partial dependence and other profiles (residuals, predictions, response, SHAP).

## Other changes

- Added logo

- All `zzz_names` arguments are deprecated and are set as `options(flashlight.zzz_names = ...)` instead.

- `light_ice` and `light_scatter` now use `override.aes` to suppress `alpha` in legend.

- Greatly improved error messages.

- Bug fixes.

# flashlight 0.7.5

Another maintenance release to remove dependency on ggpubr.

# flashlight 0.7.4

This is a maintenance release to deal with an issue with XGBoost.

# flashlight 0.7.3

This is a documentation and maintainance release.

## Maintenance

- Switched to dplyr 1.0.0.

- Added 270+ unit tests.

- Added vignettes on how to use flashlight with `caret` and `mlr3`.

## Minor changes of defaults

- The plots of `light_breakdown` have been slightly improved.

## Bug fixes

- `light_effects` and `light_profile` did not work correctly if `pred` are provided together with a `pd_indices`.

- Using `pred` in `light_effects` and `light_profile` with a multiflashlight will now produce an informative error.

# flashlight 0.7.2

## Visible changes of defaults

- `light_interaction` now uses a default grid size of 200 instead of 30 to calculate partial dependency profiles. Furthermore, the used sample size to calculate the interaction statistics have been increased from 300 to 1000. The two changes improve considerably the stability of the results at the price of a substantial increase in runtime. For slow prediction functions, choose lower values as suitable.

## New functionality

### Minor

- Like for `light_effects`, the plot method of `light_profile` has received an argument `show_points` to suppress the plotting of points.

- The plot method for `light_performance` has received a `geom` argument. It allows to switch from bars to points.

## Requirement changes

- Now requires at least tidyr >= 1.0.0 since `expand_grid` is used to calculate ICE profiles.

- R dependency has been lowered from 3.5 to 3.1.

- Removed suggested packages caret and lubridate.

# flashlight 0.7.1

Emergency release. Fixes a problem introduced with ggplot 3.3.0 when plotting light_importance and light_interaction objects.


# flashlight 0.7.0

## New functionality

### Major

- Added `light_global_surrogate` and `plot.light_global_surrogate` to create a global surrogate tree model that explains predictions from the original model in a simple to understand way.

# flashlight 0.6.0

## New functionality

### Major

- Added `light_scatter` and `plot.light_scatter` to create scatter plots of response, predictions, residuals, or SHAP values by some variable.

- Static (approximate) SHAP values can be added to a (multi-)flashlight by calling `add_shap`.

- `light_importance`, `light_profile`, and `light_scatter` are now able to work with SHAP values.

### Minor

- The plot functions of `light_importance`, `light_interaction`, and `light_profile` now prints informative labels to show what is actually calculated.

- Added argument "show_points" to `plot.light_effects` to control if points should be added to the lines or not. Default is `TRUE`.

## Interface change

### Minor

- Removed currently unused output columns "value_shuffled" and "value_before" of `light_importance` due to its incompatibility with the new "shap" importances.

- Added argument "type_name" to `light_interaction`. Its output now also contains a slot `type` as well as `type_name`.

# flashlight 0.5.0

## New functionality

### Major

- Added Friedman's H statistic (global and pairwise interaction strength) to `light_interaction` with variants.

- Added random permutation visit strategy to `light_breakdown`, serving as approximate SHAP.

### Minor

- Added more options how to center `light_ice`: Mean center each profile to the same value (within "by" groups) and 0-mean center each profile.

- Added option `rotate_x` to `plot_light_breakdown` and `plot_light_importance`.

- Added function `grouped_center` to allow grouped and weighted 0-mean centering of a numeric variable.

- Added function `grouped_count` to allow grouped and weighted counts.

- Added function `grouped_weighted_mean` for fast grouped weighted means.

- `response`, `residuals`, and `predict` now have a method for multiflashlights.

## Interface change

### Minor

- Combined arguments `center = TRUE/FALSE` and `center_at` to one argument `center` with default "no". This affects `light_ice`, and `light_profile`.

- `order_by_importance` argument to `light_breakdown` has been replaced by `visit_strategy`.

- Removed `top_m` argument of `light_breakdown` as it does not make sense.


## Other changes

### Minor

-  `auto_cut`, the workhorse behind quantile binning a numeric input variable x, is now using ecdf based quantiles in order to ensure all evaluation points are in the domain of x.

- Centering at "first", "middle", and "last" in `light_ice` now anchors the curves at 0 to be in line with other implementations.

## Bug fixes

### Major

- `light_ice` was based on `dplyr::crossing`, unintentionally throwing away duplicate reference rows. This is now replaced by `dplyr::expand_grid`.

# flashlight 0.4.0

## New functionality

### Major

- Added `light_interaction`, a measure of overall interaction strength per covariable based on standard deviation across c-ICE curves.

- Added the option `m_repetitions = 1` to `light_importance`. Set to higher value to get more stable estimates of variable importance as well as standard errors for small data.

### Minor

- Added the option `digits` to `plot_counts` with default 0. Helps to format large counts or fractional counts.

- Added to option `center_at` to `light_ice` in order to control if ICE curves should be centered at position "first", "middle", or "last". Similarly, added `pd_center_at` to `light_profile`.

## Bug fixes

- Fixed a bug related to the color legend of the `plot`-method of `light_ice`.

# flashlight 0.3.0

## New functionality

- Added `type = "ale"` to `light_profile` to show accumulated local effects plots (ALE plots). They are now also calculated by `light_effects`. In the `plot` method, set `use = "all"` to show all profiles. The default is all without ALE plots.

- Added the possibility to center ICE curves at first evaluation point to improve visibility of interactions. Together with partial dependence with quartile aggregation, this is an interesting possibility to detect interactions.

- `grouped_stats` has received an argument `value_name`.

## Bug fixes

- Option `cut_type` was not active for partial dependence profiles.

- Option `cut_tpye` was not active for the multiflashlight method of `light_profile`.

# flashlight 0.2.0

## New functionality

- Added variable contribution breakdown for single observations.

## Interface change

- Removed `zero_counts` argument in `plot_counts`.

## Bug fixes

- `zero_counts` in `plot_counts` and `plot.light_effects` had no effect for single flashlights with no "by" variable. This is fixed.

# flashlight 0.1.0

This is the initial release.
