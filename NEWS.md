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
