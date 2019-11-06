# flashlight 0.3.1

## New functionality

- Added the option `m_repetitions = 1` to `light_importance`. Set to higher value to get more stable estimates of variable importance as well as standard errors.

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
