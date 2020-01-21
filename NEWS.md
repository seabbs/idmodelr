# idmodelr 0.3.2 (developmemt)


# idmodelr 0.3.1

## Package updates

* Reorganised badges in the readme and package website to make them easier to navigate.
* Updated the use of  `{tidyr}` in the `scenario_analysis` to work with the soon to be released version. Based on [this](https://github.com/tidyverse/tidyr/blob/master/NEWS.md) update.
* Added [@akira-endo](https://github.com/akira-endo) as an author - thanks for the contributions! 

# idmodelr 0.3.0

## Feature updates

* Added SIS family of models.
* Added `model_details` dataframe listing model details.
* Added `parameter_details` dataframe listing details on common modelling parameters.
* `plot_model` can now accept a list of simulations and plot each separately
* Added a other resources vignette that details R based infectious disease modelling resources.

## Package updates

* New package title
* Updated license
* Image tests via `vdiffr`.
* Update README to point to resource signposting and switched to using `simulate_model`
* Linted code

# idmodelr 0.2.0

## Feature updates

* Added additional models.
* Simplified utility functions ready for extension.
* Streamlined models.
* Update package description.

## Package updates

* Added a `NEWS.md` file to track changes to the package.
* Dropped outdated functions.
* Moved to `furrr` from `mutlidplyr`.
* Modernised package infrastructure based on [`getTBinR`](https://github.com/seabbs/getTBinR).
* Improved test coverage.
