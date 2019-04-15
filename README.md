
<!-- README.md is generated from README.Rmd. Please edit that file -->
Infectious disease model library and utilities <img src="man/figures/logo.png" align="right" alt="" width="120" />
==================================================================================================================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/idmodelr)](https://cran.r-project.org/package=idmodelr) [![develVersion](https://img.shields.io/badge/devel%20version-0.3.0-blue.svg?style=flat)](https://github.com/idmodelr) [![Documentation via pkgdown](https://img.shields.io/badge/Documentation-click%20here!-lightgrey.svg?style=flat)](https://www.samabbott.co.uk/idmodelr/) [![Development documentation via pkgdown](https://img.shields.io/badge/Development%20Documentation-click%20here!-lightblue.svg?style=flat)](https://www.samabbott.co.uk/idmodelr/dev) [![badge](https://img.shields.io/badge/Launch-idmodelr-lightblue.svg)](https://mybinder.org/v2/gh/seabbs/idmodelr/master?urlpath=rstudio) [![Travis-CI Build Status](https://travis-ci.org/seabbs/idmodelr.svg?branch=master)](https://travis-ci.org/seabbs/idmodelr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/seabbs/idmodelr?branch=master&svg=true)](https://ci.appveyor.com/project/seabbs/idmodelr) [![Coverage Status](https://img.shields.io/codecov/c/github/seabbs/idmodelr/master.svg)](https://codecov.io/github/seabbs/idmodelr?branch=master) [![metacran monthly downloads](http://cranlogs.r-pkg.org/badges/idmodelr)](https://cran.r-project.org/package=idmodelr) [![metacran downloads](http://cranlogs.r-pkg.org/badges/grand-total/idmodelr?color=ff69b4)](https://cran.r-project.org/package=idmodelr)

Explore a range of infectious disease models in a consistent framework. The primary aim of `idmodelr` is to provide a library of infectious disease models for researchers, students, and other interested individuals. These models can be used to understand the underlying dynamics and as a reference point when developing models for research. `idmodelr` also provides a range of utilities. These include: plotting functionality; a simulation wrapper; scenario analysis tooling; an interactive dashboard; tools for handling mult-dimensional models; and both model and parameter look up tables. Unlike other modelling packages such as [`pomp`](https://kingaa.github.io/pomp/), [`libbi`](http://libbi.org) and [`epimodel`](http://www.epimodel.org), `idmodelr` serves primarily as an educational resource. After users have explored model dynamics with `idmodelr` they may then implement their model using one of these packages in order to utilise the model fitting tools they provide. For newer modellers, this package reduces the barrier to entry by containing multiple infectious disease models, providing a consistent framework for simulation and visualisation, and signposting towards other, more research, focussed resources.

Installation
------------

You can install idmodelr from github with:

``` r
# install.packages("devtools")
devtools::install_github("seabbs/idmodelr")
```

Quick start
-----------

Dashboard
---------

A shiny application has been developed that showcases some of the functionality of the `idmodelr` package. This application allows the parameter spaces of a range of models built into `idmodelr` to be explored in an interactive session. It is designed to be used as a teaching aid when introducing people to the concepts behind infectious disease models without requiring them to interact with the underlying code. The code for the dashboard can be found [here](https://github.com/seabbs/exploreidmodels). It can be run locally using the following (*Note: this will install required packages to your system*),

``` r
#install.packages("shiny")
shiny::runGitHub("exploreidmodels", "seabbs")
```

or accessed [online](http://www.seabbs.co.uk/shiny/exploreidmodels) (*Note: This version may not be fully up to date*).

![Snapshot of the integrated dashboard.](man/figures/exploreidmodels.png)

Contributing
------------

### Contributing a model

Additional models are extremely welcome!

To add models in the same family as those already implemented (i.e [`SIR_ode`](https://github.com/seabbs/idmodelr/blob/master/R/SIR.R)) please follow the implemented coding style closely (alternatively open an [issue](https://github.com/seabbs/idmodelr/issues) explaining why this style needs updating). Models should be named using their compartments in capitals followed by lower case model details, and finally the model type. An example of this is the `SIR_demographics_ode` model. For highly complex models only the most major model details should be listed (aim for less than 40 characters). An entry for `model_details` is also required (see [`model_details.R`](https://github.com/seabbs/idmodelr/blob/master/data-raw/model_details.R%20for%20help%20with%20this)). Please consider also adding to the testing suite for your new model.Models can either be added via a pull request (i.e make all desired changes and then run `make`) or via an issue.

To add a new family of models (i.e stochastic models (*Note: These are currently planned in the current development cycle*)) please open an [issue](https://github.com/seabbs/idmodelr/issues) outlining your proposed approach. A new family of models is likely to require at least its own `solve_` (equivalent to [`solve_ode`](https://github.com/seabbs/idmodelr/blob/master/R/solve.R)) function and may also require other package changes.

### Other contributions

File an issue [here](https://github.com/seabbs/idmodelr/issues) if there is any other feature, that you think is missing from the package, or better yet submit a pull request!

Please note that the `idmodelr` project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

### Docker

This packge was developed in a docker container based on the [tidyverse](https://hub.docker.com/r/rocker/tidyverse/) docker image. To run the docker image run:

``` bash
docker run -d -p 8787:8787 --name idmodelr -e USER=seabbs -e PASSWORD=seabbs seabbs/idmodelr
```

The rstudio client can be found on port `:8787` at your local machines ip. The default username:password is seabbs:seabbs, set the user with `-e USER=username`, and the password with `- e PASSWORD=newpasswordhere`. The default is to save the analysis files into the user directory. Alternatively, access the development environment via [binder](https://mybinder.org/v2/gh/seabbs/idmodelr/master?urlpath=rstudio).
