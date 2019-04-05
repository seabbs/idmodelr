
<!-- README.md is generated from README.Rmd. Please edit that file -->
Infectious Disease Modelling Utilities <img src="man/figures/logo.png" align="right" alt="" width="120" />
==========================================================================================================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/idmodelr)](https://cran.r-project.org/package=idmodelr) [![develVersion](https://img.shields.io/badge/devel%20version-0.5.8-blue.svg?style=flat)](https://github.com/getTBinR) [![Build Status](https://travis-ci.org/seabbs/idmodelr.svg?branch=master)](https://travis-ci.org/seabbs/idmodelr) [![codecov](https://codecov.io/gh/seabbs/idmodelr/branch/master/graph/badge.svg)](https://codecov.io/gh/seabbs/idmodelr) [![Documentation via pkgdown](https://img.shields.io/badge/Documentation-click%20here!-lightgrey.svg?style=flat)](https://www.samabbott.co.uk/idmodelr/) [![Development documentation via pkgdown](https://img.shields.io/badge/Development%20Documentation-click%20here!-lightblue.svg?style=flat)](https://www.samabbott.co.uk/idmodelr/dev) [![badge](https://img.shields.io/badge/Launch-idmodelr-lightblue.svg)](https://mybinder.org/v2/gh/seabbs/idmodelr/master?urlpath=rstudio) [![Travis-CI Build Status](https://travis-ci.org/seabbs/idmodelr.svg?branch=master)](https://travis-ci.org/seabbs/idmodelr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/seabbs/idmodelr?branch=master&svg=true)](https://ci.appveyor.com/project/seabbs/idmodelr) [![Coverage Status](https://img.shields.io/codecov/c/github/seabbs/getTBinR/master.svg)](https://codecov.io/github/seabbs/idmodelr?branch=master)

`idmodelr` is an R package that contains utility functions for infectious disease modelling. It also offers some simple compartmental ODE models which can be used for teaching purposes or as the basis for more complex models.

Installation
------------

You can install idmodelr from github with:

``` r
# install.packages("devtools")
devtools::install_github("seabbs/idmodelr")
```

Contributing
------------

File an issue [here](https://github.com/seabbs/idmodelr/issues) if there is a feature, that you think is missing from the package, or better yet submit a pull request!

Please note that the `idmodelr` project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

### Docker

This packge was developed in a docker container based on the [tidyverse](https://hub.docker.com/r/rocker/tidyverse/) docker image. To run the docker image run:

``` bash
docker run -d -p 8787:8787 --name idmodelr -e USER=seabbs -e PASSWORD=seabbs seabbs/idmodelr
```

The rstudio client can be found on port `:8787` at your local machines ip. The default username:password is seabbs:seabbs, set the user with `-e USER=username`, and the password with `- e PASSWORD=newpasswordhere`. The default is to save the analysis files into the user directory. Alternatively, access the development environment via [binder](https://mybinder.org/v2/gh/seabbs/idmodelr/master?urlpath=rstudio).
