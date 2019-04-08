# localModel: Local Explanations of Machine Learning Models for Tabular Data.

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/localModel)](https://cran.r-project.org/package=localModel)
[![Travis-CI Build Status](https://travis-ci.org/ModelOriented/localModel.svg?branch=master)](https://travis-ci.org/ModelOriented/localModel)
[![Coverage Status](https://img.shields.io/codecov/c/github/ModelOriented/localModel/master.svg)](https://codecov.io/github/ModelOriented/localModel?branch=master)


`localModel` is a successor to the [`live`](https://github.com/MI2DataLab/live) package. It implements a variant of LIME method for explaining single predictions of black box machine learning models for tabular data.
Interpretable features are created based on [Ceteris Paribus](https://github.com/ModelOriented/ceterisParibus2) plots.
Details of the methodology are described in the vignette.

To get started, install the newest version from GitHub by using the code below. 
Please do not use the `devtools` package, since it is affected [a bug](https://github.com/r-lib/devtools/issues/1900) which makes `localModel` installation impossible.

```
remotes::install_github("ModelOriented/localModel")
```

To get help, see examples and details of the methodology, please refer to package website and vignettes.

## Acknowledgments 

Work on this package is financially supported by the NCN Opus grant 2017/27/B/ST6/01307.
