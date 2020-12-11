[![Build Status](https://travis-ci.com/oceanbolt/oceanboltr.svg)](https://travis-ci.com/oceanbolt/oceanboltr)
[![Codecov test coverage](https://codecov.io/gh/oceanbolt/oceanboltr/branch/development/graph/badge.svg)](https://codecov.io/gh/oceanbolt/oceanboltr?branch=development)
[![Code size](https://img.shields.io/github/languages/code-size/oceanbolt/oceanboltr.svg)](https://github.com/oceanbolt/oceanboltr)
[![CRAN status](https://www.r-pkg.org/badges/version/oceanboltr)](https://CRAN.R-project.org/package=oceanboltr)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/oceanboltr?color=blue)](https://cran.r-project.org/package=oceanboltr)


# R Wrapper for Oceanbolt API

This is an R wrapper for the Oceanbolt API.

## How to Install

### CRAN

```{r}
install.packages('oceanboltr')
```

### Development version

```{r}
devtools::install_github('ocenbolt/oceanboltr')
```

## Initialization of the package

You need to generate API token at your OceanBolt [admin panel](https://app.oceanbolt.com/profile).

There are several options how you can store token locally:

- copy/paste each time you're using `oceanboltr` package

```{r}
library(oceanboltr)

token <- "<YOUR_OCEANBOLT_TOKEN>"
registerToken(token)
```

- local text file

```{r}
library(oceanboltr)

token <- scan("<PATH_TO_YOUR_OCEANBOLT_TOKEN>", what = "character")
registerToken(token)
```

- your OS’s secure secret store (need to install `keyring` package)

It's recommended way for active users as it doesn't require token registration before each package usage. For `keyring` package installation, please, follow installation instrustions here: (https://github.com/r-lib/keyring).

```{r}
library(oceanboltr)

token <- "<YOUR_OCEANBOLT_TOKEN>"
registerToken(token, type = "keyring")
```

## Usage

TODO
