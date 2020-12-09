[![Build Status](https://travis-ci.com/oceanbolt/oceanboltr.svg)](https://travis-ci.com/oceanbolt/oceanboltr)

# R Wrapper for Oceanbolt API

This is an R wrapper for the Oceanbolt API.

## Goal

We should create a wrapper for the REST API in R which exposes wrapper functions for each of the endpoints.
The api should return data in the form of tibbles (or maybe data.tables?) Whatever is easier.

Each function should have a short documentation about the params and the response.

Library name **oceanboltr**

The wrapper should be packaged into a full R library which should be published to CRAN. 
The git repo should have ci/cd functionality to automatically build and release a new version whenever we tag a commit.

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


## Endpoints

The following endpoints should be converted:

#### List endpoints:
| Method | URL   |      Description      |  R Function name |
|----------|----------|:-------------:|------:|
| GET | https://beta.api.oceanbolt.com/v2/entities/countries | returns list of countries | listCountries() |
| GET | https://beta.api.oceanbolt.com/v2/entities/zones | returns list of zones | listZones() |
| GET | https://beta.api.oceanbolt.com/v2/entities/segments | returns list of segments | listSegments() |
| GET | https://beta.api.oceanbolt.com/v2/entities/regions | returns list of regions | listRegions() |
| GET | https://beta.api.oceanbolt.com/v2/entities/commodities | returns list of commodities | listCommodities() |

#### Data endpoints:

| Method | Doc URL   |      Description      |  R Function name |
|----------|----------|:-------------:|------:|
| POST | https://openapi.oceanbolt.com/#operation/getTonnageZone | returns tonnage zone data | getTonnageZoneCount() |
| POST | https://openapi.oceanbolt.com/#tag/fleetspeed | returns fleet speed data | getFleetSpeed() |
| POST | https://openapi.oceanbolt.com/#tag/fleetspeed | returns fleet speed data | getFleetSpeed() |
| POST | https://openapi.oceanbolt.com/#operation/postTradeflowLadenLegs | returns individual trade flows | getTradeFlows()
| POST | https://openapi.oceanbolt.com/#operation/postTradeflowDailyTimeseries | returns trade flows timeseries | getTradeFlowsTimeseries()


## Error handling

When the API returns status code > 400 the error code should be clearly printed in the console

## Function signature for data functions

The data endpoint functions should have default values for all parameters, which are in turn submitted is payload.

The signature should ideally be `{r} getTradeFlows(segment=c("panamax","supramax"),fromDate="2020-01-01")`

I have drafted up a very quick proof of concept R script with ideas and examples of how the function signatures could be. This is available in this repo.


