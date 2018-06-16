
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/m70gtm2010x6hnqi/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-db/branch/master) [![Build Status](https://travis-ci.org/KWB-R/kwb.db.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.db) [![codecov](https://codecov.io/github/KWB-R/kwb.db/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.db) <!-- [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) -->

kwb.db
======

This repository contains the R package kwb.db. The package provides functions that aim at simplifying the data transfer between databases and R. It is based on the [RODBC](https://cran.r-project.org/web/packages/RODBC/) package that gives access to databases that provide an [ODBC](https://docs.microsoft.com/en-us/sql/odbc/reference/what-is-odbc) interface. Databases may be Microsoft Access files, Microsoft Excel files or any other database that is registered as an ODBC data source on your local machine. See e.g. [here](https://docs.microsoft.com/en-us/sql/odbc/admin/odbc-data-source-administrator) for how to setup ODBC data sources in Windows.

Installation
------------

Once you have the devtools package installed, you can install the package kwb.db like this:

``` r
# If required, install the devtools package...
# install.packages("devtools")

# Install kwb.db (and dependent packages) from this GitHub repository
devtools::install(github("kwb-r/kwb.db", dependencies = TRUE)
```

Database Access in RODBC
------------------------

With the RODBC package, you need to open a database connection, send one or more requests to the database and finally close the dabase connection.

Database Access with this package
---------------------------------

With the functions of this package it is not needed to open and close a database connection explicitly; this is done behind the scenes in the functions. Instead of a database connection the path to the database file needs to be passed to the functions as an argument.

The main functions are [`hsGetTable()`](docs/reference/hsGetTable.html) and [`hsPutTable()`](docs/reference/hsPutTable.html). They transfer data from a database to a data frame in R and save data from a data frame in R into a new table in a database, respectively.

Use [`hsTables()`](docs/reference/hsTables.html) to get a list of tables that are available in a database and [`hsFields()`](docs/reference/hsFields.html) to get a list of table fields that are contained in a database table.

A general workflow could look like this:

``` r
# Define the path to a MS Access datbase file
mdb <- "/path/to/your/database.mdb"

# Have a look at what tables are contained in the database
(tables <- kwb.db::hsTables(mdb))

# For each table, get the vector of available fields (= columns)
lapply(tables, kwb.db::hsFields, mdb = mdb)

# Get the content of the first table
data <- kwb.db::hsGetTable(mdb, tables[1])

# Do some modifiactions or create somehow else a new data frame
data_new <- do_some_fancy_stuff(data)

# Save the new data frame as a new "fancy_table" in the database
kwb.db::hsPutTable(mdb, data_new, "fancy_table")
```

In each of the `kwb.db::`-function calls above a database connection is opened, a request to the database is sent and the connection is closed again. Thus, the user does not have to care about open database connections.

Take care when getting time series data from an MS Access database, see therefore [`hsMdbTimeSeries()`](docs/reference/hsMdbTimeSeries.html).
