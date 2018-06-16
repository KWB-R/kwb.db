[![Appveyor build status](https://ci.appveyor.com/api/projects/status/m70gtm2010x6hnqi/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-db/branch/master)
[![Build Status](https://travis-ci.org/KWB-R/kwb.db.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.db)
[![codecov](https://codecov.io/github/KWB-R/kwb.db/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.db)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

# kwb.db: Functions Supporting Database Access

This package contains functions that aim at simplifying the data transfer 
between databases and R. It is based on the RODBC package that gives access to
databases that provide an [ODBC](https://docs.microsoft.com/en-us/sql/odbc/reference/what-is-odbc) 
interface. Databases may be Microsoft Access files, Microsoft Excel files or 
any other database that is registered as ODBC source on your local machine.

## Database Access in RODBC

With the RODBC package, you need to open a database connection, run a database
query and close the connection.

## Database Access with this package

With the functions of this package it is not needed any more to open and close a
database connection explicitely; this is done behind the scenes in the 
functions. Instead of a database connection the path to the database file needs 
to be passed to the functions as an argument. 

The main functions are `hsGetTable()` and `hsPutTable()` that transfer data from an MS Access database to a data frame in R and save data from a data frame in R
into a table in an MS Access database, respectively.

Use `hsTables()` to get a list of tables that are available in a database and `hsFields()` to get a list of table fields that are contained in a database 
table.

Take care when getting time series data from an MS Access database, see 
therefore `hsMdbTimeSeries()`. 
