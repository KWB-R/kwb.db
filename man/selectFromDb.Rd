% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectFromDb.R
\name{selectFromDb}
\alias{selectFromDb}
\title{Select from Database}
\usage{
selectFromDb(
  tableName,
  fields = "*",
  whereClause = "TRUE",
  odbc,
  dbg = TRUE,
  ...
)
}
\arguments{
\item{tableName}{name of database table from which to load data}

\item{fields}{names of fields to be selected}

\item{whereClause}{SQL WHERE condition string}

\item{odbc}{database name or file}

\item{dbg}{if \code{TRUE}, debug messages are shown}

\item{\dots}{additonal arguments passed to \code{\link{hsSqlQuery}}}
}
\description{
Select from Database
}
