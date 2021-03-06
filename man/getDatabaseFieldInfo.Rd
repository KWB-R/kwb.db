% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDatabaseFieldInfo.R
\name{getDatabaseFieldInfo}
\alias{getDatabaseFieldInfo}
\title{Get Information on Table Field Names and Types}
\usage{
getDatabaseFieldInfo(db)
}
\arguments{
\item{db}{path to MS Access or MS Excel file or name of ODBC data source}
}
\value{
data frame with columns \code{TABLE_NAME}, \code{COLUMN_NAME}, 
  \code{TYPE_NAME}, \code{DECIMAL_DIGITS}
}
\description{
Get Information on Table Field Names and Types
}
