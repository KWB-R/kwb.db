% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logical.R
\name{isMySQL}
\alias{isMySQL}
\title{Is the Given Database of Type MySQL?}
\usage{
isMySQL(db, ..., con = NULL)
}
\arguments{
\item{db}{database file (*.mdb, *.accdb, *.xls, *.xlsx) or name of ODBC
database}

\item{\dots}{arguments passed to \code{\link{hsOpenDb}}, e.g.
\emph{use2007Driver}}

\item{con}{connection object as returned by \code{\link{hsOpenDb}}, if 
already available. Default: \code{NULL}}
}
\value{
TRUE if \emph{db} is a MySQL database, else FALSE
}
\description{
Is the Given Database of Type MySQL?
}
