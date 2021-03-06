% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{hsTsField}
\alias{hsTsField}
\title{Available timestamp-field(s) in database table}
\usage{
hsTsField(src, tbl, namesOnly = TRUE, all = FALSE)
}
\arguments{
\item{src}{source file (MS Access or Excel)}

\item{tbl}{table name}

\item{namesOnly}{if TRUE, only the name(s) of the timestamp field(s) is (are)
returned, otherwise additional information.}

\item{all}{if TRUE, all timestamp fields are considiered, otherwise only the
first timestamp field.}
}
\description{
Available timestamp-field(s) in database table
}
