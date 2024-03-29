% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_schemata.R
\name{read_schemata}
\alias{read_schemata}
\title{Read Table Schemata from XML Files}
\usage{
read_schemata(path, reduce = TRUE)
}
\arguments{
\item{path}{path to folder containing xml files}

\item{reduce}{if \code{TRUE} (the default) the returned tables are reduced to 
relevant fields}
}
\description{
This function reads the *_schema.xml files exported to folder TBL by KWB tool 
mdbToTxt.exe
}
