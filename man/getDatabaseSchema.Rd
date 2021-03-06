% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schema.R
\name{getDatabaseSchema}
\alias{getDatabaseSchema}
\title{Get Database Schema}
\usage{
getDatabaseSchema(db, tableNames = NULL, tableTypes = c("TABLE", "VIEW"))
}
\arguments{
\item{db}{full path to database (*.mdb, *.xls) or name of ODBC database}

\item{tableNames}{optional. Vector of table names of tables to be included}

\item{tableTypes}{types of database objects to be included. Default: 
\code{c("TABLE", "VIEW")}}
}
\value{
list with elements \emph{tables} and \emph{relationships}. Element
  \emph{tables} is a list o named elements with the name representing the
  table names and the elements being lists describing the table...
}
\description{
Get Database Schema
}
