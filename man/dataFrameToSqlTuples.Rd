% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql.R
\name{dataFrameToSqlTuples}
\alias{dataFrameToSqlTuples}
\title{Create SQL Tuples from Data Frame}
\usage{
dataFrameToSqlTuples(newData)
}
\arguments{
\item{newData}{a data frame}
}
\value{
vector of character strings each of which represents one row in
  \code{newData}
}
\description{
Create SQL Tuples from Data Frame
}
\examples{
x <- data.frame(
  name = c("Peter", "Paul"), 
  birthday = as.POSIXct(c("1981-12-13", "2003-01-16"))
)

setCurrentSqlDialect("msaccess")
dataFrameToSqlTuples(x)

# Note that the representation of a date and time is different in MySQL
setCurrentSqlDialect("mysql")
dataFrameToSqlTuples(x)

}
