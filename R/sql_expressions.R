# hsSqlExOr --------------------------------------------------------------------

#' SQL Expression \dQuote{OR}
#' 
#' Creates a boolean expression of the form \cr \code{bFunc(x1) OR bFunc(x2) OR 
#' ... OR bFunc(xn)}. This function can be used to create SQL queries where many
#' table fields have to be checked in the same way for some criterion (see
#' example).
#' 
#' @param x vector of strings, e.g. representing table field names.
#' @param bFunc name of a boolean function to be \dQuote{applied} to each
#'   element of \emph{x}.
#'   
#' @seealso \code{\link{hsSqlExAnd}}
#' @export
#' @examples 
#' ## Build SQL query finding records in table t in which at least
#' ## one of the table fields f1 to f100 is NULL.
#' sql <- sprintf("SELECT * FROM t WHERE %s", 
#'                hsSqlExOr(paste("f", 1:100, sep = ""), "isNull"))
#'   
#' ## Show the SQL string
#' sql
#'   
#' ## Output (middle part omitted):
#' # SELECT * FROM t WHERE (FALSE) OR isNull(f1) OR 
#' # isNull(f2) OR isNull(f3) OR ... OR isNull(f100)
#' 
hsSqlExOr <- function(x, bFunc = "")
{
  sprintf("(%s)", paste(c("FALSE", x), collapse = sprintf(") OR %s(", bFunc)))
}

# hsSqlExAnd -------------------------------------------------------------------

#' SQL Expression \dQuote{AND}
#' 
#' Creates a boolean expression of the form \cr \code{bFunc(x1) AND bFunc(x2)
#' AND ... AND bFunc(xn)}. This function can be used to create SQL queries where
#' many table fields have to be checked in the same way for some criterion (see
#' example).
#' 
#' @param x vector of strings, e.g. representing table field names.
#' @param bFunc name of a boolean function to be \dQuote{applied} to each
#'   element of \emph{x}.
#'   
#' @seealso \code{\link{hsSqlExOr}}
#' @export 
#' @examples 
#' ## Build SQL query finding records in table t in which all
#' ## of the table fields f1 to f100 are NULL.
#' sql <- sprintf("SELECT * FROM t WHERE %s", 
#'                hsSqlExAnd(paste("f", 1:100, sep = ""), "isNull"))
#'   
#' ## Show the SQL string
#' sql
#'   
#' ## Output (middle part omitted):
#' # SELECT * FROM t WHERE (TRUE) AND isNull(f1) AND 
#' # isNull(f2) AND isNull(f3) AND ... AND isNull(f100)
#' 
hsSqlExAnd <- function(x, bFunc) 
{
  sprintf("(%s)", paste(c("TRUE", x), collapse = sprintf(") AND %s(", bFunc)))
}

# hsSqlExTimeCond --------------------------------------------------------------

#' SQL Expression: Time Period
#' 
#' WHERE-condition string in MS Jet SQL syntax filtering for a specific 
#'   time interval
#' 
#' @param tsField name of timestamp field
#' @param dateFirst Date object representing the first date of the time interval
#'   to be selected.
#' @param dateLast Date object representing the last date of the time interval
#'   to be selected.
#' @param inclLast if \code{TRUE}, dateLast will be included in result data set,
#'   else excluded.
#' @param sqlDialect one of \code{c("mysql", "msaccess")}
#' @param dbg if \code{TRUE}, debug messages are shown
#' @return Condition string in MS Jet SQL syntax to be used in WHERE clause
#' 
#' @seealso \code{\link{hsJetDate}}
#' 
#' @examples
#' \dontrun{
#' ## Condition string to filter field "datetime" for timestamps 
#' ## between 21 and 22 of July 2010
#' from <- as.Date("2011-08-23")
#' to   <- as.Date("2011-08-24")
#' cond <- hsSqlExTimeCond("Zeitst", from, to)
#' cond  
#'   
#' ## Output:
#' # TRUE  AND Zeitst >= #08/23/2011 00:00:00#  
#' #       AND Zeitst <= #08/24/2011 00:00:00#
#'   
#' ## The condition string may now be used in an SQL query 
#' ## to select data from within the time interval. 
#' sql <- sprintf("SELECT * FROM tbl_Hyd WHERE %s", cond)
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   res <- hsSqlQuery(xmdb(), sql)
#'   head(res)
#' }
#'    
#' ## Output:
#' # Zeitst Q v     H T_Kanal
#' # 1 2011-08-23 00:00:00 0 0 1.260    19.5
#' # 2 2011-08-23 00:01:00 0 0 1.259    19.5
#' # 3 2011-08-23 00:02:00 0 0 1.259    19.5
#' # 4 2011-08-23 00:03:00 0 0 1.259    19.5
#' # 5 2011-08-23 00:04:00 0 0 1.260    19.5
#' # 6 2011-08-23 00:05:00 0 0 1.260    19.5
#' }
#' 
hsSqlExTimeCond <- function(
  tsField, dateFirst = NULL, dateLast = NULL, inclLast = TRUE,
  sqlDialect = getCurrentSqlDialect(), dbg = FALSE
)
{
  ## Init condition string
  cond <- " TRUE "
  
  ## Append condition <tsField> >= <dateFirst>
  if (! (missing(dateFirst) || is.null(dateFirst) || 
         (is.character(dateFirst) && dateFirst == ""))) {
    
    sqlDate <- sqlDateExpression(dateFirst, sqlDialect = sqlDialect, dbg)
    
    cond <- paste(cond, "AND", tsField, ">=", sqlDate)
  }
  
  ## Append condition <tsField> <= <dateLast>
  if (! (missing(dateLast) || is.null(dateLast) || 
         (is.character(dateLast) && dateLast == ""))) {
    
    sqlDate <- sqlDateExpression(dateLast, sqlDialect = sqlDialect, dbg)
    
    operator <- ifelse(inclLast, "<=", "<")
    cond <- paste(cond, "AND", tsField, operator, sqlDate)
  }
  
  cond
}

# sqlDateExpression ------------------------------------------------------------

sqlDateExpression <- function(dateObject, sqlDialect, dbg = FALSE)
{
  if (sqlDialect == "msaccess") {
    
    paste0("#", hsJetDate(dateObject, dbg), "#")
    
  } else {
    
    paste0("'", format.Date(dateObject), "'")
  }
}

# hsSqlExTimeGroup -------------------------------------------------------------

#' SQL Expression: Time Grouping
#' 
#' Generates SQL code for grouping timestamps by years, months or days
#' 
#' @param tbl name of the table
#' @param tsField name of the table field containing the timestamp
#' @param interval specifies the time period to group by ("y": years, "m":
#'   months, "d": days)
#' @param cond additional condition in SQL syntax
#'   
#' @return Returns SQL code for grouping timestamps by years, months or days
#'   
#' @seealso \code{\link{hsSqlExTimeCond}}
#' @export
#' @examples 
#' ## Show SQL query that gets the number of datasets per 
#' ## day ("d") considering the condition "Q > 0"
#' hsSqlExTimeGroup("myTable", "myTimestamp", "d", "Q > 0")
#'   
#' ## Output (reformatted):  
#' ## SELECT DateSerial(Year(hsTS), Month(hsTS), Day(hsTS)) 
#' ##   AS myInterval, Count(*) AS myCount 
#' ## FROM (
#' ##     SELECT myTimestamp AS hsTS FROM myTable WHERE Q > 0
#' ## ) 
#' ## GROUP BY DateSerial(Year(hsTS), Month(hsTS), Day(hsTS)) 
#' ## ORDER BY DateSerial(Year(hsTS), Month(hsTS), Day(hsTS))
#' 
hsSqlExTimeGroup <- function(tbl, tsField, interval, cond = "TRUE")
{
  if (interval == "y") {
    groupBy <- "DateSerial(Year(hsTS), 1, 1)"
  } else if (interval == "m") {
    groupBy <- "DateSerial(Year(hsTS), Month(hsTS), 1)"
  } else if (interval == "d") {
    groupBy <- "DateSerial(Year(hsTS), Month(hsTS), Day(hsTS))"
  } else {
    stop("Unknown interval character: ", interval)
  }
  
  # Create SQL query
  subSql <- sprintf("SELECT %s AS hsTS FROM [%s] WHERE %s", tsField, tbl, cond)
  
  expr <- sprintf("%s AS myInterval, Count(*) AS myCount", groupBy)
  sql <- sprintf("SELECT %s FROM (%s) GROUP BY %s ORDER BY %s", 
                 expr, subSql, groupBy, groupBy)
  
  
  sql
}

# hsSqlExTsFields --------------------------------------------------------------

#' SQL Expression: Timestamp Info
#' 
#' Generates SQL code for selecting different parts of the timestamp
#' 
#' @param tsField name of the table field containing the timestamp
#' @param extraTsFields vector of integers representing different types of
#'   time-stamp information:\cr 0 = the timestamp in POSIXct as it is converted
#'   to by R from the text representation delivered by MS Access, \cr 1 =
#'   Timestamp as character string, \cr 2 = Date, \cr 3 = number of days since
#'   1899-12-30, \cr 4 = number of seconds since midnight of Date \cr 5 = number
#'   of minutes since midnight of Date \cr 6:8 = year, month, day \cr 9:11 =
#'   hours, minutes, seconds \cr
#' 
#' @return Returns SQL code for selecting different types of information on 
#'   the time stamp.
#' 
#' @seealso \code{\link{hsGetTimeSeries}}
#' @export
#' @examples 
#' hsSqlExTsFields("myTimestamp", c(6:11))
#'   
#' ## Output (re-formatted):
#' # "Year(myTimestamp) AS myTimestamp_year, 
#' #   Month(myTimestamp) AS myTimestamp_month, 
#' #   Day(myTimestamp) AS myTimestamp_day, 
#' #   Hour(myTimestamp) AS myTimestamp_h, 
#' #   Minute(myTimestamp) AS myTimestamp_m, 
#' #   Second(myTimestamp) AS myTimestamp_s"
#' 
hsSqlExTsFields <- function(tsField, extraTsFields = 0:11) 
{
  fields <- character()
  i <- 0
  
  # Expression for SQL string
  
  # Timestamp itself
  if (0 %in% extraTsFields) {
    
    fields[i <- i+1] <- tsField
  }
  
  # Timestamp as Text
  if (1 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("CStr(%s) AS %s_txt", tsField, tsField)
  }
  
  # Date only
  if (2 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("int(%s) AS %s_Date", tsField, tsField)
  }
  
  # Number of days since 1899-12-30
  if (3 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf(
      "1*int(%s) AS %s_dSince18991230", tsField, tsField
    )
  }
  
  # Seconds since Midnight of Date
  if (4 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf(
      "Round(86400*(%s - int(%s))) AS %s_secInDay", tsField, tsField, tsField
    )
  }
  
  # Minutes since midnight of Date
  if (5 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf(
      "Round(1440*(%s - int(%s))) AS %s_minInDay", 
      tsField, tsField, tsField
    )
  }
  
  # Year
  if (6 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("Year(%s) AS %s_year", tsField, tsField)
  }
  
  # Month
  if (7 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("Month(%s) AS %s_month", tsField, tsField)
  }
  
  # Day
  if (8 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("Day(%s) AS %s_day", tsField, tsField)
  }
  
  # Hours
  if (9 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("Hour(%s) AS %s_h", tsField, tsField)
  }
  
  # Minutes
  if (10 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("Minute(%s) AS %s_m", tsField, tsField)
  }
  
  # Seconds
  if (11 %in% extraTsFields) {
    
    fields[i <- i+1] <- sprintf("Second(%s) AS %s_s", tsField, tsField)
  }
  
  paste(fields, collapse = ", ")
}
