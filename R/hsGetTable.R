# hsGetTable -------------------------------------------------------------------

#' Get Table from MS Access Database
#' 
#' Provides data from an MS Access database table in forms of a data frame.
#' 
#' ATTENTION: This function may not return what you want if the table contains a
#' timestamp field. Use \code{hsMdbTimeSeries} instead.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param tbl Table name. Put it into brackets [] if it contains spaces and if
#'   it mdb does not point to a MySQL database
#' @param cond Condition string.
#' @param fields Comma separated list of names of fields to be selected.
#' @param dbg if TRUE, debug messages are shown, else not
#' @param check if TRUE (default), \emph{tbl} is checked for existence in
#'   \emph{mdb} before trying to get the data and a list of available tables is
#'   shown in the case that the table does not exist.
#' @param use2007Driver passed to \code{\link{hsTables}} and 
#'   \code{\link{hsSqlQuery}}
#' @param \dots Additional arguments to be passed to hsSqlQuery
#'   
#' @return data.frame containing data from table in database
#' 
#' @seealso \code{\link{hsSqlQuery}, \link{hsPutTable}, hsGetTimeSeries,
#'   hsMdbTimeSeries}
#' @importFrom kwb.utils hsQuoteChr
#' @export
#' @examples
#' \dontrun{ 
#' ## Get all datasets from tbl_Hyd in example database where
#' ## Q > 1.0 m3/s and temperature > 20 degree Celsius
#' ## (only on Windows!)
#' 
#' if (.Platform$OS.type == "windows") {
#'   ts <- hsGetTable(xmdb(), "tbl_Hyd", "Q > 1.0 AND T_Kanal > 20")
#'   head(ts)
#' } 
#'  
#' ## Output:
#' # Zeitst     Q     v     H T_Kanal
#' # 1 2011-08-24 22:33:00 1.075 0.459 1.366    20.1
#' # 2 2011-08-24 22:34:00 1.062 0.453 1.370    20.2
#' # 3 2011-08-24 22:35:00 1.050 0.449 1.364    20.2
#' # 4 2011-08-24 22:36:00 1.042 0.446 1.361    20.3
#' # 5 2011-08-24 22:37:00 1.032 0.443 1.354    20.3
#' # 6 2011-08-24 22:38:00 1.010 0.436 1.348    20.4
#'   
#' ## TAKE CARE when getting time-series data:
#' if (.Platform$OS.type == "windows") {
#'   hsGetTable(xmdb(), "tblTimestampTest_DST")
#' }
#'    
#' ## Output:
#' #                tstamp
#' # 1 2011-03-27 01:00:00
#' # 2 2011-03-27 01:30:00
#' # 3                <NA>
#' # 4                <NA>
#' # 5 2011-03-27 03:00:00
#' # 6 2011-03-27 03:30:00
#'   
#' ## As the output shows the timestamps between 02:00:00 and
#' ## 02:59:59 have been set to <NA>. Reason: When retrieving
#' ## date/time data from MS Access, R converts the timestamps
#' ## from a text representation into POSIXct objects. As POSIXct's
#' ## standard time zone seems to be taken from the Windows system
#' ## R tries to convert to Central European Time (CET) which
#' ## does not exist for the hour in which time is switched to
#' ## daylight-saving time (as in the example).
#'   
#' ## This standard behaviour can be changed by setting the
#' ## standard time zone:
#' tz <- Sys.getenv("tz") # save current standard time zone
#' Sys.setenv(tz = "UTC") # set standard time zone to UTC
#'   
#' ## The same command as above now delivers all timestamps
#' ## (in Coordinated Universal Time, UTC):
#' 
#' if (.Platform$OS.type == "windows") {
#'   hsGetTable(xmdb(), "tblTimestampTest_DST")
#' }
#'    
#' ## Output:
#' #                tstamp
#' # 1 2011-03-27 01:00:00
#' # 2 2011-03-27 01:30:00
#' # 3 2011-03-27 02:00:00
#' # 4 2011-03-27 02:30:00
#' # 5 2011-03-27 03:00:00
#' # 6 2011-03-27 03:30:00
#' 
#' ## Reset standard time zone
#' Sys.setenv(tz = tz)
#' }
#' 
hsGetTable <- function(
  mdb, tbl, cond = "TRUE", fields = "*", dbg = TRUE, check = TRUE, 
  use2007Driver = NULL, ...
)
{
  if (missing(tbl) || check) {
    
    availableTableNames <- hsTables(
      mdb, namesOnly = TRUE, use2007Driver = use2007Driver
    )
  }
  
  # Help the user by showing a list of available table names
  if (missing(tbl)) {
    
    stop("No table name given. ",.message_availableTables(availableTableNames))
  }
  
  if (check) {
    
    # remove possible quotes embracing the table name
    availableTableNames <- gsub("^'(.*)'$", "\\1", availableTableNames)
    
    if (! (tbl %in% availableTableNames)) {
      
      stop(
        "Table ", kwb.utils::hsQuoteChr(tbl), " does not exist in ", 
        kwb.utils::hsQuoteChr(mdb), ".\n", 
        .message_availableTables(availableTableNames)
      )
    }
  }
  
  # Return result of SQL query
  is.MySql <- isMySQL(mdb, use2007Driver = use2007Driver)
  
  sql <- sqlForSelect(
    tablename = tbl, fields = fields, whereClause = cond,
    sqlDialect = ifelse(is.MySql, "mysql", "msaccess")
  )
  
  hsSqlQuery(mdb, sql = sql, dbg = dbg, use2007Driver = use2007Driver, ...)
}
