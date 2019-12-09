# hsSqlQuery -------------------------------------------------------------------

#' Send SQL Query to Database
#' 
#' Get data from database requested via an SQL query. This function performs
#' opening of the connection, data retieval via SQL and closing of the
#' connection.  If an error occurs the program stops and an error description is
#' shown.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param sql SQL query
#' @param use2007Driver if TRUE the functions odbcConnectAccess2007 and
#'   odbcConnectExcel2007 are used instead of odbcConnectAccess and
#'   odbcConnectExcel, respectively
#' @param dbg if TRUE (default), debug messages are shown.
#' @param stopOnError if TRUE (default), the program stops in case of an error,
#'   otherwise a warning is shown and NULL is returned.
#' @param DBMSencoding finally passed to \code{odbcDriverConnect}. Default: "",
#'   You may want to use: "UTF-8"
#' @param \dots additional arguments to be passed to \code{sqlQuery}
#'   
#' @return On success, a data.frame containing the data that is internally
#'   requested by calling the RODBC function sqlQuery and that is provided by
#'   the database is returned.  On error R stops execution and does not return
#'   anything.
#'   
#' @seealso \code{\link{hsPutTable}, \link{hsGetTable}}
#' @importFrom kwb.utils catIf
#' @export
#' @examples
#' \dontrun{
#' ## Get Q time series from table "tbl_Hyd" in example database
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   tsQ <- hsSqlQuery(
#'     xmdb(), "SELECT Zeitst AS t, Q FROM tbl_Hyd WHERE Q > 1.0"
#'   )
#'    
#'   ## Show the first lines of the resulting data.frame
#'   head(tsQ)
#' }
#'    
#' ## Output
#' # t     Q
#' # 1 2011-08-24 22:27:00 1.061
#' # 2 2011-08-24 22:28:00 1.091
#' # 3 2011-08-24 22:29:00 1.115
#' # 4 2011-08-24 22:30:00 1.092
#' # 5 2011-08-24 22:31:00 1.086
#' # 6 2011-08-24 22:32:00 1.074
#' }
#' 
hsSqlQuery <- function(
  mdb, sql, use2007Driver = NULL, dbg = TRUE, stopOnError = TRUE, 
  DBMSencoding = "", ...
)
{
  # Store current sql dialect
  sqlDialect <- getCurrentSqlDialect(warn = FALSE)
  
  ## Open database connection, on exit close it and reset the sql dialect
  con <- hsOpenDb(
    mdb, use2007Driver = use2007Driver, DBMSencoding = DBMSencoding
  )
  
  on.exit({
    hsCloseDb(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  ## Send SQL query
  kwb.utils::catIf(dbg, sprintf("\nRunning SQL: %s\n\n", sql))
  
  # Try to use the RODBC-function first
  res <- try(RODBC::sqlQuery(con, sql, ...))
  
  # Try to use the odbc32-function next
  if (inherits(res, "try-error")) {
    res <- odbc32::sqlQuery(con, sql, ...)
  }
  
  # Did an error occur?
  if ((class(res) == "character") && (length(res) > 0)) {
    
    msg <- paste(res, collapse = "\n")
    
    if (stopOnError) {
      
      clean_stop(msg)
      
    } else {
      
      warning(msg)
      return (NULL)
    }
  }
  
  # Did we get data?
  kwb.utils::catIf(dbg, sprintf(
    "The query returned %d records with %d fields: %s\n",
    nrow(res), ncol(res), paste(names(res), collapse = ",")
  ))
  
  ## Return result data.frame and suppress character results
  if (class(res) == "data.frame") res
}
