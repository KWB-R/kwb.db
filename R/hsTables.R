# hsTables ---------------------------------------------------------------------

#' Available tables in database
#' 
#' Returns a data.frame as returned by sqlTables, containing information on the 
#' tables contained in the database.  Opening of the database connection, 
#' getting the list of tables and closing of the database connection is done 
#' within this function.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param excludeSystemTables if TRUE (default), system tables are excluded from
#'   the table list, else included.
#' @param namesOnly if TRUE, only table names are returned. Default: TRUE
#' @param use2007Driver if TRUE the functions odbcConnectAccess2007 and
#'   odbcConnectExcel2007 are used instead of odbcConnectAccess and
#'   odbcConnectExcel, respectively
#' @param dbg if TRUE, debug messages are shown
#'   
#' @return data.frame with columns \emph{TABLE_CAT}, \emph{TABLE_SCHEM}, 
#'   \emph{TABLE_NAME}, \emph{TABLE_TYPE}, \emph{REMARKS}, see sqlTables of 
#'   RODBC package.
#'   
#' @seealso \code{\link{hsFields}}
#' @importFrom kwb.utils catIf
#' @importFrom RODBC sqlTables 
#' @export
#' @examples
#' \dontrun{
#' ## Get names of tables in the example database
#' ## (only on Windows)
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   tnames <- hsTables(xmdb(), namesOnly = TRUE)
#'   
#'   ## Exclude system tables by filtering for table names
#'   ## not starting with '^MSys'
#'   tNonSys <- grep("^MSys", tnames, invert = TRUE, value = TRUE)
#'   
#'   ## Print the names of the non-system tables.
#'   cat(paste(tNonSys, "\n"))
#' }
#'    
#' ## Ouput:
#' # tbl_Hyd
#' #  tbl_Qua
#' #  ...
#' }
#' 
hsTables <- function(
  mdb, excludeSystemTables = grepl("\\.(mdb|accdb)$", mdb), namesOnly = TRUE,
  use2007Driver = NULL, dbg = FALSE
)
{
  kwb.utils::catIf(dbg, "in hsTables: use2007Driver =", use2007Driver, "\n")
  
  sqlDialect = getCurrentSqlDialect(warn = FALSE)
  
  ## Open database connection and close it on exit
  con <- hsOpenDb(mdb, use2007Driver = use2007Driver)
  
  on.exit({
    hsCloseDb(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  tblList <- (get_odbc_function("sqlTables"))(con)
  
  if (excludeSystemTables) {
    tblList <- tblList[tblList$TABLE_TYPE != "SYSTEM TABLE", ]
  }
  
  if (namesOnly) {
    tblList$TABLE_NAME
  } else {
    tblList
  }
}
