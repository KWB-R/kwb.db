# hsFields ---------------------------------------------------------------------

#' Available Fields in Database Table
#' 
#' Returns a vector containing the field names of a database table.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param tbl table name.
#' @param namesOnly if TRUE, only field names are returned, otherwise all
#'   available information on the fields. Default: TRUE
#' @param chopDollar if TRUE (default), a dollar sign at the end of the table
#'   name is removed before sending it to \code{sqlColumns},
#' @param ignore.case if TRUE, case is ignored when comparing the given table
#'   with the names of the existing tables. Default: FALSE
#' @param use2007Driver passed to \code{\link{isMySQL}}
#' @param dbg if TRUE, debug messages are shown
#'   
#' @return Vector containing the field names of the database table (if namesOnly
#'   = TRUE) or data.frame with columns \emph{COLUMN_NAME}, \emph{DATA_TYPE},
#'   \emph{TYPE_NAME}, \emph{COLUMN_SIZE}, \emph{BUFFER_LENGTH},
#'   \emph{DECIMAL_DIGITS}, \emph{NUM_PREC_RADIX}, \emph{NULLABLE} describing
#'   the database fields in detail, otherwise.
#' 
#' @seealso \code{\link{hsTables}}
#' @importFrom RODBC sqlColumns
#' @export
#' @examples
#' \dontrun{ 
#' ## List the fields of table "tbl_Hyd" in the example database
#' ## (only on Windows!)
#' 
#' if (.Platform$OS.type == "windows") {
#'   setCurrentSqlDialect("msaccess")
#'   fields <- hsFields(xmdb(), "tbl_Hyd")
#'   fields
#' }
#'   
#' ## Ouput:
#' # [1] "Zeitst"  "Q"       "v"       "H"       "T_Kanal"
#' }
#' 
hsFields <- function(
  mdb, tbl, namesOnly = TRUE, chopDollar = TRUE, 
  ignore.case = (! isMySQL(mdb, use2007Driver = use2007Driver)), 
  use2007Driver = NULL, dbg = FALSE
)
{
  ## if no table name is given stop with showing a list of available tables
  tbls <- hsTables(mdb, namesOnly = TRUE)
  
  msg <- sprintf("Available tables:\n  %s", paste(
    '"', tbls, '"', sep = "", collapse = '\n  '
  ))
  
  if (missing(tbl)) {
    
    clean_stop("No table name given. ", msg)
  }
  
  pattern <- paste("^", tbl, "$", sep = "")
  
  if (length(grep(pattern, tbls, ignore.case = ignore.case)) != 1) {
    
    clean_stop(sprintf('Table "%s" not found in %s. ', tbl, mdb), msg)
  }
  
  sqlDialect <- getCurrentSqlDialect(warn = FALSE)
  
  con <- hsOpenDb(mdb)
  
  on.exit({
    hsCloseDb(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  if (chopDollar) {
    
    tbl <- sub("\\$$", "", tbl)
  }
  
  fieldInfo <- try(RODBC::sqlColumns(con, tbl))
  
  if (inherits(fieldInfo, "try-error")) {
    
    msg <- paste("Error when calling RODBC::sqlColumns():", fieldInfo)
    
    if (is64BitR()) {
      
      msg <- paste0(
        msg, "\nYou are using the 64 bit version of R. You may try to run the ", 
        "32 bit version of R. Unfortunately there is no equivalent to ", 
        "sqlColumns() in odbc32."
      )
    }

    clean_stop(msg)
  }
  
  if (namesOnly) {
    
    return(fieldInfo$COLUMN_NAME)
    
  } else {
    
    ## omit first three (table related) columns of data frame
    return(fieldInfo[, -c(1:3)])
  }
}