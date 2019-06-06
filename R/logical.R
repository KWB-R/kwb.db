# is64BitR ---------------------------------------------------------------------

is64BitR <- function()
{
  .Machine$sizeof.pointer == 8
}

# isAccess2007File -------------------------------------------------------------

isAccess2007File <- function(filepath)
{
  grepl("\\.accdb$", filepath)
}

# isAccess2003File -------------------------------------------------------------

isAccess2003File <- function(filepath)
{
  grepl("\\.mdb$", filepath)
}

# isAccessFile -----------------------------------------------------------------

isAccessFile <- function(filepath)
{
  isAccess2003File(filepath) || isAccess2007File(filepath)
}

# isExcel2007File --------------------------------------------------------------

#' Is this an XLSX file?
#' 
#' @param filepath (vector of) path(s) to the file(s) to be checked for .xlsx
#'   extension
#' 
#' @return (vector of) logical. 
#' 
isExcel2007File <- function(filepath)
{
  grepl("\\.xlsx$", filepath, ignore.case = TRUE)
}

# isExcel2003File --------------------------------------------------------------

#' Is this an XLS file?
#' 
#' @param filepath (vector of) path(s) to the file(s) to be checked for .xls
#'   extension
#' 
#' @return (vector of) logical. 
#' 
isExcel2003File <- function(filepath)
{
  grepl("\\.xls$", filepath, ignore.case = TRUE)
}

# isExcelFile ------------------------------------------------------------------

#' Is this an Excel file?
#' 
#' @param filepath (vector of) path(s) to the file(s) to be checked for .xls
#'   or .xlsx extension
#' 
#' @return (vector of) logical. 
#' @export
#' 
isExcelFile <- function(filepath)
{
  isExcel2003File(filepath) || isExcel2007File(filepath)
}

# isMySQL ----------------------------------------------------------------------

#' Is the Given Database of Type MySQL?
#' 
#' @param db database file (*.mdb, *.accdb, *.xls, *.xlsx) or name of ODBC
#'   database
#' @param \dots arguments passed to \code{\link{hsOpenDb}}, e.g.
#'   \emph{use2007Driver}
#' 
#' @return TRUE if \emph{db} is a MySQL database, else FALSE
#' 
isMySQL <- function(db, ...)
{
  sqlDialect <- getCurrentSqlDialect(warn = FALSE)
  
  connection <- hsOpenDb(db, ...)
  
  on.exit({
    hsCloseDb(connection)
    setCurrentSqlDialect(sqlDialect)
  })
  
  attr(connection, "isMySQL")
}

# isOdbcDataSource -------------------------------------------------------------

isOdbcDataSource <- function(db)
{
  db %in% names(RODBC::odbcDataSources())
}
