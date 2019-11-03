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
#' @param con connection object as returned by \code{\link{hsOpenDb}}, if 
#'   already available. Default: \code{NULL}
#' @return TRUE if \emph{db} is a MySQL database, else FALSE
#' @export
#' 
isMySQL <- function(db, ..., con = NULL)
{
  # If a connection is given and if it has an attribute "isMySQL", return the
  # value of that attribute
  if (! is.null(con) && ! is.null(is_mysql <- attr(con, "isMySQL"))) {
    return(is_mysql)
  }

  # Otherwise try to check by extension
  if (isExcelFile(db) || isAccessFile(db)) {
    return(FALSE)
  }
  
  sqlDialect <- getCurrentSqlDialect(warn = FALSE)
  
  connection <- try(hsOpenDb(db, ...))
  
  on.exit({
    hsCloseDb(connection)
    setCurrentSqlDialect(sqlDialect)
  })
  
  if (inherits(connection, "try-error")) clean_stop(sprintf(
    "Cannot open '%s' to check if this is a MySQL database!", db
  ))
  
  attr(connection, "isMySQL")
}

# isOdbcDataSource -------------------------------------------------------------

#' @importFrom RODBC odbcDataSources
#' @importFrom odbc32 odbcDataSources
#' 
isOdbcDataSource <- function(db)
{
  data_sources <- (get_odbc_function("odbcDataSources"))()
  
  db %in% names(data_sources)
}
