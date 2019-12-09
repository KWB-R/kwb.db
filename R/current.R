# setCurrentDb -----------------------------------------------------------------

#' Set Current Database
#' 
#' @param db full path to MS Access database or ODBC database name
#' @export
#' 
setCurrentDb <- function(db)
{
  options(kwb.db.current.db = db)
}

# currentDb --------------------------------------------------------------------

#' Get Path to or Name of Current Database
#' 
#' Get Path to or name of current database (as set with 
#' \code{\link{setCurrentDb}})
#' 
#' @param dbg if TRUE, a message obout setting the current database is printed
#' @importFrom kwb.utils catIf
#' @export
#' 
currentDb <- function(dbg = TRUE)
{
  db <- getOption("kwb.db.current.db")
  
  if (is.null(db)) {
    
    clean_stop(
      "Please specify the database or use 'setCurrentDb' to set the current ", 
      "database!"
    )
  }
  
  kwb.utils::catIf(dbg, "\n*** Current database: ", db, "...\n\n")
  
  db
}

# setCurrentSqlDialect ---------------------------------------------------------

#' Set Current SQL Dialect
#' 
#' @param dialectName one of "msaccess", "mysql"
#' @export
#' 
setCurrentSqlDialect <- function(dialectName)
{
  options(kwb.db.current.sql.dialect = dialectName)
}

# getCurrentSqlDialect ---------------------------------------------------------

#' Get Current SQL Dialect
#' 
#' @param warn if TRUE and if no current SQL dialog is stored in the options,
#'   the program stops with an error message
#' @param dbg if TRUE, a message about the current SQL dialect is printed
#' @importFrom kwb.utils catIf
#' @export
#' 
getCurrentSqlDialect <- function(warn = TRUE, dbg = FALSE)
{
  sqlDialect <- getOption("kwb.db.current.sql.dialect")
  
  if (is.null(sqlDialect) && warn) {
    
    clean_stop(
      "Please use setCurrentSqlDialect to set the current SQL dialect first!"
    )
  }
  
  kwb.utils::catIf(dbg, "Current SQL dialect:", sqlDialect, "\n")
  
  sqlDialect
}
