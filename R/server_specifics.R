# .hsJetType -------------------------------------------------------------------

.hsJetType <- function(rtype)
{
  if (rtype == "character") {
    
    "VARCHAR(255)"
    
  } else if (rtype == "integer") {
    
    "INTEGER"
    
  } else if (rtype == "numeric") {
    
    "DOUBLE"
    
  } else if (rtype == "logical") {
    
    "VARCHAR(5)"
    
  } else if (rtype %in% c("POSIXt", "POSIXct", "POSIXlt")) {
    
    "DATETIME"
    
  } else {
    
    "TEXT"
  }
}

# .hsMySqlType -----------------------------------------------------------------

.hsMySqlType <- function(rtype)
{
  if (rtype == "character") {
    
    "varchar(255)"
    
  } else if (rtype == "integer") {
    
    "integer"
    
  } else if (rtype == "numeric") {
    
    "double"
    
  } else if (rtype == "logical") {
    
    "varchar(5)"
    
  } else if (rtype %in% c("POSIXt", "POSIXct", "POSIXlt")) {
    
    "datetime"
    
  } else {
    
    "TEXT"
  }
}

# hsJetDate --------------------------------------------------------------------

#' Date to \dQuote{mm/dd/yyyy HH:MM:SS}
#' 
#' Returns a date in MS Jet SQL syntax: mm/dd/yyyy HH:MM:SS
#' 
#' @param datetime Date (and time) information in forms of Date object or POSIX
#'   object or string.
#' @param dbg if TRUE, debug messages are shown
#' @importFrom kwb.utils catIf
#' @importFrom kwb.datetime hsToPosix
#'  
hsJetDate <- function(datetime, dbg = FALSE)
{
  dFormat <- "%m/%d/%Y %H:%M:%S" # This is a date in MS Jet SQL syntax
  
  cls <- class(datetime)

  kwb.utils::catIf(dbg, sprintf("class(datetime) = %s\n", cls))

  format.Date(kwb.datetime::hsToPosix(datetime), dFormat)
}
