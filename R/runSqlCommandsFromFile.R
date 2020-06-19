# runSqlCommandsFromFile -------------------------------------------------------

#' Run SQL Commands from File
#' 
#' @param db Full path to MS Access mdb file or ODBC database name
#' @param sqlFile full path to file containing Ms Access SQL commands
#' @param \dots further arguments passed to \code{\link{hsSqlQuery}}
#' 
runSqlCommandsFromFile <- function(db, sqlFile, ...)
{
  sqlCommands <- readSqlCommandsFromFile(sqlFile)
  
  for (sqlCommand in sqlCommands) {
    
    hsSqlQuery(db, sqlCommand, ...)
  }
}

# readSqlCommandsFromFile ------------------------------------------------------

#' Read SQL Commands from File
#' 
#' Lines starting with "--" or "#" are ignored. SQL commands must be separated
#' by semicolon and end of line character (\\n).
#' 
#' @param sqlScript full path to file containing SQL commands
#' 
readSqlCommandsFromFile <- function(sqlScript)
{
  sqlLines <- readLines(sqlScript)
  
  sqlLines <- grep("^\\s*(#|\\-\\-)", sqlLines, value = TRUE, invert = TRUE)
  
  sqlCode <- paste(sqlLines, collapse = "\n")
  
  sqlCommands <- strsplit(sqlCode, split=";\\s*\n")[[1]]
  
  sqlCommands <- sapply(X = sqlCommands, USE.NAMES = FALSE, FUN = function(x) {
    gsub("(^(\\n|\\s)+)|((\\n|\\s)+$)", "", x)
  })
  
  sqlCommands[sqlCommands != ""]
}
