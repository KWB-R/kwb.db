# selectFromDb -----------------------------------------------------------------

#' Select from Database
#' 
#' @param tableName name of database table from which to load data
#' @param fields names of fields to be selected
#' @param whereClause SQL WHERE condition string
#' @param odbc database name or file
#' @param dbg if \code{TRUE}, debug messages are shown
#' @param \dots additonal arguments passed to \code{\link{hsSqlQuery}}
#' 
selectFromDb <- function(
  tableName, fields = "*", whereClause = "TRUE", odbc, dbg = TRUE, ...
)
{
  tableName <- .toExistingTableName(tableName, db = odbc)
  hsSqlQuery(odbc, sqlForSelect(tableName, fields, whereClause), dbg = dbg, ...)
}

# .toExistingTableName ---------------------------------------------------------
.toExistingTableName <- function(tableName, db)
{
  # check if the table name exists in the database and, if not, try to find
  # a table with matching name
  tableNames <- hsTables(db, namesOnly = TRUE)
  
  if (!(tableName %in% tableNames)) {
    
    pattern <- sprintf("^%s$", tableName)
    matching <- grep(pattern, tableNames, ignore.case = TRUE)
    
    n <- length(matching)
    
    if (n == 0) {
      
      clean_stop(sprintf(
        "There is no table with a name matching '%s' in database '%s'.\n%s",
        pattern, db, .message_availableTables(tableNames)
      ))
      
    } else {
      
      tableName <- tableNames[matching[1]]
      
      if (n > 1) {
        
        warning(sprintf(
          "There is more than one table name matching '%s'. I chose '%s'.",
          pattern, tableName
        ))
      }
    }
  }
  
  tableName
}
