# printDatabaseSchema ----------------------------------------------------------

#' Print Database Schema
#' 
#' @param dbSchema database schema as returned by
#'   \code{\link{getDatabaseSchema}}
#' 
printDatabaseSchema <- function(dbSchema)
{
  for (tableName in names(dbSchema$tables)) {

    cat(sprintf("Table '%s':\n", tableName))

    tableSchema <- dbSchema$tables[[tableName]]

    for (fieldName in names(tableSchema$fields)) {

      fieldType <- tableSchema$fields[[fieldName]]$DataType
      examples <- tableSchema$fields[[fieldName]]$Examples

      cat(sprintf(
        "  Field '%s': %s (e.g. %s)\n", fieldName, fieldType, examples
      ))
    }
  }
}

# getDatabaseSchema ------------------------------------------------------------

#' Get Database Schema
#' 
#' @param db full path to database (*.mdb, *.xls) or name of ODBC database
#' 
#' @return list with elements \emph{tables} and \emph{relationships}. Element
#'   \emph{tables} is a list o named elements with the name representing the
#'   table names and the elements being lists describing the table...
#'
getDatabaseSchema <- function(db)
{
  sqlDialect <- ifelse(isMySQL(db), "mysql", "msaccess")

  tableInfo <- hsTables(db, namesOnly = FALSE)

  roworder <- order(
    tableInfo$TABLE_TYPE, tableInfo$TABLE_NAME
  )

  tableInfo <- tableInfo[roworder, ]

  selected <- tableInfo$TABLE_TYPE %in% c("TABLE", "VIEW")

  tableNames <- sort(tableInfo$TABLE_NAME[selected])

  databaseSchema <- list(tables = list())

  for (tableName in tableNames) {

    cat("Analysing schema of table", tableName, "...\n")

    tableSchema <- .getTableSchema(
      db = db,
      tableName = tableName,
      sqlDialect = sqlDialect
    )

    databaseSchema$tables[[tableName]] <- tableSchema
  }

  databaseSchema
}

# .getTableSchema --------------------------------------------------------------

.getTableSchema <- function(db, tableName, sqlDialect)
{
  fieldNames <- hsFields(db, tableName)

  tableFields <- list()

  for (fieldName in fieldNames) {

    cat("  Analysing field", fieldName, "...\n")

    fieldSchema <- .getFieldSchema(
      db = db,
      tableName = tableName,
      fieldName = fieldName,
      sqlDialect = sqlDialect
    )

    tableFields[[fieldName]] <- fieldSchema
  }

  list(
    description = paste("Enter a description of table", tableName, "here"),
    fields = tableFields
  )
}

# .getFieldSchema --------------------------------------------------------------

.getFieldSchema <- function(db, tableName, fieldName, sqlDialect)
{
  sql <- sprintf(
    "SELECT DISTINCT %s %s FROM %s WHERE NOT IsNull(%s) %s",
    ifelse(sqlDialect == "msaccess", "TOP 10", ""),
    fieldName,
    safeTableName(tableName, sqlDialect),
    fieldName,
    ifelse(sqlDialect == "mysql", "LIMIT 10", "")
  )

  exampleValues <- hsSqlQuery(
    db,
    sql,
    stringsAsFactors = FALSE,
    dbg = FALSE,
    stopOnError = FALSE
  )

  examples <- if (! kwb.utils::isNullOrEmpty(exampleValues)) {
    
    numberOfRows <- min(c(nrow(exampleValues), 3))
    
    paste(
      kwb.utils::hsQuoteChr(exampleValues[seq_len(numberOfRows), 1]),
      collapse = "; "
    )
    
  } else {
    
    ""
  }

  list(
    DataType = getTypeIdentifier(exampleValues[, 1]),
    Description = "Enter a field description here",
    Examples = examples
  )
}

# safeTableName ----------------------------------------------------------------

#' Safe Table Name
#' 
#' MS Access: table name enclosed in brackets "[" and "]", else: table name
#' enclosed in backquotes "`"
#' 
#' @param tableName name of table to be quoted
#' @param sqlDialect one of \code{c("mysql", "msaccess")}
#' 
safeTableName <- function(
  tableName, sqlDialect = getCurrentSqlDialect(warn = FALSE)
)
{
  sqlType <- .getSqlType(tableName)

  if ("joined table" %in% sqlType) {
    
    tableName
    
  } else if ("query specification" %in% sqlType) {
    
    paste("(", tableName, ")")
    
  } else {
    
    containsSpaceOrSpecialCharacter <- grepl("\\s+|\\.|\\$", tableName)

    if (is.null(sqlDialect) || !containsSpaceOrSpecialCharacter) {
      
      tableName
      
    } else if (sqlDialect == "msaccess") {
      
      paste0("[", tableName, "]")
      
    } else {
      
      paste0("`", tableName, "`")
    }
  }
}

# getTypeIdentifier ------------------------------------------------------------

#' Get Type Identifier
#' 
#' @param x R object for which to find an adequate database object type
#' 
#' @return Returns "int", "double", "text", "date_time" or "boolean" depending
#'   on the data type of \emph{x}
#' 
getTypeIdentifier <- function(x)
{
  if (is.integer(x)) {
    
    "int"
    
  } else if (is.numeric(x)) {
    
    "double"
    
  } else if (is.character(x)) {
    
    "text"
    
  } else if ((class(x) == "Date") || ("POSIXct" %in% class(x))) {
    
    "date_time"
    
  } else if (is.logical(x)) {
    
    "boolean"
    
  } else if ("ODBC_binary" %in% class(x)) {
    
    "object"
    
  } else {
    
    utils::str(x)
    
    stop("Unknown type (see output of str() above")
  }
}
