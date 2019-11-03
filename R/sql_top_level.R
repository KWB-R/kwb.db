# sqlForSelect -----------------------------------------------------------------

#' Generate SQL SELECT statement#' 
#' 
#' @param tablename table name
#' @param fields expression to select fields; field names are separated by comma
#'   and alias names may be used, just as SQL accepts, e.g.: "tstamp as
#'   myDateTime, parVal as myValue"; Default: "*"
#' @param whereClause where condition; Default: "TRUE"
#' @param groupBy GROUP BY-clause, Default: "" (no grouping)
#' @param orderBy ORDER BY-clause, Default: "" (no sorting of results)
#' @param sqlDialect one of \code{c("mysql", "msaccess")}
#' 
sqlForSelect <- function(
  tablename, fields = "*", whereClause = "TRUE", groupBy = "", orderBy = "",
  sqlDialect = getCurrentSqlDialect()
)
{
  sql <- paste(
    "SELECT", fields,
    "FROM", safeTableName(tablename, sqlDialect), .getAlias(tablename),
    "WHERE", whereClause
  )

  sql <- .addClauseIfGiven(sql, "GROUP BY", groupBy)
  sql <- .addClauseIfGiven(sql, "ORDER BY", orderBy)

  .addSqlType(sql, "query specification")
}

# .addClauseIfGiven ------------------------------------------------------------

.addClauseIfGiven <- function(sql, keyword, clause)
{
  if (clause != "") {
    
    paste(sql, keyword, clause)
    
  } else {
    
    sql
  }
}

# sqlForSelectByKey ------------------------------------------------------------

#' Generate SQL SELECT Statement
#' 
#' Generate SQL SELECT statement (key field values instead of where clause)
#' 
#' @param tablename table name
#' @param fields expression to select fields; field names are separated by comma
#'   and alias names may be used, just as SQL accepts, e.g.: "tstamp as
#'   myDateTime, parVal as myValue"; Default: "*"
#' @param keyValues list of "key=value" pairs with the keys being valid field
#'   names of \emph{table}
#' 
sqlForSelectByKey <- function(
  tablename, fields = "*", keyValues = NULL
)
{
  sqlForSelect(tablename, fields, keyValuesToSqlFilter(keyValues))
}

# sqlForInsertIgnoreInMsAccess -------------------------------------------------

#' SQL for "INSERT IGNORE" in MS Access
#' 
#' Returns SQL for inserting all records from table.source that are not yet
#'   contained in table.target into table.target
#' 
#' @param db database name or file
#' @param table.source name of source table
#' @param table.target name of target table
#' @param uniqueFields names of unique fields
#' 
sqlForInsertIgnoreInMsAccess <- function(
  db, table.source, table.target, uniqueFields = NA
)
{
  tableFields <- hsFields(db, table.target)

  keyField <- tableFields[1]
  fieldsWithoutID <- tableFields[-1]

  if (all(is.na(uniqueFields))) {
    uniqueFields <- fieldsWithoutID
  }

  condition <- paste(
    sprintf("s.%s = t.%s", uniqueFields, uniqueFields),
    collapse = "\n    AND "
  )

  joinExpression <- sqlLeftJoinExpression(
    left = structure(table.source, alias = "s"),
    right = structure(table.target, alias = "t"),
    condition = condition
  )

  sqlSource <- sprintf(
    "SELECT\n  %s\nFROM\n  %s\nWHERE\n  IsNull(t.%s)",
    paste("s", fieldsWithoutID, sep = ".", collapse = ",\n  "),
    joinExpression,
    keyField
  )

  sqlInsert <- sprintf(
    "INSERT INTO %s (\n  %s)\n%s",
    table.target,
    paste(fieldsWithoutID, collapse = ",\n  "),
    sqlSource
  )

  structure(sqlInsert, source.SQL = sqlSource)
}

# sqlForInsert -----------------------------------------------------------------

#' Generate SQL INSERT statement
#' 
#' @param tablename table name
#' @param fields field names, separated by comma
#' @param sqlSource value tupels of form (value1.1, value1.2, value1.3, ...)
#'   (value2.1, value2.2, value2.3, ...) ... or SQL SELECT statement providing
#'   these tupels
#' @param sourceAreValues if TRUE, \emph{sqlSource} is expected to be an SQL
#'   query providing data to be inserted -> no keyword VALUES in generated SQL
#'   code
#' @param ignore if TRUE the keyword IGNORE is inserted between INSERT and INTO
#'   in the SQL statement -> no error will be given if data to insert already
#'   exists
#' 
sqlForInsert <- function(
  tablename, fields, sqlSource, sourceAreValues = ! grepl(sqlSource, "^SELECT"),
  ignore = FALSE
)
{
  sqlIgnore <- ifelse(ignore, "IGNORE", "")
  sqlValues <- ifelse(sourceAreValues, "VALUES", "")

  sprintf(
    "INSERT %s INTO %s(%s) %s %s", 
    sqlIgnore, tablename, fields, sqlValues, sqlSource
  )
}

# sqlForInsertFromSelect -------------------------------------------------------

#' SQL for INSERT FROM SELECT
#' 
#' Generate SQL INSERT statement of the form INSERT INTO target.table (fields)
#'   SELECT fields FROM source.table
#' 
#' @param target.table name of target table
#' @param source.table name of source table or SQL providing source data
#' @param fields vector of character with field names
#' 
sqlForInsertFromSelect <- function(target.table, source.table, fields)
{
  fieldList <- paste(fields, collapse = ",\n  ")

  sprintf(
    "INSERT INTO %s (\n  %s\n)\nSELECT\n  %s\nFROM\n  %s",
    target.table, fieldList, fieldList, source.table
  )
}

# sqlForInsertDataFrame --------------------------------------------------------

#' Generate SQL INSERT Statement
#' 
#' Generate SQL INSERT statement to insert values in a data frame
#' 
#' @param tablename table name
#' @param dataFrame data frame with column names representing table field names
#' @param ignore if TRUE the keyword IGNORE is inserted between INSERT and INTO
#'   in the SQL statement -> no error will be given if data to insert already
#'   exists
#' @param \dots further arguments passed to \code{\link{sqlForInsert}}
#' 
sqlForInsertDataFrame <- function(tablename, dataFrame, ignore = FALSE, ...)
{
  sqlForInsert(
    tablename = tablename,
    fields = kwb.utils::commaCollapsed(names(dataFrame)),
    sqlSource = kwb.utils::commaCollapsed(dataFrameToSqlTuples(dataFrame)),
    sourceAreValues = TRUE,
    ignore = ignore,
    ...
  )
}

# sqlForUpdate -----------------------------------------------------------------

#' Generate SQL UPDATE Statement
#' 
#' @param tablename table name
#' @param keyValues assignments as list of \emph{key=value} pairs with the keys
#'   representing valid fields of table \emph{tablename}
#' @param whereClause where condition
#' @param ignore if TRUE the keyword IGNORE is inserted between UPDATE and INTO
#'   in the SQL statement -> no error will be given if updating fails, e.g.
#'   because of key constraints
#' 
sqlForUpdate <- function(tablename, keyValues, whereClause, ignore = FALSE)
{
  sqlIgnore <- ifelse(ignore, "IGNORE", "")
  
  sprintf(
    "UPDATE %s %s SET %s WHERE %s",
    sqlIgnore, tablename, keyValuesToSqlAssignment(keyValues), whereClause
  )
}
