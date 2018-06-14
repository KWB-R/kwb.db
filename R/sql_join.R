# sqlLeftJoin ------------------------------------------------------------------

#' Generate SQL for LEFT JOIN
#' 
#' @param sqlSource SQL of subquery that provides the "base" table on the left
#' @param tablesAndIDs named character vector with the names representing the
#'   names of the tables to be joined and the values representing the ID fields
#'   of these tables
#' @param fields fields to be selected
#' 
sqlLeftJoin <- function(sqlSource, tablesAndIDs, fields = "*") 
{
  tableNames <- names(tablesAndIDs)
  
  for (tableName in tableNames) {
    
    sqlSource <- sqlLeftJoinBody(
      leftSql = sqlSource, 
      rightTable = tableName, 
      id = tablesAndIDs[tableName], 
      useAlias = (tableName == tableNames[1])
    )
  }
  
  sprintf("SELECT %s FROM %s", fields, sqlSource)
}

# sqlLeftJoinBody --------------------------------------------------------------

#' Generate (Base Part of) SQL for LEFT JOIN
#' 
#' @param leftSql SQL of subquery that provides the "base" table on the left
#' @param rightTable name of "right" table
#' @param id name of ID field of "right" table (must correspond to a field
#'   returned by \emph{leftSql})
#' @param idLeft name of ID field of "left" table
#' @param useAlias if TRUE, the alias given in \emph{aliasName} is given to the
#'   subquery \emph{leftSql}. Default: FALSE
#' @param aliasName alias name to be used if \emph{useAlias} is TRUE. Default:
#'   "tbase"
#' 
sqlLeftJoinBody <- function(
  leftSql, rightTable, id, idLeft = id, useAlias = (id != idLeft), 
  aliasName = "tbase"
) 
{
  sprintf(
    "(%s) %s LEFT JOIN %s ON %s = %s", 
    leftSql, 
    ifelse(useAlias, aliasName, ""), 
    rightTable, 
    .sqlFieldIdentifier(fieldName = idLeft, tableName = aliasName),
    .sqlFieldIdentifier(fieldName = id, tableName = rightTable)
  )
}

# sqlLeftJoinExpression --------------------------------------------------------

#' Create an SQL LEFT JOIN Expression
#' 
#' @param left left part of JOIN (e.g. table name)
#' @param right right part of JOIN (e.g. table name)
#' @param condition condition
#' 
sqlLeftJoinExpression <- function(left, right, condition)
{
  sqlJoinExpression(left, right, condition, type = "left")
}

# sqlJoinExpression ------------------------------------------------------------

#' Create an SQL JOIN Expression
#' 
#' @param left left part of JOIN (e.g. table name)
#' @param right right part of JOIN (e.g. table name)
#' @param condition condition
#' @param type one of c("LEFT", "RIGHT", "INNER")
#' 
sqlJoinExpression <- function(left, right, condition, type = "INNER")
{
  sql <- paste(
    .getTableAndAlias(left), "\n", toupper(type), "JOIN", 
    .getTableAndAlias(right), "\n", "ON", "(", condition, ")"
  )
  
  .addSqlType(sql, "joined table")
}

# .getTableAndAlias ------------------------------------------------------------

.getTableAndAlias <- function(x) 
{
  paste(.getTable(x), .getAlias(x))
}

# .getTable --------------------------------------------------------------------

.getTable <- function(x) 
{
  if (any(c("query specification", "joined table") %in% .getSqlType(x))) {
    
    paste("(", x, ")")
    
  } else {
    x
  }
}

# .getAlias --------------------------------------------------------------------

.getAlias <- function(x) 
{
  alias <- attr(x, which = "alias")

  if (is.null(alias)) {
    
    ""
    
  } else {
    
    paste("AS", alias)
  }
}

# .sqlFieldIdentifier ----------------------------------------------------------

.sqlFieldIdentifier <- function(fieldName, tableName)
{
  paste(tableName, fieldName, sep = ".")
}
