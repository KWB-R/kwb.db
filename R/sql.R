# .getSqlType ------------------------------------------------------------------

.getSqlType <- function(x)
{
  # sql.type according to http://www.savage.net.au/SQL/sql-92.bnf.html
  attr(x, which = "sql.type")
}

# .addSqlType ------------------------------------------------------------------

.addSqlType <- function(x, sql.type)
{
  # sql.type according to http://www.savage.net.au/SQL/sql-92.bnf.html
  sql.types <- unique(c(.getSqlType(x), sql.type))

  attr(x, "sql.type") <- sql.types
  x
}

# .sqlEquals -------------------------------------------------------------------

.sqlEquals <- function(leftSide, rightSide)
{
  paste(leftSide, "=", rightSide)
}

# keyValuesToSqlAssignment -----------------------------------------------------

#' List of Key = Value Pairs to SQL Assignment
#' 
#' @param keyValues list of \code{key = value} pairs
#' 
keyValuesToSqlAssignment <- function(keyValues)
{
  keyValuesToSql(keyValues, filter = FALSE)
}

# keyValuesToSqlAssignment2 ----------------------------------------------------

#' Key Values to SQL Assignment (2)
#' 
#' @param keyvalues list of \code{key = value pairs}
#' 
#' @return list with elements \emph{fieldList} and \emph{valueList}
#' @importFrom kwb.utils commaCollapsed
#' 
keyValuesToSqlAssignment2 <- function(keyvalues)
{
  (quotedValues <- as.character(sapply(as.list(keyvalues), singleValueToSql)))

  list(
    fieldList = kwb.utils::commaCollapsed(names(keyvalues)),
    valueList = kwb.utils::commaCollapsed(quotedValues)
  )
}

# keyValuesToSqlFilter ---------------------------------------------------------

#' List of Key = Value Pairs to SQL Filter Expression
#' 
#' @param keyValues list of \code{key = value} pairs
#' @param like if \code{TRUE}, the SQL comparison operator will be \code{LIKE} 
#'   instead of \code{=}.
#' 
keyValuesToSqlFilter <- function(keyValues, like = FALSE)
{
  keyValuesToSql(keyValues, filter = TRUE, like = like)
}

# keyValuesToSql ---------------------------------------------------------------

#' List of Key = Value Pairs to SQL Expressions
#' 
#' @param keyValues list of \code{key = value} pairs
#' @param filter logical. If \code{TRUE} the target is an SQL filter expression,
#'   otherwise an SQL SET expression.
#' @param like passed to \code{\link{keyValueToSql}}
#' @export
#' @examples
#' keyValues <- list(name = "Peter", birth = as.POSIXct("1999-09-09"))
#' 
#' setCurrentSqlDialect("msaccess")
#' cat(keyValuesToSql(keyValues, filter = TRUE))
#' cat(keyValuesToSql(keyValues, filter = TRUE, like = FALSE))
#' cat(keyValuesToSql(keyValues, filter = FALSE))
#' 
#' setCurrentSqlDialect("mysql")
#' cat(keyValuesToSql(keyValues, filter = TRUE))
#' cat(keyValuesToSql(keyValues, filter = TRUE, like = FALSE))
#' cat(keyValuesToSql(keyValues, filter = FALSE))
#' 
keyValuesToSql <- function(keyValues, filter, like = filter)
{
  FUN <- function(x, filter) {
    
    keyValueToSql(x, keyValues[[x]], like = like, filter = filter)
  }

  sqlParts <- sapply(names(keyValues), FUN=FUN, filter)

  if (length(sqlParts) > 0) {
    
    paste(sqlParts, collapse = ifelse(filter, " AND ", ","))
    
  } else {
    
    ifelse(filter, "TRUE", "")
  }
}

# keyValueToSql ----------------------------------------------------------------

#' Generate SQL Filter or Assignment Expression
#' 
#' @param cname field name
#' @param cvalue field value
#' @param like if \code{TRUE}, the SQL comparison operator will be \code{LIKE} 
#'   instead of \code{=}.
#' @param filter if \code{TRUE} an SQL filter expression is returned, otherwise 
#'   an SQL assignment expression
#'   
#' @return (vector of) character representing an SQL expression
#' @importFrom kwb.utils commaCollapsed
#' @export
#' @examples
#' cat(kwb.db:::keyValueToSql("age", 1))
#' cat(kwb.db:::keyValueToSql("name", "peter"))
#' cat(kwb.db:::keyValueToSql("name", "peter", like = FALSE))
#' 
keyValueToSql <- function(cname, cvalue, like = TRUE, filter = TRUE)
{
  if (filter && length(cvalue) > 1) {
    
    operator <- "IN"
    sqlValue <- sprintf("(%s)", kwb.utils::commaCollapsed(
      sapply(cvalue, FUN = singleValueToSql)
    ))
    
  } else {
    
    operator <- valueToSqlOperator(cvalue, like, filter)
    sqlValue <- singleValueToSql(cvalue)
  }

  paste(cname, operator, sqlValue)
}

# valueToSqlOperator -----------------------------------------------------------

valueToSqlOperator <- function(x, like, filter)
{
  if (is.null(x)) {
    
    ""
    
  } else if (is.na(x)) {
    
    ifelse(filter, " IS ", " = ")
    
  } else if (like && !is.numeric(x)) {
    
    " LIKE "
    
  } else {
    
    "="
  }
}

# singleValueToSql -------------------------------------------------------------

#' @importFrom kwb.utils hsQuoteChr
#' 
singleValueToSql <- function(x)
{
  stopifnot(length(x) < 2)

  mysql <- getCurrentSqlDialect() == "mysql"

  if (is.null(x)) {
    
    ""
    
  } else if (is.na(x)) {
    
    "NULL"
    
  } else if (is.numeric(x)) {
    
    as.character(x)
    
  } else if ("POSIXt" %in% class(x)) {
    
    if (! mysql) {
      
      paste("#", x, "#", sep="")
      
    } else {
      kwb.utils::hsQuoteChr(as.character(x))
    }
    
  } else {
    
    kwb.utils::hsQuoteChr(x, qchar = ifelse(mysql, "\"", "'"))
  }
}

# dataFrameToSqlTuples ---------------------------------------------------------

#' Create SQL Tuples from Data Frame
#' 
#' @param newData a data frame
#' 
#' @return vector of character strings each of which represents one row in
#'   \code{newData}
#' @importFrom kwb.utils commaCollapsed
#' @export
#' @examples
#' x <- data.frame(
#'   name = c("Peter", "Paul"), 
#'   birthday = as.POSIXct(c("1981-12-13", "2003-01-16"))
#' )
#' 
#' setCurrentSqlDialect("msaccess")
#' dataFrameToSqlTuples(x)
#' 
#' # Note that the representation of a date and time is different in MySQL
#' setCurrentSqlDialect("mysql")
#' dataFrameToSqlTuples(x)
#' 
dataFrameToSqlTuples <- function(newData)
{
  colnames <- names(newData)

  for (colname in colnames) {
    
    sqlCol <- sapply(newData[[colname]], singleValueToSql)
    
    sqlCols <- if (colname == colnames[1]) {
      
      data.frame(sqlCol)
      
    } else {
      
      cbind(sqlCols, sqlCol)
    }
  }

  sprintf("(%s)", apply(sqlCols, 1, kwb.utils::commaCollapsed))
}

# listValuesToSql --------------------------------------------------------------

#' Create SQL Tuples from a List
#' 
#' @param x a list defining \code{key = value} pairs
#' 
#' @return vector of character strings each of which represents one assignment
#'   in \code{x}
#' @export
#' @examples
#' x <- list(name = "Peter", birthday = as.POSIXct("1981-12-13"))
#' 
#' setCurrentSqlDialect("msaccess")
#' cat(listValuesToSql(x))
#' 
#' # Note that the representation of a date and time is different in MySQL
#' setCurrentSqlDialect("mysql")
#' cat(listValuesToSql(x))
#' 
listValuesToSql <- function(x)
{
  paste(sapply(x, singleValueToSql), collapse = ",")
}
