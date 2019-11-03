# .grepValue -------------------------------------------------------------------

.grepValue <- function(...) grep(..., value = TRUE)

# .newlineCollapsed ------------------------------------------------------------

.newlineCollapsed <- function(...) kwb.utils::collapsed(..., "\n")

# .commaNewlineCollapsed -------------------------------------------------------

.commaNewlineCollapsed <- function(...) kwb.utils::collapsed(..., ",\n")

# .newlineSeparated ------------------------------------------------------------

.newlineSeparated <- function(...) paste(..., sep = "\n")

# createRDatabaseInterface -----------------------------------------------------

#' Create R-Function Interface to Database
#' 
#' @param db name of ODBC data source or full path to MS Access or Excel file
#' @param dbSchema list structure describing the database schema, as returned by
#'   kwb.odm::dbSchema_ODM()
#' @param tableNamePattern pattern matching the names of tables/views for which
#'   an accessor function is to be generated
#' @param functionPrefix prefix to be given to each generated function. Default:
#'   "db_"
#' @param pattern pattern matching the part of the table name that will be
#'   replaced with \emph{functionPrefix}. Default: "^", i.e. the prefix will be
#'   appended to the start of the table name
#' @param rfile full path to the file in which to store the data access 
#'   functions
#' @param rfile.create full path to the file in which to store the data creation 
#'   functions (only relevant if \emph{crete.create.functions} is TRUE)
#' @param create.create.functions logical. If TRUE (the default if FALSE) not 
#'   only data access but also data creation functions are generated
#' @param dbg if TRUE, progress messages are shown during the creation of the
#'   interface functions
#' @param sql.dbg if TRUE, SQL strings used to access the database are shown
#'   when calling the interface functions
#' @export
#' 
createRDatabaseInterface <- function
(
  db = NULL, dbSchema = NULL, tableNamePattern = "", functionPrefix = "db_",
  pattern = "^", rfile = file.path(tempdir(), "dbInterface.R"),
  rfile.create = file.path(tempdir(), "dbInterfaceCreate.R"),
  create.create.functions = FALSE, dbg = TRUE, sql.dbg = TRUE
)
{
  rcodes <- rcode_DatabaseInterface(
    db = db,
    dbSchema = dbSchema,
    tableNamePattern = tableNamePattern,
    functionPrefix = functionPrefix,
    pattern = pattern,
    dbg = dbg,
    sql.dbg = sql.dbg,
    create.create.functions = create.create.functions
  )

  writeLines(rcodes$select, con = rfile)

  rfiles <- if (create.create.functions) {
    
    writeLines(rcodes$create, con = rfile.create)
    
    c(rfile, rfile.create)
    
  } else {
    
    rfile
  }

  cat(
    sprintf(
      "The generated script(s) has/have been written to:\n  %s\n",
      paste(kwb.utils::windowsPath(rfiles), sep = "\n")
    )
  )

  rfiles
}

# rcode_DatabaseInterface ------------------------------------------------------

#' R-code for Functions to Access Database Tables
#' 
#' @param db database name or file
#' @param dbSchema database schema as returned by
#'   \code{\link{getDatabaseSchema}}
#' @param tableNamePattern pattern matching the names of tables/views for which
#'   an accessor function is to be generated
#' @param functionPrefix prefix to be given to each generated function. Default:
#'   "db_"
#' @param pattern pattern matching the part of the table name that will be
#'   replaced with \emph{functionPrefix}. Default: "^", i.e. the prefix will be
#'   appended to the start of the table name
#' @param dbg if TRUE, progress messages are shown during the creation of the
#'   interface functions
#' @param sql.dbg if TRUE, SQL strings used to access the database are shown
#'   when calling the interface functions
#' @param create.create.functions if TRUE, functions for creating new records in
#'   the database tables are created
#' 
rcode_DatabaseInterface <- function(
  db = NULL, dbSchema = NULL, tableNamePattern = "", functionPrefix = "db_",
  pattern = "^", dbg = TRUE, sql.dbg = TRUE, create.create.functions = FALSE
)
{
  tableSchemata <- .getTableSchemata(dbSchema, db)
  tableNames <- .getTableNamesMatchingPattern(tableSchemata, tableNamePattern)

  rcode.select <- .introComment()
  rcode.create <- .introComment()

  for (tableName in tableNames) {

    funName <- .tableNameToFunctionName(functionPrefix, pattern, tableName)

    rcode.select <- c(
      rcode.select,
      "",
      .callWithMessages(
        funName = funName,
        dbg = sql.dbg,
        FUN = rcode_tableAccess,
        dbDefault = .dbDefault(db),
        tableSchema = tableSchemata[[tableName]],
        tableName = tableName
      )
    )

    if (create.create.functions) {
      rcode.create <- c(
        rcode.create,
        "",
        .callWithMessages(
          funName = paste(funName, "create", sep = "_"),
          dbg = sql.dbg,
          FUN = rcode_createRecord,
          dbDefault = .dbDefault(db),
          tableSchema = tableSchemata[[tableName]],
          tableName = tableName
        )
      )
    }
  }

  result <- list(select = rcode.select)

  if (create.create.functions) {
    result$create <- rcode.create
  }

  result
}

# .callWithMessages ------------------------------------------------------------

.callWithMessages <- function(funName, dbg, FUN, ...)
{
  kwb.utils::catIf(dbg, "*** Writing db access function", funName, "... ")

  result <- FUN(funName = funName, ...)

  kwb.utils::catIf(dbg, "ok.\n")

  result
}

# .getTableSchemata ------------------------------------------------------------

.getTableSchemata <- function(dbSchema, db)
{
  if (is.null(dbSchema)) {
    dbSchema <- getDatabaseSchema(db)
  }

  dbSchema$tables
}

# .getTableNamesMatchingPattern ------------------------------------------------

.getTableNamesMatchingPattern <- function(tableSchemata, tableNamePattern)
{
  tableNames <- names(tableSchemata)

  if (tableNamePattern != "") {
    tableNames <- grep(tableNamePattern, tableNames, value = TRUE)
  }

  tableNames
}

# .introComment ----------------------------------------------------------------

.introComment <- function()
{
  c(
    "#",
    "# This R-script has been generated by createRDatabaseInterface",
    sprintf("# at: %s", date()),
    "#"
  )
}

# .tableNameToFunctionName -----------------------------------------------------

.tableNameToFunctionName <- function(functionPrefix, pattern, tableName)
{
  sub(pattern, functionPrefix, tableName)
}

# rcode_tableAccess ------------------------------------------------------------

rcode_tableAccess <- function(
  dbDefault, tableSchema, tableName, funName, dbg = TRUE
)
{
  fields <- names(tableSchema$fields)

  fieldDescriptions <- sapply(tableSchema$fields, "[[", "Description")

  HEADER <- c(
    "# FUNCTION_NAME HORIZONTAL_LINE",
    "FUNCTION_NAME <- function # select records from TABLE_NAME",
    "### select and filter records from TABLE_NAME"
  )

  WHERE_ARGUMENTS <- .newlineCollapsed(
    sprintf("  where_%s = NA,\n  ### condition for field \\emph{%s}: %s",
            fields, fields, fieldDescriptions)
  )

  SELECT_ARGUMENTS <- .newlineCollapsed(
    sprintf("  select_%s = NA,\n  ### select field \\emph{%s}?",
            fields, fields)
  )

  ORDERBY_ARGUMENTS <- .newlineCollapsed(
    sprintf("  orderBy_%s = NA,\n  ### order by field \\emph{%s}?",
            fields, fields)
  )

  ADDITIONAL_ARGUMENTS <- c(
    "  db = DEFAULT_DB,",
    "  ...",
    "  ### additional arguments passed to \\code{\\link{selectFromTable}}"
  )

  ASSIGNMENTS <- .commaNewlineCollapsed(sprintf(
    "      %s = %s", fields, fields
  ))

  WHERE_ASSIGNMENTS <- .commaNewlineCollapsed(sprintf(
    "    where_%s = where_%s", fields, fields
  ))

  SELECT_ASSIGNMENTS <- .commaNewlineCollapsed(sprintf(
    "    select_%s = select_%s", fields, fields
  ))

  ORDERBY_ASSIGNMENTS <- .commaNewlineCollapsed(sprintf(
    "    orderBy_%s = orderBy_%s", fields, fields
  ))

  BODY_EXPRESSION_1 <- c(
    "  arguments <- list(",
    "    select = select,",
    paste(SELECT_ASSIGNMENTS, ","),
    "    where = where,",
    paste(WHERE_ASSIGNMENTS, ","),
    "    orderBy = orderBy,",
    ORDERBY_ASSIGNMENTS,
    "  )"
  )

  BODY_EXPRESSION_2 <- "  selectFromTable(db, 'TABLE_NAME', arguments, ...)"

  HEADER <- kwb.utils::multiSubstitute(
    HEADER,
    list(FUNCTION_NAME = funName,
         TABLE_NAME = tableName,
         HORIZONTAL_LINE = .horizontalLine(funName))
  )

  ADDITIONAL_ARGUMENTS <- kwb.utils::multiSubstitute(
    ADDITIONAL_ARGUMENTS,
    list(FIELDS = kwb.utils::collapsed(fields, ","),
         DEFAULT_DB = dbDefault)
  )

  BODY_EXPRESSION_2 <- kwb.utils::multiSubstitute(
    BODY_EXPRESSION_2,
    list(TABLE_NAME = tableName)
  )

  .newlineCollapsed(
    c(
      HEADER,
      "(",
      "  where = NULL,",
      "  ### WHERE condition",
      WHERE_ARGUMENTS,
      "  select = NULL,",
      "  ### fields/expressions to select",
      SELECT_ARGUMENTS,
      "  orderBy = NULL,",
      "  ### ORDER BY clause of SQL expression",
      ORDERBY_ARGUMENTS,
      ADDITIONAL_ARGUMENTS,
      ") ",
      "{",
      BODY_EXPRESSION_1,
      "",
      BODY_EXPRESSION_2,
      "}"
    )
  )
}

# .horizontalLine --------------------------------------------------------------

.horizontalLine <- function(functionName)
{
  n.dashes <- 77 - nchar(functionName)
  kwb.utils::collapsed(rep("-", n.dashes), "")
}

# rcode_createRecord -----------------------------------------------------------

rcode_createRecord <- function(
  dbDefault, tableSchema, tableName, funName, dbg = TRUE
)
{
  # do not consider first field if it is the tables ID field
  fieldNames <- names(tableSchema$fields)

  if (grepl("ID$", fieldNames[1], ignore.case = TRUE)) {
    fieldNames <- fieldNames[-1]
  }

  fieldSchemata <- tableSchema$fields[fieldNames]

  fields <- names(fieldSchemata)

  fieldDescriptions <- sapply(fieldSchemata, "[[", "Description")

  grammar <- list(

    function_code = .newlineSeparated(
      "<header>",
      "(",
      "<all_arguments>",
      ")",
      "{",
      "<body_expression>",
      "}"
    ),

    header = .newlineSeparated(
      "# <function_name> <horizontal_line>",
      "<function_name> <- function # insert one record into table '<table_name>'",
      "### insert one record into table '<table_name>'"
    ),

    all_arguments = .newlineSeparated(
      "<arguments>,",
      "<additional_arguments>"
    ),

    additional_arguments = .newlineSeparated(
      "  db = <default_db>,",
      "  run = TRUE,",
      "  ### if \\code{TRUE} the SQL query is performed, otherwise the SQL ",
      "  ### code that would be performed is returned",
      "  ...",
      "  ### additional arguments passed to \\code{\\link{hsSqlQuery}}"
    ),

    body_expression = .newlineSeparated(
      "<body_expression_1>",
      "",
      "<body_expression_2>"
    ),

    body_expression_1 = .newlineSeparated(
      "  keyValues <- list(",
      "<assignments>",
      "  )"
    ),

    body_expression_2 = .newlineSeparated(
      "  sqlExpressions <- keyValuesToSqlAssignment2(keyValues)",
      "",
      "  sql <- sqlForInsert(",
      "    tablename =\"<table_name_lower_case>\",",
      "    fields = sqlExpressions$fieldList,",
      "    sqlSource = paste(\"(\", sqlExpressions$valueList, \")\")",
      "  )",
      "",
      "  if (run) {",
      "    hsSqlQuery(db, sql = sql, ...)",
      "  } else {",
      "    sql",
      "  }"
    )
  )

  kwb.utils::resolve(
    x = "function_code",
    dict = grammar,
    table_name = tableName,
    table_name_lower_case = tolower(tableName),
    function_name = funName,
    horizontal_line = .horizontalLine(funName),
    arguments = kwb.utils::collapsed(sprintf(
      "  %s = NA,\n  ### value for table field \\emph{%s}: %s",
      fields, fields, fieldDescriptions), "\n"),
    assignments = kwb.utils::collapsed(sprintf(
      "      %s = %s", fields, fields), ",\n"),
    default_db = dbDefault
  )
}

# .dbDefault -------------------------------------------------------------------

.dbDefault <- function(db = NULL)
{
  if (is.null(db)) {
    
    "currentDb()"
    
  } else {
    
    kwb.utils::hsQuoteChr(db)
  }
}

# getFilteredRecords -----------------------------------------------------------

#' Get Filtered Records
#' 
#' @param db database name or database file
#' @param tableName name of the table from which to read records
#' @param keyValues list of \code{key = value} pairs with the keys representing
#'   field names of the table \code{tableName}, defining filter criteria
#' @param fields table fields to be selected
#' @param like if \code{TRUE} the SQL \code{LIKE} operator is used instead of
#'   the equal operator \code{=} when matching the values in \code{keyValues} 
#'   with the table fields
#' @param \dots additonal arguments passed to \code{\link{selectFromDb}}
#' 
getFilteredRecords <- function(db, tableName, keyValues, fields, like, ...)
{
  # set SQL dialect according to database type
  setCurrentSqlDialect(ifelse(isMySQL(db), "mysql", "msaccess"))

  where <- keyValuesToSqlFilter(kwb.utils::excludeNULL(keyValues), like = like)

  selectFromDb(tableName, fields, where, db, ...)
}

# selectFromTable --------------------------------------------------------------

#' Select from Table
#' 
#' @param db database name or file
#' @param tableName name of table from which to read data
#' @param arguments list with elements \code{select}, \code{where}, 
#'   \code{orderBy} and further elements starting with  \code{select_}, 
#'   \code{where_} and  \code{orderBy_}, respectively
#' @param run if TRUE (default) the SQL SELECT statement is run otherwise
#'   returned as character string
#' @param \dots further arguments passed to \code{\link{hsSqlQuery}}
#' @export
#' 
selectFromTable <- function(db, tableName, arguments, run = TRUE, ...)
{
  argumentNames <- names(arguments)

  sqlParts <- argumentsToSqlParts(
    select = arguments$select,
    selects = arguments[.grepValue("^select_", argumentNames)],
    where = arguments$where,
    wheres = arguments[.grepValue("^where_", argumentNames)],
    orderBy = arguments$orderBy,
    orderBys = arguments[.grepValue("^orderBy_", argumentNames)]
  )

  sql <- sqlForSelect(
    tablename = .toExistingTableName(tableName, db),
    fields = sqlParts$select,
    whereClause = sqlParts$where,
    orderBy = sqlParts$orderBy,
    sqlDialect = ifelse(isMySQL(db), "mysql", "msaccess")
  )

  if (run) {
    
    hsSqlQuery(db, sql, ...)
    
  } else {
    
    sql
  }
}
