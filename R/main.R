# lookupRecord -----------------------------------------------------------------

#' Lookup Record
#' 
#' Looks up a record in a database table and returns the ID of the record or
#'   NULL if the record does not exist.
#' 
#' @param db full path to MS Access/Excel database or name of ODBC data source
#' @param tableName name of table in which record is to be looked up
#' @param keyAssignments key-value-assignments used to identify the record to be looked up.
#'   The assignments are defined in forms of a list, e.g. list(key1 = "value1",
#'   key2 = "value2").
#' @param idField name of ID field, default: name of first table field
#' @param dbg if TRUE, debug messages are shown
#' @param use2007Driver passed to \code{\link{isMySQL}}
#' @export
#' 
lookupRecord <- function(
  db, tableName, keyAssignments, idField = hsFields(db, tableName)[1], 
  dbg = FALSE, use2007Driver = NULL
)
{
  whereClause <- keyValuesToSql(keyAssignments, filter = TRUE, like = FALSE)

  is.MySql <- isMySQL(db, use2007Driver = use2007Driver)

  sql <- sqlForSelect(
    tablename = tableName,
    fields = idField,
    whereClause = whereClause,
    sqlDialect = ifelse(is.MySql, "mysql", "msaccess")
  )

  id <- hsSqlQuery(db, sql, stringsAsFactors = FALSE, dbg = dbg)

  numberOfRecords <- nrow(id)

  if (numberOfRecords > 1) {
    
    clean_stop(sprintf(
      "More than one record in %s with %s", tableName, whereClause
    ))
  }

  if (numberOfRecords == 1) {
    
    id[[idField]]
    
  } # else NULL
}

# hsLookupOrAddRecord ----------------------------------------------------------

#' Lookup or Add Record
#' 
#' Looks up a record in a database table and returns the ID of the record. If
#' the record is not found it is inserted to the table and the ID of the new
#' record is returned.
#' 
#' @param mdb full path to MS Access database
#' @param tbl name of table in which record is to be looked up
#' @param keyAssigns key-value-assignements used to identify the record to be
#'   looked up. The assignments are defined in forms of a list, e.g. list(key1 =
#'   "value1", key2 = "value2").
#' @param fieldAssigns further field-value-assignements used when a new record
#'   needs to be inserted. The assignments are defined in forms of a list, e.g.
#'   list(field1 = "value1", field2 = "value2").
#' @param idField name of ID field, default: name of first table field
#' @param dbg if TRUE, debug messages are shown
#' @importFrom kwb.utils hsQuoteChr
#' @export
#' 
hsLookupOrAddRecord <- function (
  mdb, tbl, keyAssigns, fieldAssigns = NULL, idField = hsFields(mdb, tbl)[1],
  dbg = FALSE
)
{
  id <- lookupRecord(
    db = mdb,
    tableName = tbl,
    keyAssignments = keyAssigns,
    idField = idField,
    dbg = dbg)

  if (is.null(id)) {

    ## insert new record
    fnames <- paste(c(names(keyAssigns), names(fieldAssigns)), collapse = ", ")
    fvals  <- paste(c(kwb.utils::hsQuoteChr(keyAssigns),
                      kwb.utils::hsQuoteChr(fieldAssigns)),
                    collapse = ", ")

    hsSqlQuery(
      mdb,
      sprintf("INSERT INTO %s(%s) VALUES(%s)", tbl, fnames, fvals),
      stringsAsFactors = FALSE,
      dbg = dbg
    )

    ## lookup record again
    id <- hsLookupOrAddRecord(
      mdb = mdb,
      tbl = tbl,
      keyAssigns = keyAssigns,
      fieldAssigns = fieldAssigns,
      idField = idField,
      dbg = dbg
    )
  }

  id
}

# hsTsField --------------------------------------------------------------------

#' Available timestamp-field(s) in database table
#' 
#' @param src source file (MS Access or Excel)
#' @param tbl table name
#' @param namesOnly if TRUE, only the name(s) of the timestamp field(s) is (are)
#'   returned, otherwise additional information.
#' @param all if TRUE, all timestamp fields are considiered, otherwise only the
#'   first timestamp field.
#' 
hsTsField <- function(src, tbl, namesOnly = TRUE, all = FALSE)
{
  allFields <- hsFields(src, tbl, namesOnly = FALSE)
  
  tsFields <- allFields[allFields$TYPE_NAME == "DATETIME", ]

  ## Return if table does not contain any timestamp field
  if (nrow(tsFields) < 1) {
    return()
  }

  if (namesOnly) {
    
    if (all) {
      
      tsFields$COLUMN_NAME
      
    } else {
      
      tsFields$COLUMN_NAME[1]
    }
    
  } else {
    
    if (all) {
      
      tsFields
      
    } else {
      
      tsFields[1, ]
    }
  }
}

# hsSetPrimaryKey --------------------------------------------------------------

#' Set Primary Key of Database Table
#' 
#' Sets fields with names given in vector \emph{keyFields} as key fields of 
#' table \emph{tbl} in MS ACCESS database \emph{mdb}.
#' 
#' @param mdb Full path to MS Access database file (*.mdb).
#' @param tbl Name of table in which key fields are to be defined.
#' @param keyFields (Vector of) key field name(s)
#' @param dbg if TRUE, debug messages are shown
#' @export
#' 
hsSetPrimaryKey <- function(mdb, tbl, keyFields, dbg = FALSE)
{
  sql <- sprintf(
    "ALTER TABLE %s ADD CONSTRAINT pk PRIMARY KEY(%s)",
    tbl, paste(keyFields, collapse = ", ")
  )
  
  hsSqlQuery(mdb, sql, dbg = dbg)
}

# hsGetTimeSeries --------------------------------------------------------------

#' Get Time Series With Timestamp Info
#' 
#' Reads time-series data from an MS Access database table and returns a data
#' frame containing the data. In the data frame the timestamp column contains
#' the timestamps as they are converted to by R from (text versions of) the
#' original timestamps read from MS ACCESS. As this conversion may fail (e.g.
#' the time information gets lost when transferring timestamps from large data
#' sets between R and MS Access) this function may return different pieces of
#' information on the timestamp in forms of additional columns, preceding the
#' timestamp column, in the result data frame. Per default, eleven additional
#' columns are returned: 1. <ts>_txt (timestamp as text), 2. <ts>_Date (date
#' only), 3. <ts>_dSince18991230 (number of days since 1899-12-30), 4.
#' <ts>_secInDay (number of seconds within the day), 5. <ts>_minInDay (number of
#' minutes within the day), 6. <ts>_year (year), 7. <ts>_month (number of
#' month), 8. <ts>_day (number of day within the month), 9. <ts>_h (hours within
#' day), 10. <ts>_min (minutes within hour), 11. <ts>_s (seconds within minute) 
#' where in each case <ts> is the name of the timestamp field.
#' 
#' This function is called internally by the higher-level function 
#' \code{\link{hsMdbTimeSeries}} that reconstructs the correct timestamps from
#' the different pieces of timestamp information and provides them in forms of
#' POSIXct objects in UTC timezone.
#' 
#' Use \code{\link{hsMdbTimeSeries}} instead if you do not want to care about
#' any timestamp conversion problems!
#' 
#' @param mdb Full path to MS Access database file (*.mdb)
#' @param tbl Name of table containing the time-series data.
#' @param tsField Name of table field containing the timestamps.
#' @param fields Vector containing names of value fields to be selected from the
#'   table. This vector may or may not contain the name of the timetamp field.
#' @param minDate Minimum date (and time) of time interval to be selected in
#'   ISO-Syntax: yyyy-mm-dd [HH:MM:SS], where the part in brackets in optional.
#' @param maxDate Day following the maximum date of the time interval to be
#'   selected, in ISO-Syntax: yyyy-mm-dd [HH:MM:SS], where the part in brackets
#'   in optional.
#' @param xTsFields Extra timestamp fields to be selected. Vector containing
#'   numbers between 1 and 11, where each number represents a type of date/time
#'   information as described for function \code{\link{hsSqlExTsFields}}.
#' @param inclLast If TRUE, \emph{maxDate} will be included in result data set,
#'   else excluded.
#' @param sqlFilter additional SQL filter criterion
#' @param dbg if TRUE, debug messages are shown
#'   
#' @return data frame containing the requested data (timestamp and value
#'   columns) and additional columns preceding the timestamp column containing
#'   different pieces of information on the timestamp.
#'   
#' @seealso \code{\link{hsMdbTimeSeries}, \link{hsGetTable},
#'   \link{hsSqlExTsFields}}
#' @importFrom kwb.utils catIf
#' @importFrom kwb.datetime hsToPosix
#' 
#' @export
#' @examples
#' \dontrun{
#' ## Get flow time series of 24 of July 2011 from tbl_Hyd in example database
#' ## Additionally to the timestamp that is created by R, return the date only
#' ## (timestamp info id = 2) and the number of minutes within the day
#' ## (timestamp info id = 5).
#'   
#' setCurrentSqlDialect("msaccess")
#'  
#' if (.Platform$OS.type == "windows") {
#'   ts <- hsGetTimeSeries(
#'     mdb = xmdb(),
#'     tbl = "tbl_Hyd",
#'     tsField = "Zeitst",
#'     fields = c("Q", "v"),
#'     minDate = "2011-08-24",
#'     maxDate = "2011-08-25",
#'     xTsFields = c(2, 5),
#'     dbg = TRUE
#'   )
#'    
#'   ## Show the last records of the returned dataset.
#'   tail(ts)
#' }
#'    
#' ## Output:
#' #      Zeitst_Date Zeitst_minInDay              Zeitst     Q     v
#' # 1435  2011-08-24            1435 2011-08-24 23:55:00 0.638 0.281
#' # 1436  2011-08-24            1436 2011-08-24 23:56:00 0.601 0.265
#' # 1437  2011-08-24            1437 2011-08-24 23:57:00 0.564 0.249
#' # 1438  2011-08-24            1438 2011-08-24 23:58:00 0.536 0.237
#' # 1439  2011-08-24            1439 2011-08-24 23:59:00 0.504 0.223
#' # 1440  2011-08-25               0 2011-08-25 00:00:00 0.483 0.214
#' }
#'   
hsGetTimeSeries <- function(
  mdb, tbl, tsField = hsTsField(mdb, tbl), fields = "*", minDate = NULL,
  maxDate = NULL, xTsFields = c(1:11), inclLast = TRUE, sqlFilter = "TRUE",
  dbg = FALSE
)
{
  # The timestamp field must be in the vector of fields to be selected:
  if (fields != "*" && ! (tsField %in% fields)) {
    
    fields <- c(tsField, fields)
  }
  
  fieldList <- paste(fields, collapse = ", ")
  
  kwb.utils::catIf(dbg, sprintf("fieldList: %s\n", fieldList))
  
  # If there are extra timestamp fields to be selected add these to the field
  # list
  if (length(xTsFields) > 0) {
    
    fieldList <- paste(
      hsSqlExTsFields(tsField = tsField, extraTsFields = xTsFields),
      fieldList,
      sep = ", "
    )
  }
  
  kwb.utils::catIf(dbg, sprintf(
    "fieldList with extra timestamp fields: %s\n", fieldList
  ))
  
  # Generate SQL string selecting for requested fields of records within
  # the time interval between minDate and maxDate ordered by timestamp
  sql <- sprintf(
    "SELECT %s FROM [%s] WHERE %s AND %s ORDER BY %s",
    fieldList,
    tbl,
    sqlFilter,
    hsSqlExTimeCond(
      tsField = tsField,
      dateFirst = minDate,
      dateLast = maxDate,
      inclLast = inclLast,
      dbg = dbg
    ), # time interval filter
    tsField
  ) # ORDER BY timstamp field)
  
  # Run the query
  hsSqlQuery(mdb, sql, dbg = dbg)
}

# hsMdbTimeSeries --------------------------------------------------------------

#' Get Mdb time series in UTC
#' 
#' Reads time-series data from an MS Access database table and returns a data
#' frame containing the data. The name of the timestamp field must be given in
#' \emph{tsField} and the names of the value fields to be selected from the
#' table must be given in vector \emph{fields}. Instead of an ODBC channel the
#' name of the database must be given. This function takes care that the
#' timestamps are transferred correctly between MS Access and R by requesting
#' date and time information separately from MS Access and putting both together
#' to a POSIXct object in UTC timezone. This is necessary because with very long
#' data sets the RODBC function sqlQuery (or the function
#' \code{\link{hsSqlQuery}} that calls this function) may deliver timestamps in
#' which time information is lacking!
#' 
#' @param mdb Full path to MS Access database file (*.mdb)
#' @param tbl Name of table containing the time-series data.
#' @param tsField Name of table field containing the timestamps.
#' @param fields Vector containing names of value fields to be selected from the
#'   table. This vector may or may not contain the name of the timetamp field.
#' @param minDate Minimum date (and time) of time interval to be selected in
#'   ISO-Syntax: yyyy-mm-dd [HH:MM:SS], where the part in brackets in optional.
#' @param maxDate Day following the maximum date of the time interval to be
#'   selected, in ISO-Syntax: yyyy-mm-dd [HH:MM:SS], where the part in brackets
#'   in optional.
#' @param resolution time resolution: \dQuote{min} = minutes, \dQuote{s} =
#'   seconds. If time resolution is \dQuote{min} timestamps are rounded to the
#'   next full minute.
#' @param inclLast If TRUE, \emph{maxDate} will be included in result data set,
#'   else excluded.
#' @param sqlFilter additional SQL filter criterion
#' @param calcType for internal use only, do not change!
#' @param dbg if TRUE, debug messages are shown
#'   
#' @return data.frame with POSIXct timestamp column <strTimestamp> (UTC time
#'   zone) and value columns as selected in <strFieldList>
#'   
#' @seealso \code{\link{hsGetTimeSeries}, \link{hsGetTable}}
#' @importFrom kwb.utils printIf
#' @importFrom utils head
#' @export
#' @examples
#' \dontrun{
#' ## Get flow time series of 24 of August 2011 from tbl_Hyd in example database
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   ts <- hsMdbTimeSeries(
#'     xmdb(), "tbl_Hyd", "Zeitst", c("Q", "v"), "2011-08-24", "2011-08-25"
#'   )
#'   
#'   ## Show the last records of the returned dataset.
#'   tail(ts)
#' }
#'    
#' ## Output:
#' #                   Zeitst     Q     v
#' # 1435 2011-08-24 23:55:00 0.638 0.281
#' # 1436 2011-08-24 23:56:00 0.601 0.265
#' # 1437 2011-08-24 23:57:00 0.564 0.249
#' # 1438 2011-08-24 23:58:00 0.536 0.237
#' # 1439 2011-08-24 23:59:00 0.504 0.223
#' # 1440 2011-08-25 00:00:00 0.483 0.214
#' }
#' 
hsMdbTimeSeries <- function(
  mdb, tbl, tsField = hsTsField(mdb, tbl), fields = "*", minDate = NULL, 
  maxDate = NULL, resolution = "min", inclLast = TRUE, sqlFilter = "TRUE",
  dbg = FALSE, calcType = 1
)
{
  ## Return if resolution is neither "min" nor "s"
  if (! resolution %in% c("min", "s")) clean_stop(
    "Time resolution must be \"min\" (minutes) or \"s\" (seconds).\n"
  )

  # Get timeseries from database with three time-related extra columns:
  # 1. date only (column id = 2)
  # 2. date as number of days since 1899-12-30 (column id = 3)
  # 3. seconds since midnight (column id = 4) or
  #    minutes since midnight (column id = 5)
  tcol <- ifelse(resolution == "s", 4, 5)
  
  res <- hsGetTimeSeries(
    mdb, tbl, tsField, fields, minDate, maxDate,
    c(2, 3, tcol), inclLast = inclLast, sqlFilter = sqlFilter, dbg = dbg
  )
  
  kwb.utils::printIf(dbg, utils::head(res))
  
  ## Calculation of timestamp in two steps:
  ## 1. conversion of date to POSIXct object
  ## 2. addition of seconds representing time within the day to POSIXct object
  ## For the first step two calculation methods are considered, both of which
  ## should give the same result!
  
  if (calcType == 1) {
    
    ## Calculation type 1:
    ## Generate the POSIXct object from the date column by using hsToPosix:
    res[[tsField]] <- kwb.datetime::hsToPosix(res[, 1])
    
  } else {
    
    ## Calculation type 2:
    ## Generate the POSIXct object from the number of days since 1899-12-30
    ## (this is the way Access treats dates). We choose "UTC" time zone
    ## to prevent R from transferring date time info to CET and from
    ## considering daylight savings time!
    res[[tsField]] <- as.POSIXct(as.Date(res[, 2], origin = "1899-12-30"), tz = "UTC")
  }
  
  ## Add time information to the POSICct object
  res[[tsField]] <- res[[tsField]] + res[, 3] * ifelse(resolution == "s", 1, 60)
  kwb.utils::printIf(dbg, utils::head(res))

  # Return the data.frame without columns 1 and 2: "daysSince_1899_12_30"
  # and "intTime_min"
  #  colInds <- 1
  #  if (ncol(res) > 4) colInds <- c(colInds, 5:ncol(res))
  colInds <- 4:ncol(res)
  
  res[colInds]
}

# hsDropTable ------------------------------------------------------------------

#' Drop Database Table(s)
#' 
#' Removes the table \eqn{tbl} (if permitted). This function performs opening of
#' the connection, dropping of the table and closing of the connection.  If an
#' error occurs the program stops and an error description is shown.
#' 
#' @param mdb full path to MS Access database (*.mdb).
#' @param tbl table name.
#' @param isPtrn if TRUE, \emph{tbl} is interpreted as a regular expression
#'   matching the names of the tables to be deleted.
#' @param dbg if TRUE, debug messages are shown
#'
#' @seealso \code{\link{hsClearTable}}
#' @importFrom kwb.utils catIf
#' @importFrom RODBC sqlDrop
#' @importFrom odbc32 sqlDrop

#' @export
#'  
hsDropTable <- function(mdb, tbl, isPtrn = FALSE, dbg = TRUE)
{
  existingTables <- hsTables(mdb, namesOnly = TRUE)

  tbls <- if (isPtrn) {
    grep(tbl, existingTables, value = TRUE)
  } else {
    tbl
  }

  ## Open database connection and close it on exit
  con <- hsOpenMdb(mdb)
  on.exit(hsCloseMdb(con))

  for (tbl in tbls) {

    if (tbl %in% existingTables) {
      
      kwb.utils::catAndRun(dbg = dbg, sprintf("Dropping table '%s'", tbl), {

        (get_odbc_function("sqlDrop"))(con, tbl)
      })

    } else {
      
      cat(sprintf(
        "Table '%s' does not exist in database. No need to drop.\n", tbl
      ))
    }
  }
}

# hsClearTable -----------------------------------------------------------------

#' Clear a Database Table
#' 
#' Deletes all the rows of the table \eqn{tbl}. This function performs opening
#' of the connection, clearing of the table and closing of the connection. If
#' an error occurs the program stops and an error description is shown.
#' 
#' @param mdb full path to MS Access database (*.mdb).
#' @param tbl table name.
#' @param cond optional: condition.
#' @param \dots additional arguments passed to hsSqlQuery, e.g. "errors=TRUE"
#' 
#' @seealso \code{\link{hsDropTable}}
#' @export
#' 
hsClearTable <- function(mdb, tbl, cond = TRUE, ...)
{
  # Run SQL DELETE query
  sql <- sprintf("DELETE FROM %s WHERE %s", tbl, cond)
  hsSqlQuery(mdb, sql, ...)
}

# hsSqlQuery -------------------------------------------------------------------

#' Send SQL Query to Database
#' 
#' Get data from database requested via an SQL query. This function performs
#' opening of the connection, data retieval via SQL and closing of the
#' connection.  If an error occurs the program stops and an error description is
#' shown.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param sql SQL query
#' @param use2007Driver if TRUE the functions odbcConnectAccess2007 and
#'   odbcConnectExcel2007 are used instead of odbcConnectAccess and
#'   odbcConnectExcel, respectively
#' @param dbg if TRUE (default), debug messages are shown.
#' @param stopOnError if TRUE (default), the program stops in case of an error,
#'   otherwise a warning is shown and NULL is returned.
#' @param DBMSencoding finally passed to \code{odbcDriverConnect}. Default: "",
#'   You may want to use: "UTF-8"
#' @param \dots additional arguments to be passed to \code{sqlQuery}
#'   
#' @return On success, a data.frame containing the data that is internally
#'   requested by calling the RODBC function sqlQuery and that is provided by
#'   the database is returned.  On error R stops execution and does not return
#'   anything.
#'   
#' @seealso \code{\link{hsPutTable}, \link{hsGetTable}}
#' @importFrom kwb.utils catIf
#' @export
#' @examples
#' \dontrun{
#' ## Get Q time series from table "tbl_Hyd" in example database
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   tsQ <- hsSqlQuery(
#'     xmdb(), "SELECT Zeitst AS t, Q FROM tbl_Hyd WHERE Q > 1.0"
#'   )
#'    
#'   ## Show the first lines of the resulting data.frame
#'   head(tsQ)
#' }
#'    
#' ## Output
#' # t     Q
#' # 1 2011-08-24 22:27:00 1.061
#' # 2 2011-08-24 22:28:00 1.091
#' # 3 2011-08-24 22:29:00 1.115
#' # 4 2011-08-24 22:30:00 1.092
#' # 5 2011-08-24 22:31:00 1.086
#' # 6 2011-08-24 22:32:00 1.074
#' }
#' 
hsSqlQuery <- function(
  mdb, sql, use2007Driver = NULL, dbg = TRUE, stopOnError = TRUE, 
  DBMSencoding = "", ...
)
{
  # Store current sql dialect
  sqlDialect <- getCurrentSqlDialect(warn = FALSE)
  
  ## Open database connection, on exit close it and reset the sql dialect
  con <- hsOpenDb(
    mdb, use2007Driver = use2007Driver, DBMSencoding = DBMSencoding)
  
  on.exit({
    hsCloseDb(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  ## Send SQL query
  kwb.utils::catIf(dbg, sprintf("\nRunning SQL: %s\n\n", sql))
  
  res <- (get_odbc_function("sqlQuery"))(con, sql, ...)
  
  # Did an error occur?
  if ((class(res) == "character") && (length(res) > 0)) {
    
    msg <- paste(res, collapse = "\n")
    
    if (stopOnError) {
      
      clean_stop(msg)
      
    } else {
      
      warning(msg)
      return (NULL)
    }
  }
  
  # Did we get data?
  kwb.utils::catIf(dbg, sprintf(
    "The query returned %d records with %d fields: %s\n",
    nrow(res), ncol(res), paste(names(res), collapse = ",")
  ))
  
  ## Return result data.frame and suppress character results
  if (class(res) == "data.frame") res
}

# connectionStringAccess -------------------------------------------------------

#' Connection String Access
#' 
#' @param mdb full path to MS Access file
#' @param uid user id, if any
#' @param pwd password, if any
#' @param globalPartialBulkOps A Long value (read/write) that determines the
#'   behavior of the Jet database engine when SQL DML bulk operations fail. When
#'   set to allow partial completion of bulk operations, inconsistent changes
#'   can occur, because operations on some records could succeed and others
#'   could fail. When set to allow no partial completion of bulk operations, all
#'   changes are rolled back if a single error occurs. The Jet OLEDB:Global
#'   Partial Bulk Ops property setting can be overridden for the current
#'   Recordset object by setting the Jet OLEDB:Partial Bulk Ops property.The Jet
#'   OLEDB:Global Partial Bulk Ops and Jet OLEDB:Partial Bulk Ops properties can
#'   be set to any of the following values: Default = 0, Partial = 1, No Partial
#'   = 2
#'   
#' @references \url{http://msdn.microsoft.com/en-us/library/office/aa140022\%28v=office.10\%29.aspx}
#' @importFrom kwb.utils windowsPath
#' 
connectionStringAccess <- function(
  mdb, uid = "", pwd = "", globalPartialBulkOps = 0
)
{
  paste0(
    "Driver={Microsoft Access Driver (*.mdb)}", ";",
    "Dbq=", kwb.utils::windowsPath(mdb), ";",
    "Uid=", uid, ";",
    "Pwd=", pwd, ";",
    "Jet OLEDB:Global Partial Bulk Ops=", globalPartialBulkOps, ";"
  )
}

# xmdb -------------------------------------------------------------------------

#' Path to example database
#' 
#' Returns full path to MS Access example database
#' 
#' @export
#' 
xmdb <- function()
{
  system.file("extdata", "RExKwbBase.mdb", package = "kwb.db")
}

# getSqlDialect ----------------------------------------------------------------

#' Get SQL Dialect from Given Database
#' 
#' get SQL dialect ("mysql" or "msaccess") from given database
#' 
#' @param db ODBC database name or full path to database (mdb or xls)
#' @param use2007Driver passed to \code{\link{isMySQL}}
#' 
getSqlDialect <- function(db, use2007Driver = NULL)
{
  is.MySql <- isMySQL(db, use2007Driver = use2007Driver)
  
  ifelse(is.MySql, "mysql", "msaccess")
}

