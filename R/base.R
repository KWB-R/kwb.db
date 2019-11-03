# hsPutTable -------------------------------------------------------------------

#' Save Data Frame to Database Table
#' 
#' Writes data to a database table. This function performs opening of the
#' connection, saving of the data to a table and closing of the connection.  If
#' an error occurs the program stops and an error description is shown. If a
#' table named \emph{tbl} already exists in the database \emph{mdb} the existing
#' table is only overwritten if \emph{overwrite} is TRUE. Otherwise a
#' \code{\link[kwb.utils]{hsSafeName}} will be found for the table.
#' 
#' @param mdb full path to MS Access database file (*.mdb)
#' @param myData data.frame containing data to be written to database table
#' @param tbl Name of table to be created in the database
#' @param types field types to be passed to sqlSave as argument \emph{varTypes},
#'   see ?sqlSave for details.
#' @param overwrite shall existing table be overwritten?
#' @param DBMSencoding finally passed to \code{odbcDriverConnect}. Default: "",
#'   You may want to use: "UTF-8"
#' @param dbg if TRUE, debug messages are shown
#' 
#' @return In case of success the name of the created table is returned.
#' 
#' @seealso \code{\link{hsSqlQuery}, \link{hsGetTable}}
#' @importFrom kwb.utils hsSafeName
#' @importFrom RODBC sqlSave
#' @export
#' @examples
#' \dontrun{
#' ## Create a data.frame
#' df1 <- data.frame(id = 1:4, rnd = sample(1:100)[1:4])
#'   
#' ## Write data.frame into a table in the example database; as no
#' ## table name is specified, a table name is generated. The name
#' ## of the table is returned.
#' ## (only on Windows!)
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   tbl <- hsPutTable(xmdb(), df1)
#'   tbl # table name here: [1] "tblTmp"
#' 
#'    
#'   ## Get the data from the created table back again and print the
#'   ## data. As we see, a table containing four different random
#'   ## numbers between one and 100 has been created.
#'   df2 <- hsGetTable(xmdb(), tbl)
#'   df2
#' }
#'    
#' ## Output:
#' #   id rnd
#' # 1  1  82
#' # 2  2  14
#' # 3  3  99
#' # 4  4   6
#' }
#' 
hsPutTable <- function(
  mdb, myData, tbl = "tblTmp", types = NULL, overwrite = FALSE, 
  DBMSencoding = "", dbg = TRUE
)
{
  ## Find a safe (non-existing) table name
  tblSafe <- kwb.utils::hsSafeName(tbl, hsTables(mdb, namesOnly=TRUE))
  
  ## if tblSafe differs from tbl the table exists. Delete the
  ## existing table and use nevertheless tbl as table name if
  ## overwrite is TRUE
  if (tblSafe != tbl && isTRUE(overwrite)) {
    hsDropTable(mdb, tbl, dbg = dbg)
    tblSafe <- tbl
  }
  
  sqlDialect <- getCurrentSqlDialect(warn = FALSE)
  
  ## Open database connection, on exit close it and reset the sql dialect
  con <- hsOpenDb(mdb, DBMSencoding = DBMSencoding)
  
  on.exit({
    hsCloseDb(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  if (is.null(types)) {
    
    kwb.utils::catIf(dbg, "No field types given.\n")

    if (! isMySQL(mdb, con = con)) {
      
      for (colname in colnames(myData)) {
        
        className <- class(myData[[colname]])[1]
        dbClassName <- .hsJetType(className)
        types <- c(types, dbClassName)
      }
      
      names(types) <- colnames(myData)
    }
  }
  
  kwb.utils::printIf(
    dbg && ! is.null(types), types, "Given/generated field types"
  )
  
  # Provide arguments to sqlSave()
  arguments <- list(con, myData, tblSafe, rownames = FALSE, verbose = dbg)
  
  if (! is.null(types)) {
    arguments <- c(arguments, list(varTypes = types))
  }

  # Call the function that saves the data to the database table.
  # Select the appropriate function by means of get_odbc_function()
  result <- kwb.utils::catAndRun(
    dbg = dbg, 
    messageText = sprintf("Writing data to table '%s' in '%s'\n", tblSafe, mdb),
    expr = do.call(get_odbc_function("sqlSave"), arguments)
  )

  # Did an error occur?
  if (result != 1) {
    stop("sqlSave returned with error.\n")
  }
  
  tblSafe
}

# hsGetTable -------------------------------------------------------------------

#' Get Table from MS Access Database
#' 
#' Provides data from an MS Access database table in forms of a data frame.
#' 
#' ATTENTION: This function may not return what you want if the table contains a
#' timestamp field. Use \code{hsMdbTimeSeries} instead.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param tbl Table name. Put it into brackets [] if it contains spaces and if
#'   it mdb does not point to a MySQL database
#' @param cond Condition string.
#' @param fields Comma separated list of names of fields to be selected.
#' @param dbg if TRUE, debug messages are shown, else not
#' @param check if TRUE (default), \emph{tbl} is checked for existence in
#'   \emph{mdb} before trying to get the data and a list of available tables is
#'   shown in the case that the table does not exist.
#' @param use2007Driver passed to \code{\link{hsTables}} and 
#'   \code{\link{hsSqlQuery}}
#' @param \dots Additional arguments to be passed to hsSqlQuery
#'   
#' @return data.frame containing data from table in database
#' 
#' @seealso \code{\link{hsSqlQuery}, \link{hsPutTable}, hsGetTimeSeries,
#'   hsMdbTimeSeries}
#' @importFrom kwb.utils hsQuoteChr
#' @export
#' @examples
#' \dontrun{ 
#' ## Get all datasets from tbl_Hyd in example database where
#' ## Q > 1.0 m3/s and temperature > 20 degree Celsius
#' ## (only on Windows!)
#' 
#' if (.Platform$OS.type == "windows") {
#'   ts <- hsGetTable(xmdb(), "tbl_Hyd", "Q > 1.0 AND T_Kanal > 20")
#'   head(ts)
#' } 
#'  
#' ## Output:
#' # Zeitst     Q     v     H T_Kanal
#' # 1 2011-08-24 22:33:00 1.075 0.459 1.366    20.1
#' # 2 2011-08-24 22:34:00 1.062 0.453 1.370    20.2
#' # 3 2011-08-24 22:35:00 1.050 0.449 1.364    20.2
#' # 4 2011-08-24 22:36:00 1.042 0.446 1.361    20.3
#' # 5 2011-08-24 22:37:00 1.032 0.443 1.354    20.3
#' # 6 2011-08-24 22:38:00 1.010 0.436 1.348    20.4
#'   
#' ## TAKE CARE when getting time-series data:
#' if (.Platform$OS.type == "windows") {
#'   hsGetTable(xmdb(), "tblTimestampTest_DST")
#' }
#'    
#' ## Output:
#' #                tstamp
#' # 1 2011-03-27 01:00:00
#' # 2 2011-03-27 01:30:00
#' # 3                <NA>
#' # 4                <NA>
#' # 5 2011-03-27 03:00:00
#' # 6 2011-03-27 03:30:00
#'   
#' ## As the output shows the timestamps between 02:00:00 and
#' ## 02:59:59 have been set to <NA>. Reason: When retrieving
#' ## date/time data from MS Access, R converts the timestamps
#' ## from a text representation into POSIXct objects. As POSIXct's
#' ## standard time zone seems to be taken from the Windows system
#' ## R tries to convert to Central European Time (CET) which
#' ## does not exist for the hour in which time is switched to
#' ## daylight-saving time (as in the example).
#'   
#' ## This standard behaviour can be changed by setting the
#' ## standard time zone:
#' tz <- Sys.getenv("tz") # save current standard time zone
#' Sys.setenv(tz = "UTC") # set standard time zone to UTC
#'   
#' ## The same command as above now delivers all timestamps
#' ## (in Coordinated Universal Time, UTC):
#' 
#' if (.Platform$OS.type == "windows") {
#'   hsGetTable(xmdb(), "tblTimestampTest_DST")
#' }
#'    
#' ## Output:
#' #                tstamp
#' # 1 2011-03-27 01:00:00
#' # 2 2011-03-27 01:30:00
#' # 3 2011-03-27 02:00:00
#' # 4 2011-03-27 02:30:00
#' # 5 2011-03-27 03:00:00
#' # 6 2011-03-27 03:30:00
#' 
#' ## Reset standard time zone
#' Sys.setenv(tz = tz)
#' }
#' 
hsGetTable <- function(
  mdb, tbl, cond = "TRUE", fields = "*", dbg = TRUE, check = TRUE, 
  use2007Driver = NULL, ...
)
{
  if (missing(tbl) || check) {
    
    availableTableNames <- hsTables(
      mdb, namesOnly = TRUE, use2007Driver = use2007Driver
    )
  }
  
  # Help the user by showing a list of available table names
  if (missing(tbl)) {
    
    stop("No table name given. ",.message_availableTables(availableTableNames))
  }
  
  if (check) {
    
    # remove possible quotes embracing the table name
    availableTableNames <- gsub("^'(.*)'$", "\\1", availableTableNames)
    
    if (! (tbl %in% availableTableNames)) {
      
      stop(
        "Table ", kwb.utils::hsQuoteChr(tbl), " does not exist in ", 
        kwb.utils::hsQuoteChr(mdb), ".\n", 
        .message_availableTables(availableTableNames)
      )
    }
  }

  # Return result of SQL query
  is.MySql <- isMySQL(mdb, use2007Driver = use2007Driver)
  
  sql <- sqlForSelect(
    tablename = tbl, fields = fields, whereClause = cond,
    sqlDialect = ifelse(is.MySql, "mysql", "msaccess")
  )
  
  hsSqlQuery(mdb, sql = sql, dbg = dbg, use2007Driver = use2007Driver, ...)
}

# .message_availableTables -----------------------------------------------------

.message_availableTables <- function(tableNames)
{
  paste("Available tables:\n ", paste(tableNames, collapse = "\n  "))
}

# hsTables ---------------------------------------------------------------------

#' Available tables in database
#' 
#' Returns a data.frame as returned by sqlTables, containing information on the 
#' tables contained in the database.  Opening of the database connection, 
#' getting the list of tables and closing of the database connection is done 
#' within this function.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param excludeSystemTables if TRUE (default), system tables are excluded from
#'   the table list, else included.
#' @param namesOnly if TRUE, only table names are returned. Default: TRUE
#' @param use2007Driver if TRUE the functions odbcConnectAccess2007 and
#'   odbcConnectExcel2007 are used instead of odbcConnectAccess and
#'   odbcConnectExcel, respectively
#' @param dbg if TRUE, debug messages are shown
#'   
#' @return data.frame with columns \emph{TABLE_CAT}, \emph{TABLE_SCHEM}, 
#'   \emph{TABLE_NAME}, \emph{TABLE_TYPE}, \emph{REMARKS}, see sqlTables of 
#'   RODBC package.
#'   
#' @seealso \code{\link{hsFields}}
#' @importFrom kwb.utils catIf
#' @importFrom RODBC sqlTables 
#' @export
#' @examples
#' \dontrun{
#' ## Get names of tables in the example database
#' ## (only on Windows)
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   tnames <- hsTables(xmdb(), namesOnly = TRUE)
#'   
#'   ## Exclude system tables by filtering for table names
#'   ## not starting with '^MSys'
#'   tNonSys <- grep("^MSys", tnames, invert = TRUE, value = TRUE)
#'   
#'   ## Print the names of the non-system tables.
#'   cat(paste(tNonSys, "\n"))
#' }
#'    
#' ## Ouput:
#' # tbl_Hyd
#' #  tbl_Qua
#' #  ...
#' }
#' 
hsTables <- function(
  mdb, excludeSystemTables = grepl("\\.(mdb|accdb)$", mdb), namesOnly = TRUE,
  use2007Driver = NULL, dbg = FALSE
)
{
  kwb.utils::catIf(dbg, "in hsTables: use2007Driver =", use2007Driver, "\n")
  
  sqlDialect = getCurrentSqlDialect(warn = FALSE)
  
  ## Open database connection and close it on exit
  con <- hsOpenDb(mdb, use2007Driver = use2007Driver)
  
  on.exit({
    hsCloseDb(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  tblList <- (get_odbc_function("sqlTables"))(con)

  if (excludeSystemTables) {
    tblList <- tblList[tblList$TABLE_TYPE != "SYSTEM TABLE", ]
  }

  if (namesOnly) {
    tblList$TABLE_NAME
  } else {
    tblList
  }
}

# hsFields ---------------------------------------------------------------------

#' Available Fields in Database Table
#' 
#' Returns a vector containing the field names of a database table.
#' 
#' @param mdb full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}).
#' @param tbl table name.
#' @param namesOnly if TRUE, only field names are returned, otherwise all
#'   available information on the fields. Default: TRUE
#' @param chopDollar if TRUE (default), a dollar sign at the end of the table
#'   name is removed before sending it to \code{sqlColumns},
#' @param ignore.case if TRUE, case is ignored when comparing the given table
#'   with the names of the existing tables. Default: FALSE
#' @param use2007Driver passed to \code{\link{isMySQL}}
#' @param dbg if TRUE, debug messages are shown
#'   
#' @return Vector containing the field names of the database table (if namesOnly
#'   = TRUE) or data.frame with columns \emph{COLUMN_NAME}, \emph{DATA_TYPE},
#'   \emph{TYPE_NAME}, \emph{COLUMN_SIZE}, \emph{BUFFER_LENGTH},
#'   \emph{DECIMAL_DIGITS}, \emph{NUM_PREC_RADIX}, \emph{NULLABLE} describing
#'   the database fields in detail, otherwise.
#' 
#' @seealso \code{\link{hsTables}}
#' @importFrom RODBC sqlColumns
#' @export
#' @examples
#' \dontrun{ 
#' ## List the fields of table "tbl_Hyd" in the example database
#' ## (only on Windows!)
#' 
#' if (.Platform$OS.type == "windows") {
#'   setCurrentSqlDialect("msaccess")
#'   fields <- hsFields(xmdb(), "tbl_Hyd")
#'   fields
#' }
#'   
#' ## Ouput:
#' # [1] "Zeitst"  "Q"       "v"       "H"       "T_Kanal"
#' }
#' 
hsFields <- function(
  mdb, tbl, namesOnly = TRUE, chopDollar = TRUE, 
  ignore.case = (! isMySQL(mdb, use2007Driver = use2007Driver)), 
  use2007Driver = NULL, dbg = FALSE
)
{
  ## if no table name is given stop with showing a list of available tables
  tbls <- hsTables(mdb, namesOnly = TRUE)
  
  msg <- sprintf("Available tables:\n  %s", paste(
    '"', tbls, '"', sep = "", collapse = '\n  '
  ))
  
  if (missing(tbl)) {
    
    clean_stop("No table name given. ", msg)
  }
  
  pattern <- paste("^", tbl, "$", sep = "")
  
  if (length(grep(pattern, tbls, ignore.case = ignore.case)) != 1) {
    
    clean_stop(sprintf('Table "%s" not found in %s. ', tbl, mdb), msg)
  }
  
  sqlDialect <- getCurrentSqlDialect(warn = FALSE)
  
  con <- hsOpenDb(mdb)
  
  on.exit({
    hsCloseDb(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  if (chopDollar) {
    
    tbl <- sub("\\$$", "", tbl)
  }
  
  fieldInfo <- if (is64BitR()) {
    clean_stop("Sorry. There is no equivalent to sqlColumns() in odbc32!")
  } else {
    RODBC::sqlColumns(con, tbl)
  }
  
  if (namesOnly) {
    
    return(fieldInfo$COLUMN_NAME)
    
  } else {
    
    ## omit first three (table related) columns of data frame
    return(fieldInfo[, -c(1:3)])
  }
}