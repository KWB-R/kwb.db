# hsOpenMdb --------------------------------------------------------------------

#' Open Connection to MS Access Database
#' 
#' Deprecated. Use \code{\link{hsOpenDb}} instead.
#' 
#' @param mdb full path to MS Access database file.
#' @param dbg if TRUE and if the connection could be established details of the
#'   connection are shown.
#' @importFrom kwb.utils warningDeprecated
#' 
hsOpenMdb <- function(mdb, dbg = FALSE)
{
  kwb.utils::warningDeprecated("kwb.db::hsOpenMdb", "kwb.db::hsOpenDb")
  hsOpenDb(mdb, dbg)
}

# hsOpenDb ---------------------------------------------------------------------

#' Open Connection to MS Access or Excel
#' 
#' Opens database connection to MS Access or MS Excel, checks the connection and
#' stops on error.
#' 
#' @param src full path to MS Access database file (extension \dQuote{.mdb} or 
#'   \dQuote{.accdb}) or MS Excel file (extension \dQuote{.xls} or 
#'   \dQuote{.xlsx}) or name of ODBC data source.
#' @param use2007Driver if TRUE the functions odbcConnectAccess2007 and
#'   odbcConnectExcel2007 are used instead of odbcConnectAccess and
#'   odbcConnectExcel, respectively
#' @param dbg if TRUE and if the connection could be established details of the 
#'   connection are shown.
#' @param DBMSencoding finally passed to \code{odbcDriverConnect}. Default: "",
#'   You may want to use: "UTF-8"
#' @param \dots additional arguments passed to the odbcConnect...()-function
#'   
#' @return On success an object of class RODBC describing the connection is
#'   returned. On failure nothing is returned and program execution stops.
#'   
#' @seealso \code{\link{hsCloseDb}}
#' @importFrom kwb.utils catIf printIf
#' @examples
#' \dontrun{ 
#' ## Open a connection to the example database
#' ## (only on Windows!)
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   con <- hsOpenDb(xmdb())
#'   con
#' }
#'    
#' ## Details on the established connection are shown:
#' # RODBC Connection 9
#' # Details:
#' #   case=nochange
#' #   DBQ=C:\Users\hsonne\Documents\R\win-library\2.14\kwb.base\...
#' #   Driver={Microsoft Access Driver (*.mdb)}
#' #   DriverId=25
#' #   FIL=MS Access
#' #   MaxBufferSize=2048
#' #   PageTimeout=5
#' #   UID=admin
#'   
#' ## Close the connection again
#' 
#' if (.Platform$OS.type == "windows") {
#' 
#'   hsCloseDb(con)
#' }
#' }
#'    
hsOpenDb <- function(
  src, use2007Driver = NULL, dbg = FALSE, DBMSencoding = "", ...
)
{
  kwb.utils::catIf(dbg, "in hsOpenDb: use2007Driver =", use2007Driver, "\n")
  
  if (missing(src) || length(src) == 0) {
    
    stop("No source file (*.mdb, *.accdb, *.xls or *.xlsx) ",
         "or name of ODBC data source given.")
  }
  
  if (mode(src) != "character" || length(src) > 1) {
    
    stop("src must be a character vector of length one.")
  }
  
  ## Open database connection
  con <- openAdequateConnectionOrStop(
    src, use2007Driver = use2007Driver, DBMSencoding = DBMSencoding, ...
  )
  
  ## Return if connection failed
  if (! is64BitR() && con == -1) {
    
    stop("Could not connect to database: ", src)
  }
  
  ## in debug mode, print connection
  kwb.utils::printIf(dbg, con, "Connection")
  
  is_mysql <- if (! is64BitR()) {
    attr(con, "isMySQL")
  } else {
    message("Do not know how to check if this is a MySQL connection")
  }
  
  setCurrentSqlDialect(ifelse(is_mysql, "mysql", "msaccess"))
  
  con
}

# openAdequateConnectionOrStop -------------------------------------------------

#' Open Adequate Connection or Stop
#' 
#' @param db database name or file
#' @param use2007Driver if TRUE the functions odbcConnectAccess2007 and
#'   odbcConnectExcel2007 are used instead of odbcConnectAccess and
#'   odbcConnectExcel, respectively
#' @param dbg if \code{TRUE}, debug messages are shown
#' @param DBMSencoding finally passed to \code{odbcDriverConnect}. Default: "",
#'   You may want to use: "UTF-8"
#' @param \dots further arguments passed to \code{odbcConnectionAccess}, 
#'   \code{odbcConnectionExcel} or \code{\link[RODBC]{odbcConnect}}
#' @importFrom kwb.utils catIf
#' @importFrom RODBC odbcConnect odbcDataSources
#' 
openAdequateConnectionOrStop <- function(
  db, use2007Driver = NULL, dbg = FALSE, DBMSencoding = "", ...
)
{
  kwb.utils::catIf(
    dbg, "in openAdequateConnectionOrStop: use2007Driver =", use2007Driver, "\n"
  )
  
  is_mdb <- isAccessFile(db)
  is_xls <- isExcelFile(db)
  
  if ((is_mdb || is_xls) &&! file.exists(db)) {
    
    stop("No such file: '", db, "'! Please check the path!", call. = FALSE)
  }
  
  if (is_mdb) return(odbcConnectionAccess(
    db, use2007Driver = use2007Driver, DBMSencoding = DBMSencoding, ...
  ))
  
  if (is_xls) return(odbcConnectionExcel(
    db, use2007Driver = use2007Driver, DBMSencoding = DBMSencoding, ...
  ))
  
  if (isOdbcDataSource(db)) return(RODBC::odbcConnect(
    db, DBMSencoding = DBMSencoding, ...
  ))
  
  stop(
    "src must be a file name with extension .mdb, .accdb, .xls, .xlsx ",
    "or the name of an existing ODBC data source. ",
    "Available ODBC data sources are:\n * ",
    paste(names(RODBC::odbcDataSources()), collapse = ",\n * ")
  )
}

# odbcConnectionAccess ---------------------------------------------------------

#' @importFrom RODBC odbcConnectAccess2007 odbcConnectAccess
#' @importFrom odbc32 odbcConnectAccess2007
#' 
odbcConnectionAccess <- function(db, use2007Driver = NULL, ...)
{
  if (is.null(use2007Driver)) {
    
    use2007Driver <- isAccess2007File(db)
  }
  
  # Select the appropriate package
  pkg <- ifelse(is64BitR(), "odbc32", "RODBC")
  
  # Select the appropriate function name
  name <- ifelse(use2007Driver, "odbcConnectAccess2007", "odbcConnectAccess")

  if (is64BitR()) {

    if (! use2007Driver) stop(
      "Cannot connect to ", db, " with 64 Bit-Version of R!", call. = FALSE
    )

    socket <- .GlobalEnv$.r2r_socket
    
    if (is.null(socket)) {
      socket <- odbc32::start_server()
    }
    
    odbc32::odbcConnectAccess2007(db, socket = socket)
    
  } else {
    
    if (use2007Driver){
      RODBC::odbcConnectAccess2007(db, ...)
    } else {
      RODBC::odbcConnectAccess(db, ...)
    }
  }
}

# odbcConnectionExcel ----------------------------------------------------------

#' @importFrom RODBC odbcConnectExcel2007 odbcConnectExcel
#' 
odbcConnectionExcel <- function(db, use2007Driver = NULL, ...)
{
  if (is.null(use2007Driver)) {
    
    use2007Driver <- isExcel2007File(db)
  }
  
  if (use2007Driver) {
    
    RODBC::odbcConnectExcel2007(db, ...)
    
  } else {
    
    RODBC::odbcConnectExcel(db, ...)
  }
}

# hsCloseMdb -------------------------------------------------------------------

#' Close Connection
#' 
#' Deprecated. Use code{\link{hsCloseDb}} instead.
#' 
#' @param con Open database connection as returned by
#'   \link{hsOpenMdb}/odbcConnect
#' 
hsCloseMdb <- function(con)
{
  hsCloseDb(con)
}

# hsCloseDb --------------------------------------------------------------------

#' Close Connection to MS Access or Excel
#' 
#' Closes the database connection.
#' 
#' @param con Open database connection as returned by
#'   \link{hsOpenDb}/\code{odbcConnect}
#' 
#' @seealso \code{\link{hsOpenDb}}
#' @importFrom RODBC odbcClose
#' @importFrom odbc32 odbcClose
#' 
hsCloseDb <- function(con)
{
  ## Close database connection
  if (is64BitR()) {
    
    odbc32::odbcClose(con)
    
    socket <- .GlobalEnv$.r2r_socket
    
    if (! is.null(socket)) {
      odbc32::stop_server(socket = socket)  
    }
    
  } else {
    
    RODBC::odbcClose(con)  
  }

  #options("kwb.db.current.sql.dialect" = NULL)
}
