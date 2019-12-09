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
  result <- kwb.utils::catAndRun(
    dbg = dbg, 
    messageText = sprintf("Writing data to table '%s' in '%s'\n", tblSafe, mdb),
    expr = {
      result <- try(do.call(RODBC::sqlSave, arguments))
      if (inherits(result, "try-error")) {
        result <- do.call(odbc32::sqlSave, arguments)
      }
    }
  )
  
  # Did an error occur?
  if (result != 1) {
    clean_stop("sqlSave returned with error.\n")
  }
  
  tblSafe
}
