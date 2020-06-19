# hsDumpMdb --------------------------------------------------------------------

#' Export Database Tables to CSV Files
#' 
#' Deprecated. Please use \code{\link{dumpDatabase}} instead.
#' 
#' @param mdb full path to database
#' @param ptrn pattern matching names of tables to be exported. Default: "^tbl",
#'   i.e. tables starting with "tbl"
#' @param tdir target directory. By default a new directory is created in the
#'   same directory as mdb resides in. The new directory has the same name as
#'   the database file with dots substituted with underscores
#' @param create_target_dir if \code{TRUE}, the target directory \code{tdir}
#'   is created if it does not exist.
#' @importFrom kwb.utils warningDeprecated
#' 
hsDumpMdb <- function(
  mdb, ptrn = "^tbl", 
  tdir = file.path(dirname(mdb), gsub("\\.", "_", basename(mdb))),
  create_target_dir = FALSE
)
{
  kwb.utils::warningDeprecated("hsDumpMdb", "dumpDatabase")
  
  dumpDatabase(
    db = mdb, pattern = ptrn, target_dir = tdir, 
    create_target_dir = create_target_dir
  )
}

# dumpDatabase -----------------------------------------------------------------

#' Export Database Tables to CSV Files
#' 
#' Exports all tables of a database of which the names match a given pattern to
#' csv files.
#' 
#' @param db full path to database or name of ODBC data source
#' @param pattern pattern matching names of tables to be exported. Default:
#'   "^tbl", i.e. tables starting with "tbl"
#' @param target_dir target directory. By default a new directory is created in
#'   the same directory as mdb resides in. The new directory has the same name
#'   as the database file with dots substituted with underscores
#' @param create_target_dir if \code{TRUE}, the target directory \code{tdir} is
#'   created if it does not exist.
#' @param sep passed to \code{\link[utils]{write.table}}
#' @param dec passed to \code{\link[utils]{write.table}}
#' @param qmethod passed to \code{\link[utils]{write.table}}
#' @param row.names passed to \code{\link[utils]{write.table}}
#' @param \dots further arguments passed to \code{\link[utils]{write.table}}
#' @importFrom kwb.utils createDirectory safePath
#' @importFrom utils write.table
#' @export
#' 
dumpDatabase <- function(
  db, pattern = "^tbl", target_dir = NULL, create_target_dir = FALSE,
  sep = ",", dec = ".", qmethod = "double", row.names = FALSE, ...
)
{
  if (is.null(target_dir)) {
    
    target_dir <- file.path(
      dirname(db), gsub("\\.", "_", basename(db))
    )
  }
  
  # Create target directory if it does not exist or check its existence
  if (create_target_dir) {
    
    kwb.utils::createDirectory(target_dir)
    
  } else {
    
    kwb.utils::safePath(target_dir)
  }
  
  tables <- grep(pattern, hsTables(db, namesOnly = TRUE), value = TRUE)
  
  for (table in tables) {
    
    table_data <- hsGetTable(db, table)
    
    file <- file.path(target_dir, paste0(table, ".csv"))
    
    utils::write.table(
      table_data, file = file, sep = sep, dec = dec, qmethod = qmethod, 
      row.names = row.names, ...
    )
  }
}
