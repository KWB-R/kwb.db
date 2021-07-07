# getDatabaseFieldInfo ---------------------------------------------------------

#' Get Information on Table Field Names and Types
#' 
#' @param db path to MS Access or MS Excel file or name of ODBC data source
#' @return data frame with columns \code{TABLE_NAME}, \code{COLUMN_NAME}, 
#'   \code{TYPE_NAME}, \code{DECIMAL_DIGITS}
#' @export
#' @importFrom kwb.utils createAccessor moveColumnsToFront removeColumns
#' @importFrom kwb.utils selectColumns
#' 
getDatabaseFieldInfo <- function(db)
{
  table_info <- hsTables(db, namesOnly = FALSE)
  getcol <- kwb.utils::createAccessor(table_info)
  tables <- getcol("TABLE_NAME")[getcol("TABLE_TYPE") == "TABLE"]

  ordinal <- "ORDINAL"
  infos <- lapply(stats::setNames(nm = tables), function(tbl) {
    info <- hsFields(db, tbl, namesOnly = FALSE)
    columns <- c("COLUMN_NAME", "TYPE_NAME", "DECIMAL_DIGITS", ordinal)
    info <- kwb.utils::selectColumns(info, columns)
    kwb.utils::removeColumns(info[order(info[[ordinal]]), ], ordinal)
  })

  column <- "TABLE_NAME"
  kwb.utils::moveColumnsToFront(kwb.utils::rbindAll(infos, column), column)
}
