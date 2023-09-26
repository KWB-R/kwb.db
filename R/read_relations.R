# read_relations ---------------------------------------------------------------

#' Read Relations from MSysRelationships Table CSV Export
#' 
#' @param file path to "MSysRelationships.csv" as exported from MS Access by
#'   right clicking on the (hidden) MSysRelationships table and selecting
#'   "Export->Text file->OK"
#' @return data frame with columns \code{table}, \code{field}, \code{refTable}, 
#'   \code{refField}
#'    
read_relations <- function(file)
{
  x <- utils::read.table(file, sep = ";", stringsAsFactors = FALSE)
  
  names(x) <- c(
    "ccolumn",
    "grbit",
    "icolumn",
    "szColumn",
    "szObject",
    "szReferencedColumn",
    "szReferencedObject",
    "szRelationship"
  )
  
  x <- kwb.utils::renameAndSelect(x, list(
    szObject = "table",
    szColumn = "field",
    szReferencedObject = "refTable",
    szReferencedColumn = "refField"
  ))
  
  x[! grepl("^MSys", x$table), , drop = FALSE]
}
