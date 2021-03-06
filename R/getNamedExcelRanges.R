# getNamedExcelRanges ----------------------------------------------------------

#' Get Named Ranges from Excel File
#' 
#' @param file path to Excel file
#' @param pattern pattern matching the names of named cell ranges to be read.
#'   By default, all ranges starting with \code{range} are read.
#' @param dbg logical. If \code{TRUE} debug messages are shown
#' @param stringsAsFactors passed to \code{\link{hsGetTable}}
#' @param \dots further arguments passed to \code{\link{hsGetTable}}
#' 
#' @return list of data frames each of which represents the content a named cell 
#'   range in the Excel \code{file} and each of which was read with 
#'   \code{\link{hsGetTable}}.
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
#' @export
#'   
getNamedExcelRanges <- function(
  file, pattern = "^range", dbg = TRUE, stringsAsFactors = FALSE, ...
)
{
  table_names <- hsTables(file)
  
  range_names <- table_names[grepl(pattern, table_names)]
  
  ranges <- lapply(range_names, function(range_name) {
    kwb.utils::catAndRun(
      sprintf("Getting range '%s'", range_name),
      hsGetTable(
        mdb = file, tbl = range_name, stringsAsFactors = stringsAsFactors, 
        dbg = FALSE, ...
      )
    )
  })
  
  stats::setNames(ranges, range_names)
}
