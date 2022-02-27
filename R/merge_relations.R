# merge_relations --------------------------------------------------------------

#' Merge Table Schema with Relationship Information
#' 
#' @param schemata list as returned by \code{\link{read_schemata}}
#' @param relations data frame as returned by \code{\link{read_relations}}
#' 
merge_relations <- function(schemata, relations)
{
  table_relations <- lapply(
    
    split(relations, kwb.utils::selectColumns(relations, "table")), 
    
    function(x) {
      x <- kwb.utils::removeColumns(x, "table")
      x <- kwb.utils::moveColumnsToFront(x, c("field"))
      kwb.utils::resetRowNames(x)
    }
  )
  
  for (name in names(schemata)) {
    schemata[[name]]$foreign <- table_relations[[name]]
  }
  
  schemata
}
